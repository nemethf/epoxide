;;; Table-view.el --- EPOXIDE Table view node definition file

;; Copyright (C) 2014-2015 István Pelle

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A node for displaying data received on its inputs in a table form.  Table
;; header is automatically determined from the data.  It provides options to
;; reorganize columns and enable/disable them.
;; The node takes undetermined number of inputs that should be outputs of other
;; nodes.  Data is expected to have <key>=<value> format.  Header fields
;; come from keys.
;; Node takes an indefinite number of configuration arguments.  These will
;; become the initial header fields.  Every name written here will be dispalyed
;; initially.  Later modification of these aruments will not have any effect
;; on the current table instance.  For modifying the actual header the
;; `epoxide-table-view-header' variable should be edited.
;; The node does not use any of its outputs.
;; Data is displayed in the node buffer, table header is displayed in the
;; window header.

;;; Code:

(require 'org-table)
(require 'epoxide)

(eval-when-compile
  (defvar epoxide-input-markers)
  (defvar epoxide-node-inputs)
  (defvar epoxide-table-view-header)
  (defvar epoxide-table-view-header-line)
  (defvar epoxide-table-view-default-header-format)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-buffer))

(defvar epoxide-table-view-paused nil
  "When non-nil table is not refreshed.")

(defvar epoxide-table-view-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map epoxide-mode-map)
    (define-key map "v" 'epoxide-table-view-list-variables)
    (define-key map "c" 'epoxide-table-view-customize-variable)
    (define-key map (kbd "C-c C-p") 'epoxide-table-view-pause)
    map)
  "Keymap for Table view nodes.")

(defvar epoxide-table-view-header-edit-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-x C-s") 'epoxide-table-view-set-header)
    (define-key map (kbd "C-x C-a") 'epoxide-table-view-set-header)
    map)
  "Keymap for editing table view header fields.")

(defvar epoxide-table-view-header-editable-field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map (kbd "C-x C-s") 'epoxide-table-view-set-header)
    (define-key map (kbd "C-x C-a") 'epoxide-table-view-set-header)
    (define-key map (kbd "C-x h") 'epoxide-table-view-header-field-help)
    map)
  "Keymap used inside an epoxide editable field.")

(define-widget 'epoxide-table-view-header-editable-field 'editable-field
  "Modified editable field to support additional key combinations."
  :keymap epoxide-table-view-header-editable-field-keymap
  :action 'epoxide-table-view-header-field-help)

(defun epoxide-table-view-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "input")
     (value-helper . nil)
     (value-validator . nil))))

(defun epoxide-table-view-config-info ()
  "Provide documentation, value tips and validation for config fields."
  nil)

(defun epoxide-table-view-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "not used")
     (auto-create . nil))))

(defun epoxide-table-view-init ()
  "Initialize buffer-local variables."
  (set (make-local-variable 'epoxide-input-markers) nil)
  (dolist (input (reverse epoxide-node-inputs))
    (setq-local epoxide-input-markers
		(cons `((marker-pos . 1) (buffer . ,input))
		      epoxide-input-markers)))
  (set (make-local-variable 'epoxide-table-view-header) nil)
  (set (make-local-variable 'epoxide-table-view-header-line) "")
  (set (make-local-variable 'epoxide-table-view-default-header-format)
       header-line-format)
  (dolist (item epoxide-node-config-list)
    (epoxide-table-view-add-header-item item))
  (setq-local header-line-format
	      '(:eval (epoxide-table-view-update-header)))
  (use-local-map epoxide-table-view-map))

(defun epoxide-table-view-all-header-items ()
  "Return all header item names."
  (mapcar 'car epoxide-table-view-header))

(defun epoxide-table-view-displayed-header-items ()
  "Return only those header item names that get displayed."
  (delq nil (mapcar (lambda (item)
		      (when (cdr item)
			(car item)))
		    epoxide-table-view-header)))

(defun epoxide-table-view-add-header-item (name)
  "Add a new header item with NAME and enable it."
  (setq-local epoxide-table-view-header (append epoxide-table-view-header
					  (list `(,name . t)))))

(defun epoxide-table-view-set-header-item (name value)
  "Set enabled/disabled part of header item with NAME to VALUE."
  (let ((header (copy-sequence epoxide-table-view-header))
	new-header)
    (while header
      (if (equal name (caar header))
	  (setq new-header (cons `(,name . ,value) new-header))
	(setq new-header (cons (car header) new-header)))
      (setq header (cdr header)))
    (setq-local epoxide-table-view-header (nreverse new-header))))

(defun epoxide-table-view-reorder-header (new-order)
  "Reorder header items based on NEW-ORDER.
NEW-ORDER is a list of header item names."
  (let ((header (copy-sequence epoxide-table-view-header))
	(new-header '(nil))
	new-item)
    (dolist (item new-order)
      (setq new-item (assoc item header))
      (when new-item
	(setq new-header (cons new-item new-header))
	(setq header (delete new-item header))))
    (setq new-header (nreverse (delq nil new-header)))
    (setq-local epoxide-table-view-header (append new-header header))))

(defun epoxide-table-view-exec ()
  "Read the inputs and display them in a table."
  (let ((window (get-buffer-window))
	new-markers input name table-line table-lines to-be-displayed tmp-line)
    ;; Take care of input changes.
    (unless (equal (sort (copy-sequence epoxide-node-inputs) #'string-lessp)
    		   (sort (copy-sequence (mapcar (lambda (x)
    						  (cdr (assoc 'buffer x)))
    						epoxide-input-markers))
			 #'string-lessp))
      (let ((buffers-with-marker (copy-sequence (mapcar (lambda (x)
    						  (cdr (assoc 'buffer x)))
    						epoxide-input-markers)))
    	    tmp-input-markers)
    	(dolist (input epoxide-node-inputs)
    	  (if (member input buffers-with-marker)
    	      (dolist (input-marker epoxide-input-markers)
    		(when (equal input (cdr (assoc 'buffer epoxide-input-markers)))
    		  (setq tmp-input-markers (cons input-marker
    						tmp-input-markers))))
    	    (with-current-buffer input
    	      (setq tmp-input-markers (cons `((marker-pos . ,(point-max))
    					      (buffer . ,(buffer-name)))
					    tmp-input-markers)))))
    	(setq-local epoxide-input-markers (nreverse tmp-input-markers))))
    ;; Create an input string from all the available inputs.
    (dolist (marker (reverse epoxide-input-markers))
      (with-current-buffer (cdr (assoc 'buffer marker))
    	(when (and window (null epoxide-table-view-paused))
    	  (when (< (cdr (assoc 'marker-pos marker)) (point-max))
    	    (setq input (concat (buffer-substring-no-properties
    				 (cdr (assoc 'marker-pos marker)) (point-max))
    				input))))
    	(setq new-markers (cons `((marker-pos . ,(point-max))
				  (buffer . ,(buffer-name)))
				new-markers))))
    (setq-local epoxide-input-markers (nreverse new-markers))
    ;; Process this input.
    (when (and window input (null epoxide-table-view-paused))
      (dolist (line (split-string input "\n"))
	(setq table-line nil)
	(dolist (parameter (split-string line ","))
	  (setq parameter (split-string parameter "="))
	  (setq table-line
                (cons `(,(epoxide-chomp (car parameter)) . ,(cdr parameter))
                      table-line))
	  (setq name (epoxide-chomp (car parameter)))
	  (unless (member name (cons "" (epoxide-table-view-all-header-items)))
	    (epoxide-table-view-add-header-item name)))
	(setq table-lines (cons (nreverse table-line) table-lines)))
      (setq table-lines (nreverse table-lines))
      ;; Add header to data to be displayed.
      (setq to-be-displayed
	    (concat (mapconcat 'identity
			       (epoxide-table-view-displayed-header-items)
			       " | ") "\n"))
      ;; Add table data matching header cells to data to be displayed.
      (dolist (line table-lines)
	(setq tmp-line nil)
	(dolist (item (epoxide-table-view-displayed-header-items))
	  (let ((r (cadr (assoc item line))))
	    (setq tmp-line (cons (if (null r)
				     ""
				   r)
				 tmp-line))))
	(unless (equal
		 tmp-line
		 (make-list (length (epoxide-table-view-displayed-header-items))
			    ""))
	  (setq to-be-displayed
		(concat to-be-displayed
			(mapconcat 'identity (nreverse tmp-line) ",")
			"\n"))))
      ;; Insert new data and create a table from it using org-table.
      ;; When possible , keep point and window start where it was before the
      ;; update.
      (let ((pos (point))
	    (win-start (window-start window)))
	;; Temporarily switch off header display.
	(setq-local header-line-format epoxide-table-view-default-header-format)
	(erase-buffer)
	(setq truncate-lines 0)
	;; Insert and format new table.
	(insert to-be-displayed)
	(org-table-convert-region (point-min) (point-max) '(4))
	(goto-char (point-min))
	;; Remove and store header in a buffer-local variable.
	(setq-local epoxide-table-view-header-line
		    (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position)))
	(delete-region (line-beginning-position) (line-end-position))
	(delete-char 1)
	(set-window-start window win-start)
	(goto-char pos)
	;; Turn on header display.
	(setq-local header-line-format
	      '(:eval (epoxide-table-view-update-header)))))))

(defun epoxide-table-view-pause ()
  "Set flag for enabling/disabling of table refresh."
  (interactive)
  (if epoxide-table-view-paused
      (setq epoxide-table-view-paused nil)
    (setq epoxide-table-view-paused t)))

(defun epoxide-table-view-customize-variable ()
  "Using ido list epoxide related buffer local variables.

When one is selected show either an editable or non-editable form
for it depending on what was selected."
  (interactive)
  (let* ((node-buffer (current-buffer))
	 (node-name (when (local-variable-p 'epoxide-node-name)
		      epoxide-node-name))
	 (node-class (when (local-variable-p 'epoxide-node-class)
		       epoxide-node-class))
	 (root-buffer (when (local-variable-p 'epoxide-root-buffer)
			epoxide-root-buffer))
	 (variables (epoxide-tsg-select-epoxide-variables
		     (buffer-local-variables)))
	 (var (assoc (intern (ido-completing-read
			      "Choose variable: "
			      (cons "all"
				    (mapcar (lambda (x)
					      (symbol-name (car x)))
					    variables))))
		     variables)))
    (cond
     ((null var)
      ;; List all epoxide buffer local variables.
      (epoxide-variables-list))
     ((string-match "config-list\\|inputs" (symbol-name (car var)))
      ;; Show config or input parameters in an editable form.
      (epoxide-variable-edit var node-name node-class root-buffer))
     ((string-match "epoxide-table-view-header" (symbol-name (car var)))
      (epoxide-table-view-edit-header node-buffer))
     ((string-match "outputs" (symbol-name (car var)))
      (epoxide-edit-output-variable node-name node-class root-buffer))
     (t
      ;; Show other parameters in an uneditable form.
      (epoxide-variable-show var node-name node-class root-buffer)))))

(defun epoxide-table-view-list-variables ()
  "Display a list of available EPOXIDE buffer-local variables."
  (interactive)
  (let ((node-buffer (current-buffer)))
    (epoxide-variables-list)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "epoxide-table-view-header" nil t)
	(make-button (line-beginning-position) (line-end-position)
		     'action (lambda (x)
			       (epoxide-table-view-edit-header
				(button-get x 'node-buffer)))
		     'node-buffer node-buffer
		     'follow-link t)))))

(defun epoxide-table-view-edit-header (node-buffer)
  "Provide a user interface to set header cell of the table.
NODE-BUFFER is the node's buffer."
  (epoxide-kill-buffer "*set value*")
  (switch-to-buffer (get-buffer-create "*set value*"))
  (let ((inhibit-read-only t)
	(node-name (with-current-buffer node-buffer
		     epoxide-node-name))
	(node-class (with-current-buffer node-buffer
		      epoxide-node-class))
	(root-buffer (with-current-buffer node-buffer
		       epoxide-root-buffer))
	(header (with-current-buffer node-buffer
		  epoxide-table-view-header))
	(displayed-header (with-current-buffer node-buffer
			    (epoxide-table-view-all-header-items)))
	parameter-order
	values)
    (erase-buffer)
    (remove-overlays)
    (widget-insert "Set value of variable 'epoxide-table-view-header' "
		   "of node '" node-name ":" node-class
		   "'\nthat originates from " (if (bufferp root-buffer)
						  (buffer-name root-buffer)
						root-buffer)
		   ":\n\n")
    (widget-create 'epoxide-table-view-header-editable-field
		   :format "Parameter order: %v "
		   (mapconcat 'identity displayed-header ", "))
    (widget-insert "\n")
    (dolist (h header)
      (setq values (cons (widget-create 'checkbox (cdr h)) values))
      (widget-insert " " (car h) "\n"))
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (epoxide-table-view-set-header))
		   "Apply")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-buffer))
		   "Cancel")
    ;; Save information for later (when setting the value of header).
    (set (make-local-variable 'epoxide-node-buffer) node-buffer)
    (widget-setup)
    (use-local-map epoxide-table-view-header-edit-map)
    (goto-char (point-min))
    (widget-forward 3)))

(defun epoxide-table-view-set-header ()
  "Set values of the table accoring to user defined settings."
  (interactive)
  (save-excursion
    (let ((node-buffer epoxide-node-buffer)
	  parameter-order name value end-pos)
      (goto-char (point-max))
      (forward-line -1)
      (setq end-pos (point))
      (goto-char (point-min))
      (widget-forward 1)
      (setq parameter-order (widget-value (widget-at (point))))
      (with-current-buffer node-buffer
	(epoxide-table-view-reorder-header (split-string parameter-order ", ")))
      (widget-forward 1)
      (while (< (point) end-pos)
	(setq value (widget-value (widget-at (point))))
	(forward-char 3)
	(setq name (epoxide-chomp
                    (buffer-substring-no-properties (1+ (point))
                                                    (line-end-position))))
	(with-current-buffer epoxide-node-buffer
	  (epoxide-table-view-set-header-item name value))
	(widget-forward 1))
      (epoxide-kill-buffer "*set value*")
      (with-current-buffer node-buffer
	(epoxide-table-view-list-variables)))))

(defun epoxide-table-view-update-header ()
  "Display table header in window header."
  (let* ((j (if (scroll-bar-columns 'left)
		 (1+ (scroll-bar-columns 'left))
	       0))
	 (o (window-hscroll))
	 (col (current-column))
	 (i 0)
	 (header (concat (make-string j ?\s) epoxide-table-view-header-line))
	 (text (concat header
		       (make-string (- (+ (line-end-position) 2)
				       (length header))
				    ?\s)))
	 (ruler (substring text o)))
    ;; Show the `current-column' marker.
    (setq i (+ j (- col o)))
    (when (and (>= i 0) (< i (length ruler))
	       (> (length epoxide-table-view-header-line) 0))
      (aset ruler i ?¤))
    ruler))

(defun epoxide-table-view-header-field-help (&rest ignore)
  "Show a help when changing the order of header fields.

If a value is chosen add that to the list.  Argument IGNORE is
ignored it is there only for compatibilty reasons."
  (let ((options (with-current-buffer epoxide-node-buffer
		   (epoxide-table-view-all-header-items)))
	value)
    (if (null options)
	(message "Currently there is no help for header fields")
      (setq value (ido-completing-read "Help: "
				       options)))
    (when value
      (cond
       ((equal (char-before) ?\s))
       ((equal (char-before) ?,)
	(insert " "))
       (t
	(insert ", ")))
      (insert value)
      (unless (equal (point) (line-end-position))
	(insert ",")
	(unless (equal (char-after) ?\s)
	  (insert " "))))))

(defun epoxide-table-view-stop ()
  "Dummy function."
  nil)

(provide 'table-view)

;;; Table-view.el ends here
