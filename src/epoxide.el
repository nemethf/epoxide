;;; epoxide.el --- Emacs POX IDE

;; Copyright (C) 2013-2015 András Gulyás
;; Copyright (C) 2013-2016 Felicián Németh
;; Copyright (C) 2014-2016 István Pelle
;; Copyright (C) 2015      Tamás Lévai

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

;; A Modular Troubleshooting Framework for SDN (and other) Networks

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'cl))
(require 'ibuffer)        ; For ibuffer interaction.
(require 'cogre)          ; For topology drawing.
(require 'cogre/mode)
(require 'timerfunctions) ; For repeatable idle-timers of the scheduler.
(require 'json)           ; For json processing.
(require 'tramp)          ; For remote shell
(require 'dabbrev)        ; For auto-complete support.

;;; Compatibility code

(unless (boundp 'setq-local)
  ;; From subr.el of Emacs 24.4.1
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'set (list 'make-local-variable (list 'quote var)) val)))

;; For Emacs versions in which `split-string' has only 3 arguments.
(when (< (length
	  (delete '&optional
		  (copy-sequence (help-function-arglist 'split-string)))) 4)
  (defadvice split-string (after nf (string &optional sep omit trim) activate)
    "If TRIM is non-nil, it should be a regular expression to match
text to trim from the beginning and end of each substring.  If trimming
makes the substring empty, it is treated as null."
    (when trim
      (setq ad-return-value
	    (mapcar (lambda (str)
		      (when (string-match trim str)
			(setq str (substring str (match-end 0))))
		      (when (string-match trim str)
			(setq str (substring str 0 (match-beginning 0))))
		      str)
		    ad-return-value)))))

;; For Emacs versions that do not have `special-form-p'.
(unless (fboundp 'special-form-p)
  (defun special-form-p (object)
    "Non-nil if and only if OBJECT is a special form.

Compatibility code for Emacs versions prior to contain
`special-form-p'. Use the list of special forms from the
documentation of the function for Emacs 24.5.1 for comparison."
    (when (member object
		  '(and catch cond codition-case defconst defvar
			function if interactive lambda let let* or prog1
			prog2 progn quote save-current-buffer save-excursion
			save-restriction setq setq-default track-mouse
			unwind-protect while))
      t)))

;;; Variables:

(defvar epoxide-root-buffer nil
  "Buffer associated with a troubleshooting graph.")
(defvar epoxide-src-node nil
  ;; There's only one previous node, but there can be many nodes
  ;; processing link's data.
  "Source node buffer of a link buffer.")
(defvar epoxide-node-name)
(defvar epoxide-node-class)
(defvar epoxide-node-inputs)
(defvar epoxide-node-config-list)
(defvar epoxide-node-dynamic-config-list)
(defvar epoxide-node-outputs)
(defvar epoxide-init-function nil)
(defvar epoxide-exec-function nil)
(defvar epoxide-stop-function nil)
(defvar epoxide-tsg-eldoc-documentations)
(defvar epoxide-tsg-node-name)
(defvar epoxide-tsg-prev-node-name)
(defvar epoxide-tsg-node-class)
(defvar epoxide-tsg-input-buffers)
(defvar epoxide-tsg-output-buffers)
(defvar epoxide-tsg-prev-output-buffers)
(defvar epoxide-tsg-config-list)
(defvar epoxide-tsg-node-list)
(defvar epoxide-dpids)
(defvar epoxide-switch-names)
(defvar epoxide-tsg-node-name-generated-index)
(defvar epoxide-view-list ()
  "List containing epoxide views.
Views are represented as a list containing `view's that has attributes:
NAME: name of the view
COLS: number of columns
ROWS: number of rows
BUF-LIST: list of buffers to show.")
(defvar epoxide-view-last-index)
(defvar epoxide-event-queue ()
  "List containing buffer names representing events.")
(defvar epoxide-event-timer nil
  "Idle Timer used to schedule tasks of epoxide nodes.")
(defvar epoxide-event--processing-flag nil)
(defvar epoxide-tsg-visualizer-file nil
  "Filename for easy COGRE loading.")
(defvar epoxide-variable)
(defvar epoxide-values)
(defvar epoxide-ibuffer-origin-buffer)
(defvar epoxide-nodes)
(defvar epoxide-tsg-visualizer-new-layout-requested-p)
(defvar ac-candidate-face)
(defvar epoxide-initial-window-config)
(defvar epoxide-input-markers)
(defvar ibuffer-filter-groups) ;; from ibuf-ext.el

(defcustom epoxide-tramp-verbose-level tramp-verbose
  "Controls TRAMP logging level when Epoxide runs.

In order to silence TRAMP connection messages set this less or
equal to 2 (but at least 0).  See the documentation of varibale
`tramp-verbose' for more details."
  :type 'number
  :group 'epoxide)


;; --------------    Parsing an .tsg file    -----------------------------------

;; TODO: this should be a path (list of directories) in order to give
;; users a chance to add thier own node definitions without touching
;; ~/.emacs.d/elpa.  That directory will be overwritten during the
;; upgrade of the epoxide package.
(defcustom epoxide-nodes-files-directory
  (concat (file-name-directory load-file-name) "nodes")
  "Location of node definition files."
  :type 'directory
  :group 'epoxide)

(defcustom epoxide-start-tsg-on-file-open 'ask-before-starting
  "Controls what happens when a .tsg file is opened.

`start-tsg' : start TSG when opening a .tsg file without any
question.

`ask-before-starting' : ask for confirmation before parsing the
.tsg file.

`open-file-without-parsing': open .tsg file but do not parse it (a
message will be displayed that tells how to start the TSG)."
  :type '(radio
	  (const start-tsg)
	  (const ask-before-starting)
	  (const open-file-without-parsing))
  :group 'epoxide)

(defcustom epoxide-default-view-name "view"
  "This view is used as default when no other views are created."
  :type 'string
  :group 'epoxide)

(defvar epoxide-tsg-root-buffer nil
  "Temporarily contains the buffer name of the corresponding .tsg file.")

;; Structure to handle nodes.
(cl-defstruct node name class config-list dynamic-config-list input-buffers
	      output-buffers)

;; Structure to handle views.
(cl-defstruct view name cols rows input-buffers)


;; -----------------------------------------------------------------------------

(defvar epoxide-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" 'epoxide-variables-list)
    (define-key map "c" 'epoxide-variable-customize)
    (define-key map (kbd "C-x C-b") 'epoxide-ibuffer)
    (define-key map (kbd "C-x p") 'epoxide-jump-to-preceding-node)
    (define-key map (kbd "C-x n") 'epoxide-jump-to-next-node)
    (define-key map (kbd "C-x g") 'epoxide-tsg-visualizer-show)
    (define-key map (kbd "C-c C-a") 'epoxide-add-new-node)
    (define-key map (kbd "M-n") 'epoxide-forward-buffer)
    (define-key map (kbd "M-p") 'epoxide-backward-buffer)
    (define-key map (kbd "C-c C-o") 'epoxide-show-all-outputs)
    (define-key map (kbd "M-g t") 'epoxide-tsg-visualizer-show)
    (define-key map (kbd "M-g e") 'epoxide-switch-to-root-buffer)
    (define-key map (kbd "M-g v") 'epoxide-view-show)
    (define-key map (kbd "M-P") 'epoxide-view-show-prev)
    (define-key map (kbd "M-N") 'epoxide-view-show-next)
    (define-key map (kbd "C-c R") 'epoxide-restart-node)
    (define-key map (kbd "C-c C-k") 'epoxide-tsg-kill-all)
    (define-key map (kbd "M-g V") 'epoxide-view-show-views)
    map)
  "Keymap for `epoxide-mode'.")


;;; General utility functions --------------------------------------------------

(defun epoxide-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
				    (: (* (any " \t\n")) eos)))
			    ""
			    str))

(defun epoxide-kill-buffer (buffer-or-name &optional silent)
  "Kill BUFFER-OR-NAME if it is still available.
When optional argument SILENT is t, first set buffer-modified to
nil, then kill the buffer."
  (when buffer-or-name
    (let ((buffer (get-buffer buffer-or-name)))
      (when (buffer-live-p buffer)
	(when silent
	  (with-current-buffer buffer
	    (set-buffer-modified-p nil)))
	(kill-buffer buffer)))))

(defun epoxide-stop-process (process)
  "Stop PROCESS.

Call `stop-process' on PROCESS when if it is live.  If PROCESS did not stop
call `kill-process'.  Check maximum max-count times if process is killed
if it was not killed the user will be presented a question whether or not to
kill the process."
  (let ((wait 5))
    (when (and process
	       (process-live-p process))
      (condition-case nil
	  (progn
	    (stop-process process)
	    (while (and (> wait 0)
			(member (process-status process) '(exit stop signal)))
		(sleep-for 0.01)
		(decf wait))
            (unless (member (process-status process) '(exit stop signal))
              (interrupt-process process t)
              (sleep-for 0.01))
            (unless (member (process-status process) '(exit stop signal))
              (delete-process process)))
	(error nil)))))

(defun epoxide-setl (list nth new-value)
  "Return a modified LIST, in which the NTH item is set to NEW-VALUE.
If NTH is greater than the length of LIST, fill the space in-between with nils."
  (if (< (1- (length list)) nth)
      (let ((i (length list))
	    temp-list)
	(while (< i nth)
	  (setq temp-list (cons nil temp-list))
	  (setq i (1+ i)))
	(append list (nreverse (cons new-value temp-list))))
    (let (temp-list)
      (dotimes (i nth)
	(setq temp-list (cons (pop list) temp-list)))
      (pop list)
      (append (nreverse (cons new-value temp-list)) list))))

(defmacro epoxide-list-setq (var nth new-value)
  "Modify VAR by setting its NTH item to NEW-VALUE.
If NTH is greater than the length of VAR, fill the space
in-between with nils."
  `(setq ,var (epoxide-setl ,var ,nth ,new-value)))

(defun epoxide-reverse-string (s)
  "Return the reverse of string S."
  (apply 'string (reverse (string-to-list s))))

(defmacro epoxide-enumerate (spec &rest body)
  ;; checkdoc-params: (spec)
  "Loop over a list while counting each iteratation.
Evaluate BODY with VAR bound to each car from LIST, in turn,
while incrementing NUM starting from 0.  Then evaluate RESULT to
get return value, default nil.

\(fn (NUM VAR LIST [RESULT]) BODY...)"
  (declare (indent 1)
           (debug ((symbolp symbolp form &optional form) &rest form)))
  `(let ((,(car spec) 0))
     (dolist (,(nth 1 spec) ,(nth 2 spec) ,(nth 3 spec))
      ,@body
      (incf ,(car spec)))))

(defun epoxide-display-buffers (buffers &optional delete-other-windows)
  "Display BUFFERS.

BUFFERS is a list of buffers that are to be displayed.  Display
each of them by creating an automatic layout that splits the
window by taking into consideration the current frame and window
sizes.  When optional second argument DELETE-OTHER-WINDOWS is
non-nil all other windows are closed when displaying BUFFERS."
  (when delete-other-windows
    (delete-other-windows))
  (when buffers
    (switch-to-buffer (car buffers))
    (setq buffers (cdr buffers)))
  (let ((scale-factor (/ (frame-char-height) (frame-char-width))))
    (dolist (buffer buffers)
      (let* ((largest-window (get-largest-window))
	     (largest-height (window-height largest-window))
	     (largest-width (window-width largest-window))
	     ;; Split window vertically when its relative height is
	     ;; smaller than its relative width.
	     (vertical-split (if (< (* scale-factor largest-height)
				    largest-width)
				 t
			       nil))
	     (window (condition-case nil
			 (split-window (get-largest-window) nil vertical-split)
		       (error nil))))
	(when (null window)
	  (epoxide-log
	   "Some buffers are not displayed due to too small frame size.")
	  (return))
	(set-window-buffer window buffer)))
    (balance-windows)))

(defun epoxide-add-to-list (n item list)
  "Add an item to a list.

N specifies the 1 based index where ITEM should be placed in
LIST.  If the Nth position is occupied, shift the cdr of LIST.  If
N is greater than the length of LIST, fill in the gap with nils."
  (when (> n 0)
    (if (> n (length list))
	(epoxide-setl list (1- n) item)
      (setq n (1- n))
      (let ((head (butlast (copy-sequence list) (- (length list) n)))
	    (tail (nthcdr n (copy-sequence list))))
	(append (nreverse (cons item (nreverse head))) tail)))))


(define-derived-mode epoxide-mode fundamental-mode
  "Key definitions:
\\{epoxide-mode-map}"
  :group 'epoxide
  (let ((vars '(epoxide-node-name epoxide-node-class epoxide-node-inputs
	        epoxide-node-config-list epoxide-node-outputs
                epoxide-root-buffer epoxide-init-function
                epoxide-exec-function epoxide-stop-function)))
    (dolist (var vars)
      (set (make-local-variable var) nil))
    (setq-local kill-buffer-hook (cons 'epoxide-tsg-kill kill-buffer-hook))
    (setq mode-name "Epoxide")))

(define-derived-mode epoxide-link-mode epoxide-mode
  "Key definitions:
\\{epoxide-mode-map}"
  :group 'epoxide
  ;; TODO: switch off ElDoc-mode
  (set (make-local-variable 'epoxide-src-node) nil)
  (setq mode-name "Epoxide link"))

(defvar epoxide-parameter-edit-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-x C-s") 'epoxide-parameter-edit-mode-save)
    (define-key map (kbd "C-x C-a") 'epoxide-parameter-edit-mode-apply)
    map)
  "Keymap for editing epoxide parameter values.")

(defvar epoxide-editable-field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map (kbd "C-x C-s") 'epoxide-parameter-edit-mode-save)
    (define-key map (kbd "C-x C-a") 'epoxide-parameter-edit-mode-apply)
    (define-key map (kbd "C-x h") 'epoxide-parameter-help)
    map)
  "Keymap used inside an epoxide editable field.")

(define-widget 'epoxide-editable-field 'editable-field
  "Modified editable field to support additional key combinations."
  :keymap epoxide-editable-field-keymap
  :action 'epoxide-parameter-help-action)

(defvar epoxide-tsg-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?> "w" st)
    (modify-syntax-entry ?: "w" st)
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry 32 "-" st)
    (modify-syntax-entry ?\t "-" st)
    (modify-syntax-entry ?\r "-" st)
    (modify-syntax-entry ?\n "-" st)
    (modify-syntax-entry ?/ "w 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used while in `epoxide-tsg-mode'.")

(defvar epoxide-tsg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-b") 'epoxide-ibuffer)
    (define-key map (kbd "C-x C-e") 'epoxide-tsg-clear-and-start)
    (define-key map (kbd "C-x g") 'epoxide-tsg-visualizer-show)
    (define-key map (kbd "C-c C-e") 'epoxide-tsg-start)
    (define-key map (kbd "C-c C-a") 'epoxide-add-new-node)
    (define-key map (kbd "M-g t") 'epoxide-tsg-visualizer-show)
    (define-key map (kbd "M-g v") 'epoxide-view-show)
    (define-key map (kbd "M-P") 'epoxide-view-show-prev)
    (define-key map (kbd "M-N") 'epoxide-view-show-next)
    (define-key map (kbd "C-c R") 'epoxide-restart-node)
    (define-key map (kbd "C-c C-k") 'epoxide-tsg-kill-all)
    (define-key map (kbd "M-g V") 'epoxide-view-show-views)
    (define-key map (kbd "C-c C-r") 'epoxide-recommender-recommend)
    map)
  "Keymap for `epoxide-tsg-mode'.")

(defvar epoxide-tsg-font-lock-keywords
  '((;; the syntax table takes care of font locking comments
     ;("//.*$" . font-lock-comment-face)
     ("\\([[:alnum:]-]+\\)\\s-*?\\(::\\|->\\|-->\\|\\[\\|;\\)"
      (1 font-lock-variable-name-face))
     ("\\([A-Z][[:alnum:]-]*?\\)\\s-*?(" (1 font-lock-type-face)))
    nil
    nil)) ; case sensitive

;;;###autoload
(define-derived-mode epoxide-tsg-mode fundamental-mode "TSG"
  "Mode for parsing .tsg files.
Key definitions:
\\{epoxide-tsg-mode-map}"
  :group 'epoxide
  (setq mode-name "TSG")
  (setq-local sentence-end ";")
  (let ((vars '(epoxide-tsg-node-name
		epoxide-tsg-prev-node-name epoxide-tsg-node-class
		epoxide-tsg-input-buffers epoxide-tsg-output-buffers
		epoxide-tsg-prev-output-buffers epoxide-tsg-config-list
		epoxide-tsg-node-list epoxide-dpids epoxide-switch-names
		epoxide-view-list epoxide-tsg-eldoc-documentations
		epoxide-initial-window-config))
        (dir (concat temporary-file-directory "epoxide.tsg/")))
    (make-directory dir t)
    (setq-local epoxide-tsg-visualizer-file (concat dir (buffer-name) ".cgr"))
    (dolist (var vars)
      (set (make-local-variable var) nil)))
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq font-lock-defaults epoxide-tsg-font-lock-keywords)
  (setq-local epoxide-tsg-node-name-generated-index 0)
  (setq-local epoxide-view-last-index 0)
  ;; Add node directory to emacs load path.
  (add-to-list 'load-path epoxide-nodes-files-directory)
  (setq-local eldoc-documentation-function 'epoxide-tsg-eldoc-function)
  (switch-to-buffer (current-buffer))
  (eldoc-mode)
  (setq-local kill-buffer-hook (cons 'epoxide-tsg-kill kill-buffer-hook))
  (add-to-list 'kill-buffer-query-functions 'epoxide-kill-buffer-query-function)
  (epoxide-ac-init)                     ; Initialize autocomplete.
  (epoxide-tsg-eldoc-init)              ; Initialize eldoc for epoxide-tsg-mode.
  (font-lock-fontify-buffer)
  (setq-local epoxide-initial-window-config (current-window-configuration))
  (epoxide-tsg-clear-and-start))

(defvar epoxide-topology-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "w" st)
    st)
  "Syntax table used while in `epoxide-topology-mode'.")

;;;###autoload
(define-derived-mode epoxide-topology-mode fundamental-mode "Topology"
  "Mode for displaying topology files."
  :group 'epoxide
  (setq mode-name "Topology"))


;; --------------    Standard parameter names    -------------------------------

;; Standard names for paramters appearing in flow statistics.
(defvar epoxide-l2-source "l2_source")
(defvar epoxide-l2-destination "l2_destination")
(defvar epoxide-l3-source "l3_source")
(defvar epoxide-l3-destination "l3_destination")
(defvar epoxide-l3-proto "l3_proto")
(defvar epoxide-l2-type "l2_type")
(defvar epoxide-vlan-id "vlan_ID")
(defvar epoxide-l2-type "l2_type")
(defvar epoxide-max-length "maximum_length")
(defvar epoxide-table-id "table_id")
(defvar epoxide-duration "duration_sec")
(defvar epoxide-byte-count "byte_count")
(defvar epoxide-packet-count "packet_count")


;; --------------    Interacting with Ibuffer    -------------------------------

(defvar epoxide-ibuffer-tsg-group '("tsg files" (mode . epoxide-tsg-mode))
  "Ibuffer rule to group buffers showing files with `epoxide-tsg-mode'.")

(defvar epoxide-ibuffer-emacs-group '("emacs" (or
					   (name . "^\\*scratch\\*$")
					   (name . "^\\*Messages\\*$")))
  "Ibuffer rule to group Emacs related buffers.")

(defvar epoxide-ibuffer-nodes-group '("other nodes" (mode . epoxide-mode))
  "Ibuffer rule to group `epoxide-mode' buffers.")

(defvar epoxide-ibuffer-links-group
  '("other link buffers" (mode . epoxide-link-mode))
  "Ibuffer rule to group `epoxide-link-mode' buffers.")


;; --------------    Handling jumping from one node's    -----------------------
;; ----------------------    buffer to the next --------------------------------

(defcustom epoxide-jump-to-buffer-type 'link
  "Buffer types to consider when moving between epoxide buffers.
It can be one of the following values.
`node' : jumping to an adjacent buffer will take to an adjacent
          node buffer.
`link' : besides nodes, link buffers are taken into consideration."
  :type '(radio
	  (const node)
	  (const link))
  :group 'epoxide)


;; --------------    Setting up eldoc for .tsg files    ------------------------

(defun epoxide-tsg-get-node-classes ()
  "Retrieve node definitions from their directory."
  (append `("View")
	  (delq nil
		(delete-dups
		 (mapcar 'epoxide-tsg-remove-extension-from-file-name
			 (directory-files epoxide-nodes-files-directory))))))

(defun epoxide-tsg-remove-extension-from-file-name (file-name)
  "Cut file extension from FILE-NAME.
The part after the first '.' character is stripped."
  (if (member file-name  '("." ".."))
      nil
    (if (or (string-match "#" file-name)
	    (string-match "~" file-name))
	(setq file-name nil)
      (setq file-name (substring file-name 0 (string-match "\\." file-name))))
    file-name))

(defun epoxide-tsg-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil.

While being in the config list, highlight the current config parameter."
  (let* ((word (if (version<= emacs-version "24.3.1")
                   (thing-at-point 'symbol)
                 (thing-at-point 'symbol t)))
	 (doc (cdr (assoc word epoxide-tsg-eldoc-documentations)))
	 (c-pos (point))
	 (pos (save-excursion
		(if (search-backward ";" nil t)
		    (1+ (point))
		  (point-min))))
	 (prec '("::" "->" "-->"))
	 expression class data num match)
    (if doc
	doc
      ;; If no documentation was found for the current word and we are
      ;; not looking at an empty line or an tsg syntax element.
      (unless (or (member word (append '(";") prec))
		  (equal "" (epoxide-chomp
			     (buffer-substring-no-properties
			      (line-beginning-position) (line-end-position)))))
	(save-excursion
	  ;; Find the beginning of this expression describing the node.
	  (dolist (p prec)
	    (goto-char c-pos)
	    (when (search-backward p pos t)
	      (forward-char (length p))
	      (setq pos (point))))
	  ;; If a commented line got read, remove it from the expression.
	  (setq expression
		(replace-regexp-in-string
		 "//.*[\n]+" "" (buffer-substring-no-properties pos c-pos)))
	  (setq expression
		(replace-regexp-in-string
		 "\n" " " expression))
	  (goto-char pos)
	  (epoxide-tsg-skip-whitespaces)
	  ;; When input links are presumed to be coming.
	  (when (looking-at "\\[")
	    (condition-case nil
		(progn
		  (forward-list)
		  ;; Process only when ending ']' is present and the position
		  ;; where the user left point preceeds this ending ']'.
		  (if (not (<= c-pos (point)))
		      (goto-char pos)
		    (setq match t)
		    (epoxide-tsg-skip-whitespaces)
		    (setq doc (epoxide-tsg-eldoc-show-input c-pos))))
	      (error nil)))
	  ;; When configuration paramters are presumed to be coming.
	  (unless match
	    (forward-word)
	    (epoxide-tsg-skip-whitespaces)
	    (when (looking-at "(")
	      (condition-case nil
		  (forward-list)
		(error (goto-char (line-end-position))))
	      (when (> (point) c-pos)
		(setq match t)
		(setq doc (epoxide-tsg-eldoc-show-config expression doc)))))
	  ;; When output links are presumed to be coming.
	  (unless match
	    (when (looking-at "\\[")
	      (condition-case nil
		  (forward-list)
		(error (goto-char (line-end-position))))
	      (when (> (point) c-pos)
		(goto-char c-pos)
		(setq doc (epoxide-tsg-eldoc-show-output expression doc)))))))
      doc)))

(defun epoxide-tsg-eldoc-show-input (actual-position)
  "Return text that can be displayed as input list hint.
ACTUAL-POSITION is the buffer position where the user left the cursor."
  (let ((class (current-word))
	doc data pos)
    (setq data (cdr (assoc class epoxide-tsg-eldoc-documentations)))
    ;; Take the input specific part of the documentation.
    (when (and data
	       (setq pos (string-match "\ninputs: " data)))
      (setq data (substring data (1+ pos)))
      (when (setq pos (string-match "\n" data))
	(setq data (substring data 0 pos)))
      (goto-char actual-position)
      (setq doc
	    (epoxide-tsg-eldoc-highlight-current-argument
	     (current-word) data class)))
    (unless doc
      (setq doc (concat class " has no documented inputs")))
    doc))

(defun epoxide-tsg-eldoc-show-config (expression doc)
  "Return text that can be displayed as config list hint.
EXPRESSION is the current expression that is being processed and DOC is the
eldoc documentation of the node."
  (let (class data pos)
    ;; Strip from the current expression that is not part of the node name
    ;; or class.
    (setq class (substring expression 0 (string-match "(" expression)))
    (when (setq pos (string-match "\\]" class))
      (setq class (substring class (1+ pos))))
    (when (setq pos (string-match "\\[" class))
      (setq class (substring class (1+ pos))))
    (setq class (epoxide-chomp class))
    (setq data (cdr (assoc class epoxide-tsg-eldoc-documentations)))
    ;; Take the config specific part of the documentation.
    (when (and data (string-match "\nconfig: " data))
      (setq data (substring data (1+ (string-match "\nconfig: " data))))
      (when (string-match "\n" data)
	(setq data (substring data 0 (string-match "\n" data))))
      (setq
       doc
       (epoxide-tsg-eldoc-highlight-current-argument
	(number-to-string
	 (1- (length (split-string expression ", ")))) data class 1)))
    (unless doc
      (setq doc (concat class " has no documented configuration parameters")))
    doc))

(defun epoxide-tsg-eldoc-show-output (expression doc)
  "Return text that can be displayed as output list hint.
EXPRESSION is the current expression that is being processed and DOC is the
eldoc documentation of the node."
  (let (doc data class pos)
    ;; Strip from the current expression that is not part of the node name
    ;; or class.
    (setq class (epoxide-chomp expression))
    (setq class (substring class 0 (string-match "\\[" class 1)))
    (when (setq pos (string-match "(" class))
      (setq class (substring class 0 pos)))
    (when (setq pos (string-match "\\]" class))
      (setq class (substring class (1+ pos))))
    (when (setq pos (string-match "\\[" class))
      (setq class (substring class (1+ pos))))
    (setq class (epoxide-chomp class))
    (setq data (cdr (assoc class epoxide-tsg-eldoc-documentations)))
    ;; Take the output specific part of the documentation.
    (when (and data (string-match "\noutputs: " data))
      (setq data (substring data (1+ (string-match "\noutputs: " data))))
      (when (string-match "\n" data)
	(setq data (substring data 0 (string-match "\n" data))))
      (setq doc
	    (epoxide-tsg-eldoc-highlight-current-argument (current-word)
						       data class)))
    (unless doc
      (setq doc (concat class " has no documented outputs")))
    doc))

(defun epoxide-tsg-eldoc-highlight-current-argument (num data name-or-class
							 &optional offset)
  "Return hint text where NUMth part of eldoc DATA is highlighted.
If the NUMth parameter is not documented no highlight is
given.  To make the hint more descriptive node information is also
included by using the node's NAME-OR-CLASS.

When optional fourth argument OFFSET is present add offset to num
and use that as an index to find the argument to be
highlighted (it is needed as configuration arguments are indexed
from 1 as opposed from 0)."
  (when offset
    (setq num (string-to-number num))
    (setq num (number-to-string
  	       (incf num offset))))
  (let ((beg "")
	(highlight "")
	(rest "")
	doc pos)
    (setq num (concat num ": "))
    (setq name-or-class (propertize name-or-class 'face 'bold))
    (if (not (string-match num data))
	(setq doc (concat name-or-class " " data))
      (setq beg (substring data 0 (string-match num data)))
      (setq highlight (substring data (string-match num data)))
      (when (setq pos (string-match "; " highlight))
	(setq rest (substring highlight pos))
	(setq highlight (substring highlight 0 pos)))
      (setq doc (concat name-or-class " " beg
			(propertize highlight 'face 'font-lock-doc-face)
			rest)))
    doc))

(defun epoxide-tsg-eldoc-init ()
  "Initialize eldoc documentation based on the node definitions.

From the source file of each node, documentation functions named as
epoxide-<node name>-{input, config, output}-doc are added to the eldoc
documentation based on the name of its class."
  (setq-local epoxide-tsg-eldoc-documentations nil)
  (dolist (class (epoxide-tsg-get-node-classes))
    (let* ((prefix (concat "epoxide-" (downcase class) "-"))
           (input (concat prefix "input-info"))
           (config (concat prefix "config-info"))
           (output (concat prefix "output-info")))
      ;; Autoload functions that supply input, config and output documentation.
      (dolist (str (list input config output))
	(unless (string-match "^View$" class)
	    (autoload (intern str) class)))
      ;; Add documentation to eldoc.
      (epoxide-tsg-eldoc-add-class class
       (epoxide-tsg-eldoc-doc-string input)
       (epoxide-tsg-eldoc-doc-string config)
       (epoxide-tsg-eldoc-doc-string output)))))

(defun epoxide-tsg-eldoc-doc-string (info-function-name)
  "Return doc-string parts as a list from a node's info function.

INFO-FUNCTION-NAME signifies the name of the node's info function to call."
  (epoxide-tsg-eldoc-info info-function-name 'doc-string))

(defun epoxide-tsg-eldoc-helper (info-function-name)
  "Return helper parts as a list from a node's info function.

The node's info function is specified by its name: INFO-FUNCTION-NAME."
  (epoxide-tsg-eldoc-info info-function-name 'value-helper))

(defun epoxide-tsg-eldoc-validator (info-function-name)
  "Return validator parts as a list from a node's info function.

INFO-FUNCTION-NAME describes the name of the node's info function to call."
  (epoxide-tsg-eldoc-info info-function-name 'value-validator))

(defun epoxide-tsg-eldoc-info (info-function-name type)
  "Return a list of info attributes of a node.

The node's info function named INFO-FUNCTION-NAME is called and from the result
alist all those values are returned that match TYPE."
  (mapcar (lambda (x)
	    (cdr (assoc type x)))
	  (funcall (intern info-function-name))))

(defun epoxide-tsg-eldoc-add-node (node-name node-class root-buffer)
  "Add a node to the eldoc documentation based on its name.

NODE-NAME: name of node.  NODE-CLASS: class of node.  ROOT-BUFFER: buffer
conaining the .tsg file.
Input, config  and output documentation are looked up based on NODE-CLASS,
documentation is associated with NODE-NAME."
  (with-current-buffer root-buffer
    (epoxide-tsg-eldoc-add-class
     node-class
     (epoxide-tsg-eldoc-get-doc-string node-class "input")
     (epoxide-tsg-eldoc-get-doc-string node-class "config")
     (epoxide-tsg-eldoc-get-doc-string node-class "output")
     node-name)))

(defun epoxide-tsg-eldoc-get-doc-string (node-class kind)
  "Return the documentation of NODE-CLASS' parameters having type KIND.
KIND can be like input, config, output."
  (let ((doc-func (concat "epoxide-" (downcase node-class) "-"))
	match)
    (cond
     ((string-match "config" kind)
      (setq doc-func (concat doc-func "config-info"))
      (setq match t))
     ((string-match "output" kind)
      (setq doc-func (concat doc-func "output-info"))
      (setq match t))
     ((string-match "input" kind)
      (setq doc-func (concat doc-func "input-info"))
      (setq match t)))
    (condition-case nil
	(when (and (symbol-function (intern-soft doc-func))
		   match)
	  (epoxide-tsg-eldoc-doc-string doc-func))
      (error nil))))

(defun epoxide-tsg-eldoc-add-class (class-name inputs configs outputs
					    &optional node-name)
  "Add a new eldoc entry.

CLASS-NAME: class of the node.
INPUTS: input documentation of node.
CONFIGS: config documentation of node.
OUTPUTS: output documentation of node.
Optional NODE-NAME: if present the eldoc entry will be associated with
NODE-NAME, otherwise with NODE-CLASS."
  (let ((identifier (concat "class " class-name ":"))
	(input (when inputs
		 (concat "inputs: " (epoxide-tsg-eldoc-list-parameters
				     inputs))))
	(config (when configs
		  (concat "config: "
			  (epoxide-tsg-eldoc-list-parameters configs t 1))))
	(output (when outputs
		  (concat "outputs: "
			  (epoxide-tsg-eldoc-list-parameters outputs)))))
    (add-to-list 'epoxide-tsg-eldoc-documentations
		 `(,(or node-name class-name) .
		   ,(mapconcat 'identity
			       (delq nil (list identifier input config output))
			       "\n")))))

(defun epoxide-tsg-eldoc-list-parameters (parameters &optional first-line
						     offset)
  "Concatenate PARAMETERS into a string.

Use just the first lines if FIRST-LINE is not nil.

Format:
0: parameter #0; parameter #1; parameter #2; etc.

When OFFSET is present add offset to the index before converting
it to a string (e.g. configuation arguments should start with
index 1 instead of 0)."
  (let ((i -1))
    (mapconcat (lambda (p)
                 (setq i (1+ i))
                 (when first-line
                   (string-match "^\\(.*\\)$" p)
                   (setq p (match-string 1 p)))
                 (concat (number-to-string (if offset
					       (+ i offset)
					     i)) ": " p))
               parameters "; ")))


;; --------------    Parsing a .tsg file    -----------------------------------

(defun epoxide-tsg-clear-and-start ()
  "Decide whether to ask for confirmation before clearing this epoxide session.

When the current .tsg file has not been parsed, parse automatically,
otherwise ask for confirmation."
  (interactive)
  (if (and (epoxide-event-running-p) epoxide-tsg-node-list)
      (when (y-or-n-p "Clear current epoxide session and restart? ")
	(epoxide-tsg-clear-and-start-helper))
    (if (or (equal (point-max) 1)
	    (eq epoxide-start-tsg-on-file-open 'open-file-without-parsing))
	(message "You can start this TSG with C-x C-e")
    (unless (eq epoxide-start-tsg-on-file-open 'open-file-without-parsing)
      (when (or (eq epoxide-start-tsg-on-file-open 'start-tsg)
		(y-or-n-p "Start TSG? "))
	(epoxide-tsg-clear-and-start-helper))))))

(defun epoxide-tsg-clear-and-start-helper ()
  "Clear states belonging to this session then restart execution.

States are stored in buffer local variables and buffers.  The
session is determined by the current buffer."
  (epoxide-event-stop)
  (let ((epoxide-tsg-shutdown-buffer (current-buffer)))
    (epoxide-tsg-kill-buffers (current-buffer)))
  (epoxide-tsg-release-all-variables)
  (epoxide-tsg-start))

(defun epoxide-tsg-start ()
  "Parse .tsg file, open buffers and start execution.

Call `epoxide-call-config-change-handler' when restarting and
node's config has changed."
  (interactive)
  (let ((buf (current-buffer))
	(nodes (copy-sequence epoxide-tsg-node-list))
	n)
    (save-excursion
      (goto-char (point-min))
      (epoxide-tsg-start-state)
      (setq epoxide-tsg-root-buffer (buffer-name (current-buffer)))
      (message "Done parsing.")
      (epoxide-tsg-open-buffers (epoxide-event-running-p))
      (with-current-buffer buf
	(when (epoxide-event-running-p)
	  (dolist (node epoxide-tsg-node-list)
	    ;; Call node's change handler only when config really changed.
	    (setq n (epoxide-tsg-fetch-node-by-name-and-class (node-name node)
							   (node-class node)
							   nodes))
	    (when (and n
		       (not (equal (node-config-list n)
				   (node-config-list node))))
	      (epoxide-call-config-change-handler (node-name node)
						  (node-class node)))))))
    ;; Add nodes to framework, and schedule them.
    (epoxide-tsg-execute-tsg)
    (epoxide-view-show epoxide-view-last-index)))

(defun epoxide-tsg-execute-tsg ()
  "Start execution of TSG defined in the current buffer.
The troubleshooting graph itself is defined by the buffer local
variable `epoxide-tsg-node-list'."
  (dolist (node epoxide-tsg-node-list)
    (epoxide-task-start (get-buffer (epoxide-tsg-create-node-buffer-name
                                     (node-name node)
                                     (node-class node)))))
  (unless (epoxide-event-running-p)
    (epoxide-event-start)
    (message "Execution has been started...")))

(defun epoxide-tsg-check-buffer-create (buffer-name
				     root-buffer
				     &optional ignore-open-buffers)
  "Return a newly created buffer named with BUFFER-NAME.

If BUFFER-NAME already exists, kill all buffers associated with
ROOT-BUFFER and signal an error.  If IGNORE-OPEN-BUFFERS is
non-nil, don't signal an error and return the existing buffer."
  (if (or (not (get-buffer buffer-name))
	  ignore-open-buffers)
      (get-buffer-create buffer-name)
    (if (y-or-n-p (concat "Buffer '" buffer-name
			  "' already exists. Reassign it to this session? "))
	(get-buffer buffer-name)
      ;; TODO: it would be better to do the cleanup in
      ;; `epoxide-tsg-signal-name-conflict'
      (epoxide-tsg-kill-buffers root-buffer)
      (switch-to-buffer root-buffer)
      (epoxide-tsg-release-all-variables)
      (epoxide-tsg-signal-name-conflict buffer-name)
      nil)))

(defun epoxide-tsg-open-buffers (&optional ignore-open-buffers)
  "Open buffers according to nodes read from .tsg files.
Set up input, output and config-list buffer local variables in
the buffers representing nodes.  Do not fail when buffers already
exist if IGNORE-OPEN-BUFFERS is non-nil."
  (let ((root-buffer (get-buffer epoxide-tsg-root-buffer))
        node-buffer output-buffers)
    (dolist (node epoxide-tsg-node-list)
      ;; Create a buffer for the node.
      (setq node-buffer (epoxide-tsg-check-buffer-create
			 (epoxide-tsg-create-node-buffer-name
			  (node-name node) (node-class node))
			 epoxide-tsg-root-buffer
			 ignore-open-buffers))
      ;; Create link buffers.
      (setq output-buffers (mapcar (lambda (output)
				     (epoxide-tsg-check-buffer-create
				      output epoxide-tsg-root-buffer
				      ignore-open-buffers))
				   (node-output-buffers node)))
      ;; When all buffers are successfully created, intialize buffer local
      ;; variables.
      (unless (member nil (cons node-buffer output-buffers))
	(with-current-buffer node-buffer
	  ;; Create inputs, outputs and config-list as buffer local variables
	  ;; and assign the lists contained in structure node to them.
	  (when (equal major-mode 'fundamental-mode)
	    (epoxide-mode))
	  (epoxide-tsg-set-node node root-buffer)
	  ;; Assign functions to buffer local variables and call the init
	  ;; function of the node.
	  (epoxide-tsg-set-node-functions node))
	;; Initialize output buffers.
	(dolist (output (node-output-buffers node))
	  ;; Set mode of the output buffers and
	  ;; set root-buffer of the outputs to their node buffers.
	  (epoxide-tsg-set-output output root-buffer node-buffer))
	(with-current-buffer node-buffer
	  ;; Call init function after output buffer have been initialized.
	  ;; Init function has to overwrite initial buffer content if needed.
	  (funcall epoxide-init-function))))))

(defun epoxide-tsg-set-node-functions (node)
  "Load init, exec and stop functions of the NODE source.
Store functions in buffer local variables."
  (let ((init (intern (concat "epoxide-"
			      (downcase (node-class node)) "-init")))
	(exec (intern (concat "epoxide-"
			      (downcase (node-class node)) "-exec")))
	(stop (intern (concat "epoxide-"
			      (downcase (node-class node)) "-stop"))))
    (unless epoxide-exec-function
      (autoload exec (node-class node))
      (setq-local epoxide-exec-function exec))
    (unless epoxide-stop-function
      (autoload stop (node-class node))
      (setq-local epoxide-stop-function stop))
    (unless epoxide-init-function
      (autoload init (node-class node))
      (setq-local epoxide-init-function init)
      ;; Add initial content to node's buffer.
      (epoxide-tsg-init-node-buffer))))

(defun epoxide-tsg-set-output (output root-buffer src-node)
  "Initialize an output buffer.
Buffer OUTPUT is part of a TSG defined in ROOT-BUFFER and an
output link of SRC-NODE."
  (with-current-buffer output
    (epoxide-link-mode)
    (setq-local epoxide-root-buffer root-buffer)
    (setq-local epoxide-src-node src-node)
    (setq-local epoxide-init-function 'epoxide-link-init)
    (funcall epoxide-init-function)))

(defun epoxide-tsg-set-node (node root-buffer)
  "Initialize a node buffer.
Buffer NODE is part of a TSG defined in ROOT-BUFFER."
  (setq-local epoxide-node-name (node-name node))
  (setq-local epoxide-node-class (node-class node))
  (setq-local epoxide-node-inputs (node-input-buffers node))
  (setq-local epoxide-node-config-list (node-config-list node))
  (setq-local epoxide-node-dynamic-config-list (node-dynamic-config-list node))
  (setq-local epoxide-node-outputs (node-output-buffers node))
  (setq-local epoxide-root-buffer root-buffer))

(defun epoxide-tsg-init-node-buffer ()
  "Initialize node's buffer with standard information.

Listed information is: node name, class, configuration list,
source file location and `epoxide-mode's available key bindings."
  (erase-buffer)
  (epoxide-insert-node-basics)
  (insert "\n\n")
  (epoxide-insert-key-bindings "epoxide-mode-map")
  (goto-char (point-min)))

(defun epoxide-insert-node-basics ()
  "Insert basic node information to the current buffer.

Inforamtion contains: node name, class, configuration list and
source file location."
  (let ((max-length (if (> (length epoxide-node-config-list)
			     (length epoxide-node-dynamic-config-list))
			  (length epoxide-node-config-list)
			(length epoxide-node-dynamic-config-list)))
	  (i 0)
	  config)
      (while (< i max-length)
	(if (null (nth i epoxide-node-dynamic-config-list))
	    (push (nth i epoxide-node-config-list) config)
	  (push (concat "dynamic:"
			(nth i epoxide-node-dynamic-config-list))
		config))
	(incf i))
      (insert "NODE NAME: " epoxide-node-name "\nNODE CLASS: "
	      epoxide-node-class
	      "\nCONFIGURATION LIST: " (format "%s" (nreverse config))
	      "\nSOURCE FILE: "))
  (let ((source-file (find-lisp-object-file-name epoxide-init-function 'defun)))
    (insert-button source-file
		   'action (lambda (x)
			     (find-file (button-get x 'source)))
		   'source source-file
		   'follow-link t)))

(defun epoxide-insert-key-bindings (map-name)
  "Insert key bindings of keymap named MAP-NAME to the current buffer."
  (let ((key-bindings (split-string (substitute-command-keys
				     (concat "\\{" map-name "}")) "\n"))
        (separator t)
	function-name keys)
    (insert (upcase (car key-bindings)) "\n"
	    (cadr key-bindings) "\n")
    (setq key-bindings (cddr key-bindings))
    (while key-bindings
      (let ((binding (car key-bindings)))
        (if (equal (length binding) 0)
            (unless separator
              (insert "\n"))
          (setq separator nil)
          (if (string-match "^\\(.*?\\)\\s-\\{2,\\}\\(.+\\)" binding)
              (setq keys (match-string 1 binding)
                    function-name (match-string 2 binding))
            (setq keys "?"
                  function-name "?"))
          (unless (equal (downcase function-name) "prefix command")
            (insert keys "\t\t")
            (insert-button
             function-name
             'help-echo (if (fboundp (intern function-name))
                            (car (split-string
                                  (documentation (intern function-name))
                                  "\n")))
             'action (lambda (x)
                       (describe-function (button-get x 'function-name)))
             'function-name (intern function-name)
             'follow-link t)
            (insert "\n")))
        (setq key-bindings (cdr key-bindings))))))

(defun epoxide-tsg-create-node-buffer-name (node-name node-class)
  "Create a name for a node buffer.

Format: *node:<NODE-NAME>:<NODE-CLASS>*"
  (concat "*node:" node-name ":" node-class "*"))

(defun epoxide-tsg-start-state ()
  "Tsg file parsing state which handles the beginning of a new expression."
  (epoxide-tsg-skip-whitespaces)
  (when (< (point) (point-max))
    (cond
     ((member (following-char) '(?\( ?\;))
      ;; Expression cannot start with parameter list
      ;; or expression end character, signal error.
      (epoxide-tsg-signal-parse-error))
     ((equal (following-char) ?\[)
      ;; Parse input ports.
      (epoxide-tsg-input-buffers-state))
     ((member (current-word) '("::" "->" "-->"))
      ;; Expression cannot start with ::, -> or --> operators, signal error.
      (epoxide-tsg-signal-parse-error))
     (t
      (epoxide-tsg-name-or-class (current-word)))))
  (epoxide-tsg-skip-whitespaces)
  (unless (eobp)
    (epoxide-tsg-start-state)))

(defun epoxide-tsg-name-or-class (word)
  "Switch to class state if WORD can be found among the node definitions."
  (if (member word (epoxide-tsg-get-node-classes))
      (epoxide-tsg-class-state)
    (epoxide-tsg-name-state)))

(defun epoxide-tsg-skip-whitespaces ()
  "Skip whitespaces."
  (while (forward-comment 1)))

(defun epoxide-tsg-class-state ()
  "State for handling jobs and following states when reading a class name."
  (when (equal (current-word) "::")
    (forward-word))
  (if (member (current-word) (epoxide-tsg-get-node-classes))
      (setq-local epoxide-tsg-node-class (current-word))
    (epoxide-tsg-signal-no-such-node-class-error (current-word)))
  (epoxide-tsg-skip-whitespaces)
  (when (null epoxide-tsg-node-name)
    (setq epoxide-tsg-node-name
	  (epoxide-tsg-generate-new-name epoxide-tsg-node-class))
    (forward-word))
  (cond
   ((equal (following-char) ?\[)
    (epoxide-tsg-output-buffers-state))
   ((equal (following-char) ?\()
    (epoxide-tsg-config-state))
   ((equal (following-char) ?\;)
    (epoxide-tsg-expression-end-state))
   ((equal (current-word) "->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node)))
   ((equal (current-word) "-->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node))
    (epoxide-tsg-add-node-to-view))
   (t
    (epoxide-tsg-signal-parse-error))))

(defun epoxide-tsg-generate-new-name (class-name)
  "Create a new node name when it wasn't given.
Add an incremented index to the class named CLASS-NAME."
  (setq-local epoxide-tsg-node-name-generated-index
	      (1+ epoxide-tsg-node-name-generated-index))
  (concat (number-to-string epoxide-tsg-node-name-generated-index) "@"
	  class-name))

(defun epoxide-tsg-name-state ()
  "State for handling jobs and following states when reading a node name."
  (setq-local epoxide-tsg-node-name (current-word))
  (forward-word)
  (epoxide-tsg-skip-whitespaces)
  (cond
   ((equal (following-char) ?\;)
    (epoxide-tsg-expression-end-state))
   ((equal (following-char) ?\[)
    (epoxide-tsg-output-buffers-state))
   ((equal (current-word) "->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node)))
   ((equal (current-word) "::")
    (forward-word)
    (epoxide-tsg-class-state))
   ((equal (current-word) "-->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node))
    (epoxide-tsg-add-node-to-view))
   (t
    (epoxide-tsg-signal-parse-error))))

(defun epoxide-tsg-input-buffers-state ()
  "State for handling jobs and following states when reading input buffers."
  (epoxide-tsg-parse-input-buffers)
  (cond
   ((member (following-char) '(?\[ ?\( ?\;))
    (epoxide-tsg-signal-parse-error))
   ((member (current-word) '("->" "::" "-->"))
    (epoxide-tsg-signal-parse-error))
   (t
    (epoxide-tsg-name-or-class (current-word)))))

(defun epoxide-tsg-parse-input-buffers ()
  "Parse string at point as inputs to a node.
Values are saved to buffer local variable `epoxide-tsg-input-buffers'."
  (let ((start-pos (1+ (point))))
    (forward-list)
    (setq-local epoxide-tsg-input-buffers
		(map 'list 'epoxide-chomp
		     (split-string (buffer-substring-no-properties
				    start-pos (1- (point))) "," t))))
  (epoxide-tsg-skip-whitespaces))

(defun epoxide-tsg-output-buffers-state ()
  "State for handling jobs and following states when reading output buffers."
  (let ((start-pos (1+ (point))))
    (forward-list)
    (setq-local epoxide-tsg-output-buffers
		(map 'list 'epoxide-chomp
		     (split-string (buffer-substring-no-properties
				    start-pos (1- (point))) "," t))))
  (epoxide-tsg-skip-whitespaces)
  (cond
   ((equal (current-word) "->")
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node))
    (forward-word))
   ((equal (current-word) "-->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node))
    (epoxide-tsg-add-node-to-view))
   (t
    (epoxide-tsg-signal-parse-error))))

(defun epoxide-tsg-config-state ()
  "State for handling jobs and following states when reading config parameters."
  (epoxide-tsg-parse-config-list)
  (cond
   ((equal (following-char) ?\[)
    (epoxide-tsg-output-buffers-state))
   ((equal (following-char) ?\;)
    (epoxide-tsg-expression-end-state))
   ((equal (current-word) "->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node)))
   ((equal (current-word) "-->")
    (forward-word)
    (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node))
    (epoxide-tsg-add-node-to-view))
   (t
    (epoxide-tsg-signal-parse-error))))

(defun epoxide-tsg-parse-config-list ()
  "Parse string at point as config list to a node.
Values are saved to buffer local variable `epoxide-tsg-config-list'."
  (let ((start-pos (1+ (point))))
    (forward-list)
    (setq-local epoxide-tsg-config-list
		(map 'list 'epoxide-chomp
		     (split-string (buffer-substring-no-properties
				    start-pos (1- (point))) ",[ \f\t\n\r\v]+" t))))
  (epoxide-tsg-skip-whitespaces))

(defun epoxide-tsg-expression-end-state ()
  "State for handling jobs and following states when ending an expression."
  (unless (epoxide-tsg-create-view)
      (epoxide-tsg-create-node))
  (epoxide-tsg-release-temporary-variables)
  (epoxide-tsg-clear-prev-node)
  (forward-char)
  (epoxide-tsg-skip-whitespaces))

(defun epoxide-tsg-clear-prev-node ()
  "Clear information of previous node."
  (setq-local epoxide-tsg-prev-output-buffers nil)
  (setq-local epoxide-tsg-prev-node-name nil))

(defun epoxide-tsg-create-view (&optional assign-node-buffer)
  "Create a new View from the collected information.
When optional argument ASSIGN-NODE-BUFFER is non-nil, the
previous node's buffer is assigned to the specified inputs
instead of the link buffers."
  (let* ((view (epoxide-tsg-view-fetch-by-name epoxide-tsg-node-name))
	 (node (epoxide-tsg-fetch-node-by-name epoxide-tsg-node-name))
	 (class (if view
		    "View"
		  epoxide-tsg-node-class)))
    ;; When this item is not associated to any class, and it is not
    ;; a preexisting node, assume it is a view.
    (when (and (null class)
	       (null node))
      (setq class "View"))
    (when (equal class "View")
      (cond
       ((or node
	    (and view epoxide-tsg-node-class
		 (not (equal epoxide-tsg-node-class class))))
	;; If a view and a node has the same name: signal error.
	(epoxide-tsg-signal-name-conflict epoxide-tsg-node-name (line-number-at-pos)
				       (- (point) (line-beginning-position))))
       ((null view)
	;; A new view should be added to the list of views.
	(let ((name epoxide-tsg-node-name)
	      cols rows)
	  (if epoxide-tsg-config-list
	      (setq cols (string-to-number (nth 0 epoxide-tsg-config-list)))
	    (setq cols nil))
	  (if epoxide-tsg-config-list
	      (setq rows (string-to-number (nth 1 epoxide-tsg-config-list)))
	    (setq rows nil))
	  (epoxide-view-add epoxide-tsg-node-name cols rows
			    (if assign-node-buffer
				(epoxide-tsg-view-add-node-as-input-buffer view)
			      (epoxide-tsg-set-up-input-buffers nil)))
	  (epoxide-tsg-eldoc-add-node name "View" (current-buffer))))
       (view
	;; A previously defined view should be updated.
	(setf (view-input-buffers view)
	      (if assign-node-buffer
		  (epoxide-tsg-view-add-node-as-input-buffer view)
		(epoxide-tsg-set-up-input-buffers (view-input-buffers view))))))
      (epoxide-tsg-release-temporary-variables)
      t)))

(defun epoxide-tsg-view-add-node-as-input-buffer (view)
  "Add the previous node's buffer as input to VIEW."
  (let* ((new-input-buffers (when view (view-input-buffers view)))
	 (prev-node (epoxide-tsg-fetch-node-by-name epoxide-tsg-prev-node-name))
	 (prev-class (when prev-node
		       (node-class prev-node)))
	 (node-buffer (when prev-class
			(epoxide-tsg-create-node-buffer-name
			 epoxide-tsg-prev-node-name
			 prev-class))))
    (when node-buffer
      (unless epoxide-tsg-input-buffers
	(setq new-input-buffers (epoxide-setl new-input-buffers 0 node-buffer)))
      (dolist (num epoxide-tsg-input-buffers)
	(when (stringp num)
	  (setq num (string-to-number num)))
	(setq new-input-buffers
	      (epoxide-setl new-input-buffers num node-buffer))))
    new-input-buffers))

(defun epoxide-tsg-create-node ()
  "Create a node structure from the collected information."
  (let* ((node (epoxide-tsg-fetch-node-by-name epoxide-tsg-node-name))
	 (view (epoxide-tsg-view-fetch-by-name epoxide-tsg-node-name))
	 (class (when node (node-class node))))
    (cond
     ((and view class)
      ;; When there has been a view created with this name,
      ;; signal error.
      (epoxide-tsg-signal-name-conflict epoxide-tsg-node-name
					(line-number-at-pos)
					(- (point) (line-beginning-position))))
     ((and (null epoxide-tsg-node-class)
    	   (null node)
	   (null view))
      ;; If node has no assigned class now and it wasn't previously
      ;; defined either as a node or a view, signal error.
      (epoxide-tsg-signal-undefined-node-error epoxide-tsg-node-name))
     ((and node
	   epoxide-tsg-node-class
	   class
	   (not (equal epoxide-tsg-node-class class)))
      ;; Two nodes with the same name but with different classes are to be
      ;; created: signal error.
      (epoxide-tsg-signal-name-conflict (current-word) (line-number-at-pos)
				   (- (point) (line-beginning-position))))
     ((null node)
      ;; A new node should be added to the list of nodes.
      (let ((new-node (make-node
		       :name epoxide-tsg-node-name
		       :class epoxide-tsg-node-class
		       :config-list epoxide-tsg-config-list
		       :dynamic-config-list (epoxide-tsg-set-up-dynamic-config-list nil)
		       :input-buffers (epoxide-tsg-set-up-input-buffers nil)
		       :output-buffers (epoxide-tsg-set-up-output-buffers))))
	(setq-local epoxide-tsg-node-list (cons new-node epoxide-tsg-node-list))
	(setq-local epoxide-tsg-prev-node-name (node-name new-node))
	;; Add node to eldoc.
	(when (node-class new-node)
	  (epoxide-tsg-eldoc-add-node (node-name new-node) (node-class new-node)
				   (current-buffer)))))
     (node
      ;; A previously defined node should be updated.
      (setq-local epoxide-tsg-node-list (delq node epoxide-tsg-node-list))
      (let ((new-node (make-node
		       :name (node-name node)
		       :class (node-class node)
		       :config-list (if epoxide-tsg-config-list
					epoxide-tsg-config-list
				      (node-config-list node))
		       :dynamic-config-list (epoxide-tsg-set-up-dynamic-config-list
		       			     (node-dynamic-config-list node))
		       :input-buffers (epoxide-tsg-set-up-input-buffers
				       (node-input-buffers node))
		       :output-buffers
		       (delete-dups (append
				     (node-output-buffers node)
				     (epoxide-tsg-set-up-output-buffers))))))
	(setq-local epoxide-tsg-node-list (cons new-node epoxide-tsg-node-list))
	(setq-local epoxide-tsg-prev-node-name (node-name new-node))
	;; Add node to eldoc.
	(when (node-class new-node)
	  (epoxide-tsg-eldoc-add-node (node-name new-node) (node-class new-node)
				   (current-buffer)))))))
  (epoxide-tsg-release-temporary-variables))

(defun epoxide-tsg-add-node-to-view ()
  "Add the previous node the current node."
  (when epoxide-tsg-prev-output-buffers
    (message (concat "'-->' adds a node buffer to a view, "
		     "output links are ignored: line %d, character %d")
	     (line-number-at-pos)
	     (- (point) (line-beginning-position))))
  (epoxide-tsg-skip-whitespaces)
  (when (equal (following-char) ?\[)
    (epoxide-tsg-parse-input-buffers))
  (if (equal (current-word) "View")
      (progn
	(setq-local epoxide-tsg-node-class "View")
	(setq-local epoxide-tsg-node-name
		    (epoxide-tsg-generate-new-name epoxide-tsg-node-class))
	(forward-word))
    (setq-local epoxide-tsg-node-name (current-word))
    (forward-word)
    (epoxide-tsg-skip-whitespaces)
    (when (equal (current-word) "::")
      (forward-word)
      (epoxide-tsg-skip-whitespaces)
      (if (equal (current-word) "View")
	  (setq-local epoxide-tsg-node-class "View")
	(epoxide-tsg-signal-view-assignment-error epoxide-tsg-node-name))))
  (unless epoxide-tsg-node-class
    (setq-local epoxide-tsg-node-class "View")
    (unless (member epoxide-tsg-node-name (mapcar 'view-name epoxide-view-list))
      (epoxide-tsg-signal-undefined-view-error epoxide-tsg-node-name)))
  (epoxide-tsg-skip-whitespaces)
  (when (equal (following-char) ?\()
    (epoxide-tsg-parse-config-list))
  (cond
   ((equal (following-char) ?\;)
    (epoxide-tsg-create-view t))
   (t
    (if (search-forward ";" nil t)
	(epoxide-tsg-create-view t)
      (message (concat "A View should be the last item and it has no outputs, "
		       "ignoring the rest at line %d, character %d")
	       (line-number-at-pos) (- (point) (line-beginning-position)))
      (goto-char (point-max))
      (epoxide-tsg-create-view t))))
  (forward-char)
  (epoxide-tsg-release-temporary-variables)
  (epoxide-tsg-clear-prev-node))

(defun epoxide-tsg-fetch-node-by-name (name)
  "Fetch the node that has the name NAME from epoxide-tsg-node-list."
  (dolist (head epoxide-tsg-node-list)
    (when (equal (node-name head) name)
      (return head))))

(defun epoxide-tsg-set-up-input-buffers (inputs)
  "Set input buffers to those indicated by the previous node.

If the number of previous output buffers and the current input
buffers don't match signal an error.  INPUTS is a list that
specifies the input buffers for a node.  The numbers contained in
epoxide-tsg-input-buffers select the indices of INPUTS that should
be modifed.  Modified elements are set to the approriate previous
output buffers."
  (cond
   ((null epoxide-tsg-prev-node-name)
    inputs)
   ((and (equal 1 (length epoxide-tsg-prev-output-buffers))
	 (member (length epoxide-tsg-input-buffers) '(0 1)))
    (if (null epoxide-tsg-input-buffers)
	;; Connect input 0 to output 0 of previous node.
	(epoxide-setl inputs 0 (car epoxide-tsg-prev-output-buffers))
      ;; Connect the specified input to output 0 of previous node.
      (epoxide-setl inputs (string-to-number (car epoxide-tsg-input-buffers))
                    (car epoxide-tsg-prev-output-buffers))))
   ((and (null epoxide-tsg-prev-output-buffers)
	 (member (length epoxide-tsg-input-buffers) '(0 1))
	 epoxide-tsg-prev-node-name)
    (if (null epoxide-tsg-input-buffers)
	;; Connect input 0 to output 0 of previous node.
	(epoxide-setl inputs 0 (epoxide-tsg-assign-name-to-output-buffer
                                epoxide-tsg-prev-node-name 0))
      (if (< (string-to-number (car epoxide-tsg-input-buffers)) 0)
      	  ;; When a config parameter should be modified, nothing needs to be
      	  ;; changed.
      	  inputs
      	;; Connect the specified input to output 0 of previous node.
      	(epoxide-setl inputs (string-to-number (car epoxide-tsg-input-buffers))
      		      (epoxide-tsg-assign-name-to-output-buffer
      		       epoxide-tsg-prev-node-name 0)))))
   ((not (equal (length epoxide-tsg-prev-output-buffers)
		(length epoxide-tsg-input-buffers)))
    ;; Cannot pair inputs with outputs, signal error.
    (epoxide-tsg-signal-output-input-mismatch epoxide-tsg-node-name))
   (t
    ;; Discover items to be associated with inputs.
    (let* ((p (epoxide-tsg-separate-input-and-config
	       epoxide-tsg-input-buffers epoxide-tsg-prev-output-buffers))
	   (in (cadr (assoc 'input-input p)))
	   (out (cadr (assoc 'output-input p))))
      ;; Pair the specified inputs with the specified outputs of the previous
      ;; node.
      (epoxide-enumerate (i input in inputs)
      	(epoxide-list-setq inputs (string-to-number input)
      			   (nth i out)))))))

(defun epoxide-tsg-separate-input-and-config (inputs outputs)
  "Discover which buffers should connected to inputs and which ones to config.

INPUTS carries a node's input specification (read from the .tsg
file) and is a list of positive and negative indices (where
negative values define assignment to configuration arguments)
wile OUTPUTS carries the outputs of the previous node's
outputs (positive indices)."
  (let ((in (copy-sequence inputs))
	(out (copy-sequence outputs))
	input-i config-i input-o config-o)
    (dolist (i in)
      (if (> (string-to-number i) -1)
	(progn
	  (push (pop out) input-i)
	  (push i input-o))
	(push (pop out) config-i)
	(push (number-to-string (* (1+ (string-to-number i)) -1)) config-o)))
    (setq input-i (nreverse input-i))
    (setq config-i (nreverse config-i))
    (setq input-o (nreverse input-o))
    (setq config-o (nreverse config-o))
    `((output-input ,input-i)
      (output-config ,config-i)
      (input-input ,input-o)
      (input-config ,config-o))))

(defun epoxide-tsg-set-up-output-buffers ()
  "Give output buffers a name based on the node name and their index.
If there is a '(auto-create . nil) proprety in the node class
definition (epoxide-CLASS-output-info), then the output link is
created only if it is connected into another node as input.

Set `epoxide-tsg-prev-output-buffers' to the buffer names generated
by this node's output buffer numbers.  This function should be
called only after the node's input buffers are set otherwise
links will not be connected properly."
  (let* (enabled-outputs tmp)
    (setq-local epoxide-tsg-prev-output-buffers
		(mapcar (lambda (x)
			  (epoxide-tsg-assign-name-to-output-buffer
			   epoxide-tsg-node-name x))
			epoxide-tsg-output-buffers))
    (when epoxide-tsg-node-class
      (let* ((fun-name (concat "epoxide-" (downcase epoxide-tsg-node-class)
                               "-output-info"))
             (def (funcall (intern-soft fun-name)))
             (def (mapcar (lambda (x) (assoc 'auto-create x)) def)))
        (epoxide-enumerate (i o def)
			   (unless (equal o '(auto-create . nil))
			     (setq enabled-outputs (cons i enabled-outputs))))))
    (setq enabled-outputs (append enabled-outputs epoxide-tsg-output-buffers))
    (dolist (e enabled-outputs)
      (if (stringp e)
	  (push (string-to-number e) tmp)
	(push e tmp)))
    (setq enabled-outputs (delete-dups (sort (nreverse tmp) '<)))
    (mapcar (lambda (x)
    	      (epoxide-tsg-assign-name-to-output-buffer
    	       epoxide-tsg-node-name x))
    	    enabled-outputs)))

(defun epoxide-tsg-set-up-dynamic-config-list (configs)
  "Assign node output buffers to config arguments.

CONFIGS contains the node's current dynamic configuration list."
  (cond
   ((and (null epoxide-tsg-prev-output-buffers)
	 (equal (length epoxide-tsg-input-buffers) 1)
	 (< (string-to-number (car epoxide-tsg-input-buffers)) 0)
	 epoxide-tsg-prev-node-name)
    ;; Connect the specified config to output 0 of previous node.
    (epoxide-setl configs (* -1 (1+ (string-to-number
				     (car epoxide-tsg-input-buffers))))
		  (epoxide-tsg-assign-name-to-output-buffer
		   epoxide-tsg-prev-node-name 0)))
   (t
    ;; Discover items to be associated with config arguments.
    (let* ((p (epoxide-tsg-separate-input-and-config
	       epoxide-tsg-input-buffers
	       epoxide-tsg-prev-output-buffers))
	   (output-buffers (cadr (assoc 'output-config p)))
	   (config-indices (cadr (assoc 'input-config p))))
      ;; Pair the specified configuration arguments with the specified
      ;; outputs of the previous node.
      (epoxide-enumerate (c config config-indices configs)
	(epoxide-list-setq configs (string-to-number config)
			   (nth c output-buffers)))))))

(defun epoxide-tsg-assign-name-to-output-buffer (node-name output-number)
  "Create a name for the output buffer of node NODE-NAME.
The output is marked by OUTPUT-NUMBER.

The name has the following structure: *link:<NODE-NAME>:<OUTPUT-NUMBER>*"
  (concat "*link:" node-name ":" (format "%s" output-number) "*"))

(defun epoxide-restart-node ()
  "Restart a node.

Call stop then init function of the selected node and start a
task for this node.  Selected node is determined as follows:
- in epoxide-tsg-mode: ido completion is offered for the node names;
- in epoxide-mode: the current node;
- in epoxide-link-mode: the node that has this link as its
  output;
- otherwise: no node is selected.
Node will not be reparsed so in/output and config changes
in the tsg file will not take effect."
  (interactive)
  (let ((root-buffer (epoxide-get-root-buffer))
	(node-name (pcase major-mode
		     (`epoxide-tsg-mode
		      (ido-completing-read
		       "Nodes: " (mapcar 'node-name epoxide-tsg-node-list)))
		     (`epoxide-link-mode
		      (with-current-buffer epoxide-src-node
			epoxide-node-name))
		     (`epoxide-mode
		      epoxide-node-name)
		     (other-mode
		      nil)))
	node node-buffer-name)
    (when (and node-name
	       (y-or-n-p (concat "Restart node " node-name "? ")))
      (with-current-buffer root-buffer
	(setq node (epoxide-tsg-fetch-node-by-name node-name)))
      (if (null node)
	  (message "No such node: %s" node-name)
	(with-current-buffer
	    (setq node-buffer-name (epoxide-tsg-create-node-buffer-name
				    (node-name node) (node-class node)))
	  (funcall epoxide-stop-function)
	  (funcall epoxide-init-function)
	  (epoxide-task-start (get-buffer node-buffer-name)))))))


;; --------------    Views    --------------------------------------------------

(defun epoxide-tsg-view-fetch-by-name (name)
  "Return the view named NAME from `epoxide-view-list'."
  (dolist (view epoxide-view-list)
    (when (equal name (view-name view))
      (return view))))

(defun epoxide-view-add (name cols rows buf-list)
  "Add a view defined by its paramters to the list of views.
NAME, COLS, ROWS and BUF-LIST are the attributes of a view structure."
  (setq-local epoxide-view-list (nreverse (cons (make-view
					   :name name
					   :cols cols
					   :rows rows
					   :input-buffers buf-list)
					  epoxide-view-list))))

(defun epoxide-view-show (number)
  "Show view #NUMBER from 'epoxide-view-list.

The view's layout is defined by the cols and rows attribute of
the view if present.  Otherwise, an automatic layout which shows
every connected input of the given view is presented.

This function is based on 'split-window-multiple-ways from
http://www.emacswiki.org/emacs/GridLayout."
  (interactive "p")
  (with-current-buffer (epoxide-get-root-buffer)
    (when (null epoxide-view-list)
      (epoxide-view-add epoxide-default-view-name nil nil
			`(,(buffer-name)
			  ,(car (mapcar 'identity
				  (delq nil
					(apply 'append
					       (mapcar
						'node-output-buffers
						(epoxide-get-nodes
						 (current-buffer)))))))))
      (epoxide-tsg-eldoc-add-node epoxide-default-view-name "View"
				  (current-buffer))))
  (let* ((number (if current-prefix-arg
		     number
		   (with-current-buffer (epoxide-get-root-buffer)
		     epoxide-view-last-index)))
	 (view (with-current-buffer (epoxide-get-root-buffer)
		 (nth number epoxide-view-list)))
	x y buffers w)
    (when view
      (setq buffers (copy-sequence (view-input-buffers view)))
      (when (view-rows view)
	  (setq y (view-rows view)))
      (when (view-cols view)
	  (setq x (view-cols view)))
      (if (or (null y) (null x))
	  (epoxide-display-buffers buffers t)
	(delete-other-windows)
	(setq w (selected-window))
	(dotimes (i (1- x))
	  (split-window-horizontally)
	  (dotimes (j (1- y))
	    (split-window-vertically))
	  (other-window y))
	(dotimes (j (1- y))
	  (split-window-vertically))
	(balance-windows)
	(select-window w)
	(dotimes (i (* x y))
	  (switch-to-buffer (pop buffers))
	  (other-window 1)))
      (with-current-buffer (epoxide-get-root-buffer)
	(setq-local epoxide-view-last-index number))
      (message "Switched to EPOXIDE View #%s (%s)." number (view-name view)))))

(defun epoxide-view-show-adjacent (direction)
  "Show next or prevoius view from the view-list depending on DIRECTION.
If the end of the list is reached, show the first view.  If the
beginning of the list is reached, show the first view.  Current
position is stored in `epoxide-view-last-index'."
  (let* ((cur-idx (with-current-buffer (epoxide-get-root-buffer)
		    epoxide-view-last-index))
	 (view-list (with-current-buffer (epoxide-get-root-buffer)
		     epoxide-view-list))
	 new-idx)
    (when (> direction 0)
      (setq new-idx (1+ cur-idx))
      (unless (nth new-idx view-list)
	(setq new-idx 0)))
    (when (< direction 0)
      (setq new-idx (1- cur-idx))
      (when (< new-idx 0)
	(setq new-idx (1- (length view-list)))))
    (with-current-buffer (epoxide-get-root-buffer)
      (setq-local epoxide-view-last-index new-idx))
    (epoxide-view-show new-idx)))

(defun epoxide-view-show-next ()
  "Show next view."
  (interactive)
  (epoxide-view-show-adjacent 1))

(defun epoxide-view-show-prev ()
  "Show previous view."
  (interactive)
  (epoxide-view-show-adjacent -1))

(defun epoxide-show-aftereval-view (root-buffer)
  "Show an .tsg file in ROOT-BUFFER and its last link in two separate windows."
  (let ((links (mapcar 'identity
		       (delq nil
			     (apply 'append (mapcar
					     'node-output-buffers
					     (epoxide-get-nodes
					      root-buffer)))))))
    (when links
      (delete-other-windows)
      (split-window-vertically)
      (switch-to-buffer (get-buffer (car links))))))

;; Implement node specific documentary functions of View.
(defun epoxide-view-input-info ()
  "Provide documentation, value tips and validation for view inputs."
    '(((doc-string . "links to show"))))

(defun epoxide-view-output-info ()
  "Dummy function."
  nil)

(defun epoxide-view-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "number of columns")
     (value-validator . epoxide-positive-number-p))
    ((doc-string . "number of rows")
     (value-validator . epoxide-positive-number-p))))

(defun epoxide-view-show-views ()
  "Show view associated buffers in an Ibuffer listing.
Provide option to change to a new view based on a selection of
its name."
  (interactive)
  (let* ((i 0)
	 (ibuffer-show-empty-filter-groups nil) ;; Hide empty groups.
	 (root-buffer (epoxide-get-root-buffer))
	 groups views ibuffer-saved-filter-groups selected-view)
    (with-current-buffer root-buffer
      (dolist (v epoxide-view-list)
	(let ((name (view-name v)))
	  (push (epoxide-ibuffer-build-filter-list
		 (format "Buffers in view %s (#%s)" name i)
		 (copy-sequence (view-input-buffers v)))
		groups)
	  (push name views)
	  (incf i))))
    (setq groups (nreverse groups))
    (setq views (nreverse views))
    (setq ibuffer-saved-filter-groups
          (append ibuffer-saved-filter-groups
                  (list (cons "epoxide" (list groups)))))
    (ibuffer)
    (setq ibuffer-filter-groups groups)
    (ibuffer-update nil t)
    (setq selected-view (with-current-buffer root-buffer
			  (ido-completing-read
			   "Choose view: "
			   views)))
    (let ((selected-index (when (member selected-view views)
			    (- (length views)
			       (length (member selected-view views))))))
      (kill-buffer "*Ibuffer*")
      (if (null selected-index)
	  (message "No such view: %s" selected-view)
	(with-current-buffer root-buffer
	  (setq-local epoxide-view-last-index selected-index))
	(epoxide-view-show 0)))))


;; --------------    Adding a new node    --------------------------------------

(defun epoxide-get-every-node-name ()
  "Collect every node name from open .tsg files."
  (cons nil
	(apply 'append (mapcar
			(lambda (buffer)
			  (with-current-buffer buffer
			    (when (boundp 'epoxide-tsg-node-list)
			      (mapcar 'node-name epoxide-tsg-node-list))))
			(buffer-list)))))

(defun epoxide-get-uniqe-name (names node-class root-buffer)
  "Generate a unique name among NAMES for a new node of NODE-CLASS.
NAMES is list the current names, NODE-CLASS is the class of the new
node, and the TSG is defined in ROOT-BUFFER."
  (with-current-buffer root-buffer
    (let ((name (epoxide-tsg-generate-new-name node-class)))
      (while (or (member name names)
		 (member (epoxide-tsg-create-node-buffer-name
			  name node-class) (mapcar 'buffer-name
                                                   (buffer-list))))
	(setq name (epoxide-tsg-generate-new-name node-class)))
      (list name))))

(defun epoxide-add-new-node ()
  "Create a new node and add it to the current TSG.

Offers node classes to choose from, proposes node name.  If these
are provided a new node is added to the .tsg file, and the TSG.
Output number 0 is automatically created.  Further in/output and
config setup can be done later via the configuration screen."
  (interactive)
  (let ((root-buffer (epoxide-get-root-buffer))
        (names (epoxide-get-every-node-name))
	(classes (epoxide-tsg-get-node-classes))
	name class output node-buffer output-buffer node init exec stop)
    ;; Node class can only be one from the known classes.
    (while (not (member class classes))
      (setq class (ido-completing-read
		   (concat "Node class: ") classes)))
    ;; Node name should be unique.
    (while (or (member name names)
	       (member (epoxide-tsg-create-node-buffer-name
			name class) (mapcar 'buffer-name
					    (buffer-list))))
      (let ((proposed-name (epoxide-get-uniqe-name names class root-buffer)))
	(setq name (ido-completing-read
		    (concat "Node name: ") proposed-name))
	;; If proposed-name was not used, the generation index should be reset
	;; to its previous value.
	(unless (equal name proposed-name)
	  (with-current-buffer root-buffer
	    (decf epoxide-tsg-node-name-generated-index)))))
    ;; Assign a buffer name to the first output.
    (setq output (epoxide-tsg-assign-name-to-output-buffer name 0))
    (setq node (make-node
		:name name
		:class class
		:config-list nil
		:input-buffers nil
		:output-buffers (list output)))
    ;; Add node to node list of root buffer.
    (with-current-buffer root-buffer
      (setq-local epoxide-tsg-node-list (cons node epoxide-tsg-node-list)))
    ;; Create a buffer for the node.
    (setq node-buffer (epoxide-tsg-check-buffer-create
		       (epoxide-tsg-create-node-buffer-name
			(node-name node) (node-class node))
		       epoxide-tsg-root-buffer))
    ;; Create output buffer.
    (setq output-buffer (epoxide-tsg-check-buffer-create
			 output epoxide-tsg-root-buffer))
    ;; When both buffers are successfully created, intialize buffer local
    ;; variables.
    (unless (member nil (cons node-buffer output-buffer))
      ;; Assign buffer local variables.
      (with-current-buffer node-buffer
	;; Add node to .tsg file.
	(epoxide-tsg-add-new-node-to-tsg-file root-buffer node)
	;; Create inputs, outputs and config-list as buffer local variables
	;; and assign the lists contained in structure node to them.
	(epoxide-mode)
	(epoxide-tsg-set-node node root-buffer)
	;; Assign functions to buffer local variables and call the init
	;; function of the node.
	(epoxide-tsg-set-node-functions node)
	;; Initialize output buffer.
	(epoxide-tsg-set-output output root-buffer node-buffer)
	;; Add node to scheduler.
	(epoxide-task-start node-buffer)
	;; Show node configuration.
	(switch-to-buffer node-buffer)
	(epoxide-variables-list)))))

(defun epoxide-tsg-add-new-node-to-tsg-file (root-buffer node)
  "Add a new node to a TSG defined in ROOT-BUFFER.
The NODE is added to first line of the .tsg file in ROOT-BUFFER.
File is not saved."
  (with-current-buffer root-buffer
    (save-excursion
      (goto-char (point-min))
      (insert (node-name node) " :: " (node-class node) ";\n"))))


;; --------------    Signal EPOXIDE errors    ----------------------------------

(defun epoxide-tsg-signal-parse-error ()
  "Signal a parsing error."
  (epoxide-tsg-release-all-variables)
  (error "Parse error at line %d character %d" (line-number-at-pos)
	 (- (point) (line-beginning-position))))

(defun epoxide-tsg-signal-no-such-node-class-error (class)
  "Signal an error to signify that CLASS does not have a definition."
  (epoxide-tsg-release-all-variables)
  (error "No definition for node class '%s' at line %d character %d" class
	 (line-number-at-pos) (- (point) (line-beginning-position))))

(defun epoxide-tsg-signal-undefined-node-error (node-name)
  "Signal error that NODE-NAME was not assigned to a class."
  (epoxide-tsg-release-all-variables)
  (error "Node '%s' at line %d hasn't been assigned to a class" node-name
	 (line-number-at-pos)))

(defun epoxide-tsg-signal-undefined-view-error (view-name)
  "Signal error that VIEW-NAME was not assigned to a class."
  (epoxide-tsg-release-all-variables)
  (error "View '%s' at line %d hasn't been assigned to a class" view-name
	 (line-number-at-pos)))

(defun epoxide-tsg-signal-output-input-mismatch (node-name)
  "Signal an input-output assignment error.
It is signalled when the nodes on each side of a connection do
not have matching number of output and input buffers.  NODE-NAME
signifies the receiving node."
  (epoxide-tsg-release-all-variables)
  (error "Output-input mismatch with node %s at line %d" node-name
	 (line-number-at-pos)))

(defun epoxide-tsg-signal-name-conflict (node-or-buffer-name
				      &optional line character)
  "Signal an error when two nodes try to use the same name.
NODE-OR-BUFFER-NAME marks the buffer where the error occured and
optional arguments LINE and CHARACTER specify the position the
error was detected at."
  (if (not (and line character))
      (error "Error creating '%s': name is already in use"
	    node-or-buffer-name))
  (epoxide-tsg-release-all-variables)
  (error
   (concat "Error creating node '%s' at line %s, character %s: "
	   "name is already in use.")
   node-or-buffer-name line character))

(defun epoxide-tsg-signal-view-assignment-error (view-name)
  "Signal an error VIEW-NAME is to be assigned to a class other then 'View'."
  (epoxide-tsg-release-all-variables)
  (error (concat "'-->' should be followed by a view, not a node of other "
		 "class at line %d, character %d")
		 (line-number-at-pos) (- (point) (line-beginning-position))))

(defun epoxide-signal-function-eval-error (function err)
  "Give a message when there was an error applying FUNCTION.

FUNCTION gives the read function-like expression (function,
lambda function, special form) and ERR is the error signalled
when applying or eavluating FUNCTION."
  (epoxide-log (format
		(concat
		 "Epoxide function eval/apply error in "
		 "%s::%s with function-like form %s: %s")
		epoxide-node-name epoxide-node-class
		function (error-message-string err)))
  nil)


;; --------------    Killing EPOXIDE buffers and erasing    --------------------
;; ---------------------    buffer local variables    --------------------------

(defun epoxide-tsg-release-temporary-variables ()
  "Set variables used for storing current node information to nil."
  (setq-local epoxide-tsg-node-name nil)
  (setq-local epoxide-tsg-node-class nil)
  (setq-local epoxide-tsg-input-buffers nil)
  (setq-local epoxide-tsg-output-buffers nil)
  (setq-local epoxide-tsg-config-list nil))

(defun epoxide-tsg-release-all-variables ()
  "Set variables storing all node information to nil."
  (epoxide-tsg-clear-prev-node)
  (epoxide-tsg-release-temporary-variables)
  (setq-local epoxide-tsg-node-list nil)
  (setq-local epoxide-view-list nil))

(defun epoxide-get-root-buffer ()
  "Return the root-buffer associated with the current buffer."
  (if (and (boundp 'epoxide-root-buffer)
           epoxide-root-buffer)
      (get-buffer epoxide-root-buffer)
    (if (eq major-mode 'epoxide-tsg-mode)
        (current-buffer)
      nil)))

(defun epoxide-get-value (variable &optional root-buffer)
  "Return the local value of VARIABLE in ROOT-BUFFER.
If ROOT-BUFFER is nil, return the value of the root-buffer of the
current buffer."
  (let ((buffer (or root-buffer
                    (epoxide-get-root-buffer))))
    (with-current-buffer buffer
        (symbol-value variable))))

(defun epoxide-get-nodes (&optional root-buffer)
  "Return the list of nodes in the TGS of ROOT-BUFFER."
  (epoxide-get-value 'epoxide-tsg-node-list root-buffer))

;; The buffer where a tsg-kill is originated.
(defvar epoxide-tsg-shutdown-buffer nil)

(defun epoxide-kill-buffer-query-function ()
  "Ask before killing a buffer running a troubleshooting graph."
  (let ((root-buffer (epoxide-get-root-buffer)))
    (if (and root-buffer
             (not epoxide-tsg-shutdown-buffer)
	     (member major-mode
		     '(epoxide-tsg-mode epoxide-mode
		       epoxide-link-mode epoxide-tsg-visualizer-mode)))
        ;; This buffer is associated with a TSG.
	(let ((name (buffer-name root-buffer)))
	  (if (with-current-buffer root-buffer
		(and (epoxide-event-running-p) epoxide-tsg-node-list))
	      (y-or-n-p-with-timeout
	       (concat "Kill this epoxide session (" name ")? ")
	       5 nil)
	    t))
      t)))

(defun epoxide-tsg-kill-all ()
  "Kill all epoxide related buffers, including the root buffer."
  (epoxide-tsg-kill t))

(defun epoxide-tsg-kill (&optional kill-root-buffer)
  "Kill all buffers of curret buffer's troubleshooting graph.
But keep the current buffer, so it could be used in the
`kill-buffer-hook'.

When optional argument KILL-ROOT-BUFFER is non-nil kill the
buffer that displays the .tsg file also."
  (unless epoxide-tsg-shutdown-buffer
    (let* ((root-buffer (epoxide-get-root-buffer))
           (epoxide-tsg-shutdown-buffer (current-buffer)))
      (if kill-root-buffer
	  (with-current-buffer root-buffer
	    (set-window-configuration epoxide-initial-window-config))
	(delete-other-windows)
	(switch-to-buffer root-buffer))
      (save-excursion
	(epoxide-event-stop)
        (epoxide-tsg-kill-buffers
	 root-buffer
	 (delq nil (list (when kill-root-buffer
			   root-buffer)
			 "*Ibuffer*"
			 (epoxide-tsg-visualizer-create-buffer-name
			  (buffer-name root-buffer)))))))))

(defun epoxide-tsg-kill-buffers (root-buffer &optional buffers-to-close)
  "Kill buffers associated with a TSG defined in ROOT-BUFFER.
BUFFERS-TO-CLOSE: additional buffers to kill."
  (let ((nodes (epoxide-get-nodes root-buffer)))
    (dolist (node nodes)
      (when (get-buffer (epoxide-tsg-create-node-buffer-name
			 (node-name node) (node-class node)))
	(with-current-buffer (epoxide-tsg-create-node-buffer-name
			      (node-name node) (node-class node))
	  ;; Call stop function of node.
	  (funcall epoxide-stop-function)
	  ;; Collect node buffer name and output buffers of every node.
	  ;; Output buffers cannot be killed at this time since other still
	  ;; active nodes might need them.
	  (setq buffers-to-close (cons (buffer-name (current-buffer))
				       buffers-to-close))
	  (setq buffers-to-close (append (node-output-buffers node)
					 buffers-to-close)))))
    ;; All nodes are stopped at this point, every buffer can be killed.
    (dolist (buffer (delq nil buffers-to-close))
      (epoxide-kill-buffer buffer t))))


;; --------------    Showing and editing EPOXIDE buffer local    ---------------
;; --------------    variables, in/output or config parameters   ---------------

(defun epoxide-variable-show-value (variable)
  "Show buffer local value of VARIABLE.
If VARIABLE is nil, show every buffer local variables and their values."
  (interactive "sVariable: ")
  (if (not (equal variable ""))
      (message "Value of '%s' is %s in buffer '%s'." variable
	       (buffer-local-value (intern-soft variable) (current-buffer))
	       (current-buffer))
    (epoxide-variables-list)))

(defun epoxide-variable-customize ()
  "Using IDO, list epoxide related buffer local variables.
When one is selected show either an editable or non-editable form
for it depending on what was selected."
  (interactive)
  (let* ((node-name (when (local-variable-p 'epoxide-node-name)
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
     ((string-match "epoxide-node-config-list\\|inputs" (symbol-name (car var)))
      ;; Show config or input parameters in an editable form.
      (epoxide-variable-edit var node-name node-class root-buffer))
     ((string-match "outputs" (symbol-name (car var)))
      (epoxide-edit-output-variable node-name node-class root-buffer))
     (t
      ;; Show other parameters in an uneditable form.
      (epoxide-variable-show var node-name node-class root-buffer)))))

(defun epoxide-variables-list ()
  "List epoxide related local variables of the current buffer and their values."
  (interactive)
  (let ((variables (epoxide-tsg-select-epoxide-variables
		    (buffer-local-variables)))
	(node-name (when (local-variable-p 'epoxide-node-name)
		     epoxide-node-name))
	(node-class (when (local-variable-p 'epoxide-node-class)
		      epoxide-node-class))
	(root-buffer (when (local-variable-p 'epoxide-root-buffer)
		       epoxide-root-buffer))
	(buffer-name "*Help*")
	(title (concat "Epoxide related buffer local variables of buffer '"
		       (buffer-name (current-buffer)) "'"))
	label)
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (fundamental-mode)
    (setq buffer-read-only)
    (erase-buffer)
    (insert title "\n" (make-string (length title) ?=) "\n\n")
    (dolist (var variables)
      ;; Insert variable name and values.
      (setq label (pp-to-string var))
      ;; Make config or input related variables editable.
      (cond
       ((string-match "epoxide-node-config-list\\|inputs"
		      (symbol-name (car var)))
	(insert-button label
		       'action (lambda (x)
				 (epoxide-variable-edit
				  (button-get x 'variable)
				  (button-get x 'node-name)
				  (button-get x 'node-class)
				  (button-get x 'root-buffer)))
		       'variable var
		       'node-name node-name
		       'node-class node-class
		       'root-buffer root-buffer
		       'follow-link t))
       ((string-match "outputs" (symbol-name (car var)))
	(insert-button label
		       'action (lambda (x)
				 (epoxide-edit-output-variable
				  (button-get x 'node-name)
				  (button-get x 'node-class)
				  (button-get x 'root-buffer)))
		       'node-name node-name
		       'node-class node-class
		       'root-buffer root-buffer
		       'follow-link t))
       (t
	(insert label))))
    (help-mode)
    (setq truncate-lines 0)))

(defun epoxide-variable-edit (variable node-name node-class root-buffer)
  "Show a form to edit values of a VARIABLE of a the current node.
Current node is specified with is NODE-NAME, NODE-CLASS and ROOT-BUFFER."
  (epoxide-kill-buffer "*set value*")
  (switch-to-buffer (get-buffer-create "*set value*"))
  (let ((inhibit-read-only t)
	(doc (epoxide-tsg-eldoc-get-doc-string node-class
					    (symbol-name (car variable))))
	values)
    (erase-buffer)
    (remove-overlays)
    (widget-insert "Set value of variable '" (symbol-name (car variable))
		   "' of node '" node-name ":" node-class
		   "'\nthat originates from " (if (bufferp root-buffer)
						  (buffer-name root-buffer)
						root-buffer)
		   ":\n\n")
    (when (string-match "config-list" (symbol-name (car variable)))
      (widget-insert
       "Note: dynamic configuration arguments take priority over static ones."
       "\n\n"))
    ;; Create an editable list for getting new parameters.
    (widget-create 'editable-list
		   :entry-format "%i %d %v"
		   :value (cdr variable)
		   '(epoxide-editable-field :value "nil"))
    ;; Insert documentation for each field.
    (when doc
      (widget-insert "\nWhere:\n")
      (epoxide-enumerate (i x doc)
        (widget-insert "#" (number-to-string i) ": " x "\n")))
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (epoxide-variable-set))
		   "Apply")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (epoxide-variable-set t))
		   "Apply and save")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-buffer))
		   "Cancel")
    ;; Save information for later (when setting the value of variable).
    (set (make-local-variable 'epoxide-variable) (car variable))
    (set (make-local-variable 'epoxide-node-name) node-name)
    (set (make-local-variable 'epoxide-node-class) node-class)
    (set (make-local-variable 'epoxide-root-buffer) root-buffer)
    (widget-setup)
    (use-local-map epoxide-parameter-edit-map)
    (goto-char (point-min))
    (widget-forward 3)))

(defun epoxide-variable-show (variable node-name node-class root-buffer)
  "Show values of a VARIABLE of a node called NODE-NAME having class NODE-CLASS.
Node's root buffer is ROOT-BUFFER."
  (epoxide-kill-buffer "*variable*")
  (switch-to-buffer (get-buffer-create "*variable*"))
  (let ((inhibit-read-only t)
	(doc (epoxide-tsg-eldoc-get-doc-string node-class
					    (symbol-name (car variable))))
	values)
    (erase-buffer)
    (remove-overlays)
    (widget-insert "Value of variable '" (symbol-name (car variable))
		   "' of node '" node-name ":" node-class
		   "'\nthat originates from " (if (bufferp root-buffer)
						  (buffer-name root-buffer)
						root-buffer)
		   " is:\n\n")
    (when (string-match "config-list" (symbol-name (car variable)))
      (widget-insert
       "Note: dynamic configuration arguments take priority over static ones."
       "\n\n"))
    (if (null (cdr variable))
	(widget-insert "Not set.\n")
      (if (not (listp (cdr variable)))
	  (insert (format "%s" (cdr variable)))
	;; When variable's value is a list, list those values and their
	;; index in the list.
        (epoxide-enumerate (i x (cdr variable))
          (widget-insert "#" (number-to-string i) ": " (format "%s" x)
			 "\n"))))
    ;; List documentation of each parameter.
    (when doc
      (widget-insert "\nWhere:\n")
      (epoxide-enumerate (i x doc)
        (widget-insert "#" (number-to-string i) ": " x "\n")))
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-buffer))
		   "Close")
    (widget-setup)
    (use-local-map widget-keymap)
    (goto-char (1+ (line-beginning-position)))))

(defun epoxide-edit-output-variable (node-name node-class root-buffer)
  "Show a list of available outputs for the current node.
Node is identified by its NODE-NAME, NODE-CLASS and ROOT-BUFFER, and provide
posssibility to enable/disable them.

The number of listed outputs is the maximum of the documented outputs and the
currently active outputs.

Outputs that are connected to another node as inputs get listed but cannot be
disabled.

Provide option to add a new output buffer."
  (epoxide-kill-buffer "*set outputs*")
  (switch-to-buffer (get-buffer-create "*set outputs*"))
  (let* ((inhibit-read-only t)
	 (doc (epoxide-tsg-eldoc-get-doc-string node-class
					     "epoxide-node-outputs"))
         (nodes (epoxide-get-nodes root-buffer))
	 (node (epoxide-tsg-fetch-node-by-name-and-class
                node-name node-class nodes))
	 (output-count (max (length doc) (length (node-output-buffers node))))
	 values output-buffer-name)
    (erase-buffer)
    (remove-overlays)
    (widget-insert "Outputs of node '" node-name ":" node-class
		   "' that originates from " (if (bufferp root-buffer)
						 (buffer-name root-buffer)
					       root-buffer)
		   " are (enabled\nif checked):\n\n")
    ;; List available outputs.
    (dotimes (i output-count)
      (setq values (cons (widget-create
			  'checkbox (nth i (node-output-buffers node)))
			 values))
      (setq output-buffer-name
	    (epoxide-tsg-assign-name-to-output-buffer node-name i))
      (widget-insert " " output-buffer-name)
      ;; When another node connects to this output, the output cannot be
      ;; disabled.
      (when (dolist (node (epoxide-get-nodes root-buffer))
              (when (member output-buffer-name (node-input-buffers node))
                (return t))
              nil)
	(widget-apply (car values) :deactivate)
	(widget-insert
	 " (referenced by another node, cannot be switched off)"))
      (widget-insert "\n"))
    (setq values (nreverse values))
    ;; Insert documentation for each field.
    (when doc
      (widget-insert "\nWhere:\n")
      (epoxide-enumerate (i x doc)
        (widget-insert "#" (number-to-string i) ": " x "\n")))
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (widget &rest ignore)
			       (epoxide-set-outputs))
		   "Apply")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (widget &rest ignore)
			     (when (y-or-n-p (format
					      (concat
					       "Class %s has only %s documented"
					       " arguments. Continue? ")
					      (node-class
					       (widget-get widget ':node))
					      (widget-get widget ':doc-length)))
			     (epoxide-add-output
			      (widget-get widget ':count)
			      (widget-get widget ':node))
			     (epoxide-edit-output-variable
			      (node-name (widget-get widget ':node))
			      (node-class (widget-get widget ':node))
			      (widget-get widget ':root-buffer))))
		   :node node
		   :count (1+ output-count)
		   :root-buffer root-buffer
		   :doc-length (length doc)
		   "Add output")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-buffer))
		   "Cancel")
    (set (make-local-variable 'epoxide-values) values)
    (set (make-local-variable 'epoxide-node-name) node-name)
    (set (make-local-variable 'epoxide-node-class) node-class)
    (set (make-local-variable 'epoxide-root-buffer) root-buffer)
    (widget-setup)
    (use-local-map epoxide-parameter-edit-map)
    (goto-char (point-min))
    (widget-forward 1)))

(defun epoxide-add-output (count node)
  "Add new output buffer in position COUNT to NODE.

When COUNT is greater then the current number of outputs add nils
to the current output buffer list until its length is one smaller
than COUNT.  Then create a new name for an output buffer and add
that to the output buffer list of NODE.  The new buffer is also
created."
  (when (> count (length (node-output-buffers node)))
    (let ((node-buffer (epoxide-tsg-create-node-buffer-name
		      (node-name node)
		      (node-class node)))
	  (root-buffer epoxide-root-buffer)
	  (new-outputs (nreverse (node-output-buffers node)))
	  (new-buffer (epoxide-tsg-assign-name-to-output-buffer (node-name node)
							     (1- count))))
      (setq new-outputs
	    (append (make-list (- count (1+ (length new-outputs))) nil)
		    new-outputs))
      (setq new-outputs
	    (cons new-buffer
		  new-outputs))
      (setf (node-output-buffers node) (nreverse new-outputs))
      (epoxide-tsg-check-buffer-create
       (epoxide-tsg-assign-name-to-output-buffer (node-name node) (1- count))
       root-buffer)
      (epoxide-tsg-set-output new-buffer root-buffer node-buffer))))

(defun epoxide-set-outputs ()
  "Set outputs of a node."
  (let* ((values (mapcar 'widget-value epoxide-values))
	 (node-name epoxide-node-name)
	 (node-class epoxide-node-class)
	 (root-buffer epoxide-root-buffer)
	 (node-buffer (epoxide-tsg-create-node-buffer-name
		       node-name
		       node-class))
	 node outputs output-buffers)
    (with-current-buffer root-buffer
      ;; Kill those output buffers that are disabled and assign names to those
      ;; that are enabled
      (epoxide-enumerate (i value values)
	(if (not value)
	    (epoxide-kill-buffer
	     (epoxide-tsg-assign-name-to-output-buffer node-name i))
	  (epoxide-list-setq outputs i
                             (epoxide-tsg-assign-name-to-output-buffer
                              node-name i))))
      (setq node (epoxide-tsg-fetch-node-by-name-and-class
		  node-name node-class epoxide-tsg-node-list))
      ;; Set outputs of the node in the node list.
      (setf (node-output-buffers node) outputs)
      ;; Create buffers.
      (with-current-buffer node-buffer
      	  (epoxide-tsg-set-node node epoxide-tsg-root-buffer))
      ;; Set buffer local variables in the output buffer.
      (dolist (output (delq nil (mapcar 'identity outputs)))
	(get-buffer-create output)
	(epoxide-tsg-set-output output root-buffer node-buffer))
      ;; Show the changes.
      (with-current-buffer node-buffer
	(epoxide-variables-list))
      (epoxide-kill-buffer "*set outputs*"))))

(defun epoxide-collect-values ()
  "Collect all the values contained in editable fields in the current buffer."
  (save-excursion
    (let (start-pos values)
      (goto-char (point-min))
      (widget-forward 1)
      (setq start-pos (1+ (point)))
      (widget-forward 1)
      (while (> (point) start-pos)
	(when (equal (widget-type (widget-at))
		     'epoxide-editable-field)
	  (setq values
		(cons (widget-value (widget-at))
		      values)))
	(widget-forward 1))
      (nreverse values))))

(defun epoxide-parameter-help-action (widget &optional event)
  "Show a list of possible options for filling in the currently active field.
Works in a buffer local variable editing form.  Arguments WIDGET
and EVENT are ignored they are there only for compatibilty
reasons."
  (epoxide-parameter-help))

(defun epoxide-parameter-help ()
  "Show a list of possible options for filling in the currently active field.
Works in a buffer local variable editing form.

For the list of options the appropriate function of the node's definition is
consulted."
  (interactive)
  (let ((kind (symbol-name epoxide-variable))
	(func (concat "epoxide-" (downcase epoxide-node-class) "-"))
	(index 0)
	match options pos value)
    ;; Assemble the name of the function that returns the list of options.
    (cond
     ((string-match "config" kind)
      (setq func (concat func "config-info"))
      (setq match t))
     ((string-match "output" kind)
      (setq func (concat func "output-info"))
      (setq match t))
     ((string-match "input" kind)
      (setq func (concat func "input-info"))
      (setq match t)))
    (condition-case nil
	(progn
	  (when (and (symbol-function (intern-soft func))
		     match)
	    ;; Get options for all the fields.
	    (setq options (epoxide-tsg-eldoc-helper func))
	    ;; Get number of currently selected field.
	    (save-excursion
	      (setq pos (line-beginning-position))
	      (goto-char (point-min))
	      (while (< (point) pos)
		(when (equal (widget-type (widget-at (point)))
			     'epoxide-editable-field)
		  (incf index))
		(widget-forward 1)))
	    ;; Show possible options for the selected field.
	    (cond
	     ((null (nth index options))
	      (message "No help is available for this field."))
	     ((listp (nth index options))
	      (setq value (ido-completing-read "Help: " (nth index options))))
	     ((functionp (nth index options))
	      (setq value (ido-completing-read
			   "Help: " (funcall (nth index options)))))
	     ((stringp (nth index options))
	      (setq value (ido-completing-read "Help: "
					       (list (nth index options)))))
	     (t
	      (message "No help is available for this field.")))
	    ;; If an option is chosen, change to old value to the new.
	    (when value
	      (delete-region (line-beginning-position) (line-end-position))
	      (insert value))))
      (error nil))))

(defun epoxide-parameter-edit-mode-apply ()
  "Apply parameter change.
Make the necessary changes in the buffer that holds the .tsg
file, but don't save them."
  (interactive)
  (if (equal (buffer-name) "*set outputs*")
      (epoxide-set-outputs)
    (epoxide-variable-set)))

(defun epoxide-parameter-edit-mode-save ()
  "Save parameter change to .tsg file."
  (interactive)
  (if (equal (buffer-name) "*set outputs*")
      (epoxide-set-outputs)
    (epoxide-variable-set t)))

(defun epoxide-variable-validate (node-class variable-name values)
  "Check whether values of current variable are authorized.

Load NODE-CLASS and VARIABLE-NAME specific validator functions
from the node definition file and check each element of VALUES
whether it satisfies the requirements defined by the appropriate
function.

When more argument is supplied than is documented, a notification
is given."
  (let* ((class-file (downcase node-class))
	 (part-1 (concat "epoxide-" class-file "-"))
	 (part-2 (cond
		 ((string-match "config-list" variable-name)
		  "config-info")
		 ((string-match "inputs" variable-name)
		  "input-info")))
	 (func (concat part-1 part-2))
	 (dont-care (autoload (intern func) class-file))
	 (validators (epoxide-tsg-eldoc-validator func)))
    (unless (<= (length values) (length validators))
      (unless (y-or-n-p
	       (format "Class %s has only %s documented arguments; Continue? "
		       node-class (length validators)))
	(error "Cancelled by the user")))
    (epoxide-enumerate (i value values)
      (when (nth i validators)
	(unless (funcall (nth i validators) value)
	  (error "Parameter #%s cannot be set to '%s'" i value))))))

(defun epoxide-variable-set (&optional save-to-file)
  "Set the config or input parameters of the node.
Works when called in a `epoxide-mode' buffer local variable editing
form.  When optional argument SAVE-TO-FILE is non-nil the
corresponding .tsg file is saved."
  (let* ((variable epoxide-variable)
	 (node-name epoxide-node-name)
	 (node-class epoxide-node-class)
	 (root-buffer epoxide-root-buffer)
	 (value (epoxide-collect-values))
	 (node (epoxide-tsg-fetch-node-by-name-and-class
                node-name node-class (epoxide-get-nodes root-buffer))))
    (epoxide-variable-validate node-class (symbol-name variable) value)
    (with-current-buffer root-buffer
      (cond
       ((string-match "config-list" (symbol-name variable))
	;; Set new value in node list.
	(setf (node-config-list node) value)
	;; Set new value .tsg file.
	;; TODO: should it write to the buffer and not save,
	;;       or not write at all?
	(epoxide-tsg-set-config-in-tsg-file node-name node-class value
					 save-to-file))
       ((string-match "inputs" (symbol-name variable))
        (if (> (length (node-input-buffers node)) (length value))
            (error "Cannot remove input")
          (epoxide-enumerate (i input (node-input-buffers node))
            (unless (equal input (nth i value))
              (error "Cannot modify previously set input")))
          ;; Set new value .tsg file.
          (epoxide-tsg-set-inputs-in-tsg-file node root-buffer value
                                           save-to-file)
          ;; Set new value in node list.
          (setf (node-input-buffers node) value)))))
    ;; Set new value in node's buffer.
    (with-current-buffer
	(epoxide-tsg-create-node-buffer-name node-name node-class)
      (epoxide-tsg-set-node node root-buffer)
      (epoxide-call-config-change-handler node-name node-class)
      (epoxide-kill-buffer "*Help*")
      (epoxide-variables-list)
      (epoxide-kill-buffer "*set value*")
      (if save-to-file
	  (message "Variable '%s' has been saved." variable)
	(message "Variable '%s' has been set." variable)))))

(defun epoxide-call-config-change-handler (node-name node-class)
  "Call node's config change handler if it exists.
Node is identified by NODE-NAME and NODE-CLASS."
  (with-current-buffer
      (epoxide-tsg-create-node-buffer-name node-name node-class)
    (let* ((f-name (concat "epoxide-" (downcase node-class)
			   "-config-change-handler"))
	   (handler (intern f-name)))
      (when (fboundp handler)
	(funcall handler)))))

(defun epoxide-tsg-set-config-in-tsg-file (node-name node-class values
						  &optional save)
  "Find node definition is .tsg file if and change its config.

NODE-NAME: name of the node the change is to be applied on.
NODE-CLASS: class of the node the change is to be applied on.
VALUES: a list that holds the new configuration arguments.
When optional argument SAVE is non-nil, the change is save to the file."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward (concat node-name " :: " node-class) nil t)
	(epoxide-tsg-set-config-in-tsg-file-helper
	 node-name node-class values save)
      ;; Node may have an automatically generated name. Try to find
      ;; such.
      (let (found break)
	(while (and (null found)
		    (null break))
	  (if (null (search-forward node-class nil t))
	      (setq break t)
	    ;; Do not count this match if it is a comment.
	    (unless (equal (get-text-property (point) 'face)
			   'font-lock-comment-face)
	      ;; Find the best match for this node by checking the
	      ;; characters that could precede the node class.
	      (let* ((pos (point))
		     (s-pos
		      (apply 'max
			     `(,(epoxide-tsg-set-config-check-string "::")
			       ,(epoxide-tsg-set-config-check-string ";")
			       ,(epoxide-tsg-set-config-check-string "->")
			       ,(epoxide-tsg-set-config-check-string "]"))))
		     text tmp)
		(setq text (buffer-substring (point) s-pos))
		;; Remove text that appears as comment.
		(with-temp-buffer
		  (insert text)
		  (goto-char (point-min))
		  (while (< (point) (point-max))
		    (if (equal (get-text-property (point) 'face)
			       'font-lock-comment-face)
			(delete-char 1)
		      (forward-char)))
		  (setq text (buffer-substring-no-properties
			      (point-min) (point-max))))
		(when (equal node-class (epoxide-chomp text))
		  (setq found t)
		  (goto-char pos)
		  (setq break t))
		(goto-char pos)))))
	(if found
	    (epoxide-tsg-set-config-in-tsg-file-helper
	     node-name node-class values save)
	  (error "Cannot set config for node '%s:%s': node name not found"
		 node-name node-class))))))

(defun epoxide-tsg-set-config-check-string (string)
  "Find the nearest appearance of STRING before point and return its position.

Return `point-min' if STRING cannot be found."
  (let ((pos (point))
	ret)
    (while (null ret)
      (if (search-backward string nil t)
	  (when (not (equal (get-text-property (point) 'face)
			    'font-lock-comment-face))
	    (setq ret (+ (point) (length string))))
	(unless ret
	  (setq ret (point-min)))))
    (goto-char pos)
    ret))

(defun epoxide-tsg-set-config-in-tsg-file-helper (node-name node-class values
							    &optional save)
  "Insert new config of a node to .tsg file.

NODE-NAME: name of the node the change is to be applied on.
NODE-CLASS: class of the node the change is to be applied on.
VALUES: a list that holds the new configuration arguments.
When optional argument SAVE is non-nil, the change is save to the file."
  (skip-chars-forward "^;([-")
  (cond
   ((member (following-char) '(?\; ?\[ ?-))
    (insert "(" (mapconcat 'identity values ", ") ")"))
   ((equal (following-char) ?\()
    (let ((pos (+ (point) 1)))
      (skip-chars-forward "^)")
      (delete-region pos (point))
      (insert (mapconcat 'identity values ", "))))
   (t
    (error "Cannot set config for node '%s:%s': parse error"
	   node-name node-class)))
  (when save
    (save-buffer)))

(defun epoxide-tsg-set-inputs-in-tsg-file (node root-buffer value
						&optional save)
  "Insert new connections to the .tsg file.

NODE: node on the receiving end of the connection.
ROOT-BUFFER: buffer that shows the .tsg file.
VALUE: a list that holds the name of those buffers that should be set as inputs
to NODE.
When optional argument SAVE is non-nil, the change is save to the file."
  (let ((i (length (node-input-buffers node)))
	other-node output)
    (with-current-buffer root-buffer
      (save-excursion
	(goto-char (point-max))
	(insert "\n")
	(while (< i (length value))
	  (with-current-buffer (nth i value)
	    (with-current-buffer epoxide-root-buffer
	      ;; Get name of the input node and the number of its output buffer.
	      (setq other-node epoxide-node-name)
	      (setq output
		    (- (length epoxide-node-outputs)
		       (length (member (nth i value)
				       epoxide-node-outputs))))))
	  ;; Insert a single connection.
	  (insert other-node "[" (number-to-string output) "] -> ["
		  (number-to-string i) "]" (node-name node) ";\n")
	  (incf i)))
      (when save
	(save-buffer)))))

(defun epoxide-tsg-select-epoxide-variables (variables)
  "Return those items of VARIABLES that have 'epoxide' in their symbol names."
  (delq nil (map 'list (lambda (variable)
	       (if (string-match "epo" (symbol-name (car variable)))
		   variable
		 nil))
       variables)))

(defun epoxide-tsg-fetch-node-by-name-and-class (name class nodes)
  "Return the first node having name NAME and class CLASS from NODES."
  (dolist (head nodes)
    (when (and (equal (node-name head) name)
	       (equal (node-class head) class))
      (return head))))


;; --------------    Interacting with Ibuffer    -------------------------------

(defun epoxide-ibuffer-next-nodes-group (name class nodes)
  "Create an Ibuffer group from the succeeding nodes of the current node.
Current node is specified with its NAME and CLASS.  The list NODES
specifies the list in which to look for."
  (epoxide-ibuffer-build-filter-list (concat "next nodes of " name ":" class)
				 (epoxide-ibuffer-get-next-nodes
				  name class nodes)))

(defun epoxide-ibuffer-get-next-nodes (name class nodes)
  "Return the successive nodes of the current node from.
Current noode is specified with its NAME and CLASS.  The list
NODES specifies the list in which to look for."
  (let ((current-node (epoxide-tsg-fetch-node-by-name-and-class name class
								nodes))
	next-nodes)
    ;; Needed nodes are those that have the outputs of the current node
    ;; as their inputs.
    (dolist (output (node-output-buffers current-node))
      (dolist (node nodes)
	(when (member output (node-input-buffers node))
	  (setq next-nodes (cons (epoxide-tsg-create-node-buffer-name
				  (node-name node) (node-class node))
				 next-nodes)))))
    next-nodes))

(defun epoxide-ibuffer-prev-nodes-group (name class nodes)
  "Create an Ibuffer group of nodes that precede the current node.
Current noode is specified with its NAME and CLASS.  The list
NODES specifies the list in which to look for."
    (epoxide-ibuffer-build-filter-list (concat "preceeding nodes of "
					   name ":" class)
				   (epoxide-ibuffer-get-previous-nodes
				    name class nodes)))

(defun epoxide-ibuffer-get-previous-nodes (name class nodes)
  "Return the preceding nodes of the current node.
Current noode is specified with its NAME and CLASS.  The list
NODES specifies the list in which to look for."
  (let ((current-node (epoxide-tsg-fetch-node-by-name-and-class name class
								nodes))
	previous-nodes)
    ;; Needed nodes are those that have the inputs of the current node
    ;; as their outputs.
    (dolist (input (node-input-buffers current-node))
      (dolist (node nodes)
	(when (member input (node-output-buffers node))
	    (setq previous-nodes (cons (epoxide-tsg-create-node-buffer-name
					(node-name node) (node-class node))
		  previous-nodes)))))
    previous-nodes))

(defun epoxide-ibuffer-current-links-group (group name class list)
  "Create an Ibuffer group.
GROUP, NAME and CLASS are used for naming the group.  LIST
contains the items that should appear in the group."
  (epoxide-ibuffer-build-filter-list (concat group "s of " name ":" class)
				 (delq nil list)))

(defun epoxide-ibuffer-nodes-group (buffer nodes)
  "Create an Ibuffer group to list the nodes parsed from the same to .tsg file.
BUFFER is the buffer where the .tsg file is shown.  NODES is a list that store
the nodes."
  (let ((buffers (mapcar (lambda (node)
			   (epoxide-tsg-create-node-buffer-name (node-name node)
							     (node-class node)))
			 nodes))
        (name (buffer-name (get-buffer buffer))))
  (epoxide-ibuffer-build-filter-list (concat "nodes of " name) buffers)))

(defun epoxide-ibuffer-links-group (buffer nodes)
  "Create an Ibuffer group to list the outputs parsed from the same .tsg file.
BUFFER is the buffer where the .tsg file is shown.  NODES is a list that store
the nodes."
  ;; Outer mapcar is used for copying the list -- otherwise later sort would
  ;; modify it and that causes the nodes' output list to be overwritten.
  (let ((buffers (mapcar 'identity
			 (delq nil
			       (apply 'append (mapcar 'node-output-buffers
						      nodes)))))
        (name (buffer-name (get-buffer buffer))))
    (epoxide-ibuffer-build-filter-list (concat "link buffers of " name)
				   buffers)))

(defun epoxide-ibuffer-build-filter-list (group names)
  "Create an Ibuffer group named GROUP by 'or'-ing NAMES."
  (cons group (list (append '(or)
	       (mapcar (lambda (name)
		       `(name . ,name))
		       (sort names #'string-lessp))))))

(defun epoxide-ibuffer ()
  "Show Ibuffer grouping of available buffers based on the current buffer.

Listed groups are:
- buffers that precede the current buffer,
- buffers that follow the current buffer,
- input links of the current node,
- output links of the current node,
- other nodes belonging to the same .tsg file,
- other links belonging to the same .tsg file,
- tsg files,
- other nodes,
- other links,
- Emacs related buffers."
  (interactive)
  (let* ((root-buffer (epoxide-get-root-buffer))
         (nodes (epoxide-get-nodes root-buffer))
         (current-node-buffer
          (cond
           ((eq major-mode 'epoxide-link-mode)
            (get-buffer epoxide-src-node))
           ((eq major-mode 'epoxide-mode)
            (current-buffer))
           (t
            nil)))
         (ibuffer-show-empty-filter-groups nil) ;; Hide empty groups.
         name class ibuffer-saved-filter-groups qualifiers)
    (when current-node-buffer
      (with-current-buffer current-node-buffer
        (setq name epoxide-node-name)
        (setq class epoxide-node-class)))
    ;; Generate qualifiers.
    (cond
     ((eq major-mode 'epoxide-link-mode)
      (setq qualifiers
         (append qualifiers
            `(,(epoxide-ibuffer-prev-nodes-of-link-group (buffer-name))
              ,(epoxide-ibuffer-next-nodes-of-link-group (buffer-name)
							 nodes)))))
     (name
      (setq qualifiers
        (append qualifiers
           `(,(epoxide-ibuffer-prev-nodes-group name class nodes)
             ,(epoxide-ibuffer-next-nodes-group name class nodes)
             ,(epoxide-ibuffer-current-links-group
               "input" name class epoxide-node-inputs)
             ,(epoxide-ibuffer-current-links-group
               "output" name class epoxide-node-outputs))))))
    (setq qualifiers
       (append qualifiers
           `(,(epoxide-ibuffer-nodes-group root-buffer nodes)
             ,(epoxide-ibuffer-links-group root-buffer nodes)
             ,epoxide-ibuffer-tsg-group
             ,epoxide-ibuffer-nodes-group
             ,epoxide-ibuffer-links-group
             ,epoxide-ibuffer-emacs-group)))
    (setq ibuffer-saved-filter-groups
          (append ibuffer-saved-filter-groups
                  (list (cons "epoxide" (list qualifiers)))))
    (ibuffer)
    (setq ibuffer-filter-groups qualifiers)
    (ibuffer-update nil t)))

(defun epoxide-ibuffer-prev-nodes-of-link-group (buffer-name)
  "Return an ibuffer group of the prev node of the link named BUFFER-NAME."
  (with-current-buffer buffer-name
    (epoxide-ibuffer-build-filter-list
     (concat "preceeding node of " (buffer-name))
     (list (buffer-name epoxide-src-node)))))

(defun epoxide-ibuffer-next-nodes-of-link-group (output nodes)
  "Create an Ibuffer group of nodes having OUTPUT as their input.
Nodes are looked up in the list NODES."
  (let ((buffers
	 (delq nil
	       (mapcar (lambda (node)
			 (when (member output (node-input-buffers node))
			   (epoxide-tsg-create-node-buffer-name
			    (node-name node)
			    (node-class node))))
		       nodes))))
    (epoxide-ibuffer-build-filter-list (concat "next nodes of " output)
				   buffers)))


;; --------------    Handling jumping from one node's    -----------------------
;; ----------------------    buffer to the next --------------------------------

(defun epoxide-switch-to-root-buffer ()
  "Switch to the root buffer (.tsg file) of the current buffer."
  (interactive)
  (switch-to-buffer epoxide-root-buffer))

(defun epoxide-show-all-outputs ()
  "Show every output buffer of the current node.

Currently open buffers stay displayed and current windows are
split in order to fit as many buffers as possible.  When frame
size is too small to accomodate more windows, a notification is
written to the messages buffer."
  (interactive)
  (let* ((buffers (cons (current-buffer) (copy-sequence epoxide-node-outputs))))
    (epoxide-display-buffers buffers)))

(defun epoxide-jump-to-preceding-node ()
    "Jump to the preceeding buffer from the current node buffer.

When custom variable `epoxide-jump-to-buffer-type' is `node' the
preceding buffer is going to be a node buffer.  When
`epoxide-jump-to-buffer-type' is `link', one of the input buffers
of the current node is shown."
  (interactive)
  (epoxide-jump-to-next-node t))

(defun epoxide-jump-to-next-node (&optional prev)
  "Jump to the next buffer from the current node buffer.

When custom variable `epoxide-jump-to-buffer-type' is `node' the
next buffer is going to be a node buffer.  When
`epoxide-jump-to-buffer-type' is `link', one of the output
buffers of the current node is shown.  Jump to previous node
instead if PREV is t."
  (interactive)
  (if (eq major-mode 'epoxide-link-mode)
      (epoxide-link-jump-to-adjacent-node (not prev))
    (let ((direction (if prev "input" "output"))
          (neighbors (if prev epoxide-node-inputs epoxide-node-outputs)))
      (cond
       ((eq epoxide-jump-to-buffer-type 'node)
        (epoxide-jump-to-adjacent-node (not prev)))
       ((eq epoxide-jump-to-buffer-type 'link)
        (epoxide-jump-to-in-or-output neighbors direction))))))

(defun epoxide-jump-to-adjacent-node (&optional next-node)
    "Jump to one of the adjacent nodes in the chain from current node buffer.
If optional NEXT-NODE is t, jump to next node, else jump to preceding node."
  (let ((buffer-name (epoxide-get-adjacent-node next-node)))
    (when buffer-name
      (switch-to-buffer buffer-name))))

(defun epoxide-get-adjacent-node (&optional next-node)
  "Return one of the adjacent nodes in the chain from the current node buffer.
If optional NEXT-NODE is t, return next node, else return
preceding node.

If only one adjacent node is located return the name of its
buffer, if more than one is found, the available options are
offered using ido.  If none is found a notification is given and
nil is returned."
  (let ((nodes (epoxide-get-adjacent-nodes-of-node next-node))
	(type (if next-node
		  "following"
		"preceding")))
    (cond
     ((null nodes)
      (message "There's no node %s node '%s:%s'." type epoxide-node-name
	       epoxide-node-class)
      nil)
     ((equal (length nodes) 1)
      (car nodes))
     (t
      (ido-completing-read
       (concat "Possible " type " nodes are: ") (delete-dups nodes))))))

(defun epoxide-get-adjacent-nodes-of-node (&optional next-node)
  "Return adjacent nodes of the current node.
If optional argument NEXT-NODE is non-nil return next nodes,
otherwise return previous nodes."
  (if next-node
      (epoxide-ibuffer-get-next-nodes
       epoxide-node-name epoxide-node-class
       (epoxide-get-nodes epoxide-root-buffer))
    (epoxide-ibuffer-get-previous-nodes
     epoxide-node-name epoxide-node-class
     (epoxide-get-nodes epoxide-root-buffer))))

(defun epoxide-jump-to-in-or-output (buffers type)
  "Switch to a buffer in BUFFERS.
TYPE should be either input or output."
  (let ((buffer (epoxide-get-in-or-output buffers type)))
    (when buffer
      (switch-to-buffer buffer))))

(defun epoxide-get-in-or-output (buffers type)
  "Return a buffer from BUFFERS.

When BUFFERS is empty a notification is shown and nil is
returned.  When BUFFERS has only one element return that as a
list.  When BUFFERS has more than one element available options
are shown by ido.

TYPE is used when displaying notifications, its role is to
distinguish between input or output buffers."
  (cond
   ((null buffers)
    (message "There's no %s for node '%s:%s'." type epoxide-node-name
	     epoxide-node-class)
    nil)
   ((equal (length buffers) 1)
    (car buffers))
   (t
    (ido-completing-read
     (concat "Possible " type " buffers are: ") (delete-dups buffers)))))

(defun epoxide-link-jump-to-adjacent-node (&optional next-node)
  "Jump to one of the adjacent nodes in the chain.
Works when initiating jump from a link buffer.  If optional
argument NEXT-NODE is t, jump to next node, else jump to
preceding node."
  (let ((buffer-name (epoxide-link-get-adjacent-node next-node)))
    (when buffer-name
      (switch-to-buffer buffer-name))))

(defun epoxide-link-get-adjacent-node (&optional next-node)
  "Return one of the adjacent nodes in the chain.
Works when inititating from a link bufer.  If optional NEXT-NODE
is t, return next node, else return preceding node.

If only one adjacent node is located return that buffer name, if
more than one is found, the available options are offered using
ido.  If none is found, a notification is given and nil is
returned."
  (let ((buffer (buffer-name))
	(type-1 (if next-node
		    "input"
		  "output"))
	(type-2 (if next-node
		    "following"
		  "preceding"))
	(nodes (epoxide-get-nodes epoxide-root-buffer)))
    (setq nodes (epoxide-get-adjacent-nodes-of-output buffer nodes next-node))
    (cond
     ((null nodes)
      (message "There are no nodes having '%s' as %s." buffer type-1)
      nil)
     ((equal (length nodes) 1)
      (car nodes))
     (t
      (ido-completing-read
       (concat "Possible " type-2 " nodes are: ") (delete-dups nodes))))))

(defun epoxide-get-adjacent-nodes-of-output (buffer nodes &optional next-node)
  "Return adjacent nodes of output BUFFER from NODES.
If optional argument NEXT-NODE is non-nil return next nodes,
otherwise return previous nodes."
  (delq nil
	(if next-node
	    (mapcar (lambda (node)
		      (when (member buffer (node-input-buffers node))
			(epoxide-tsg-create-node-buffer-name (node-name node)
							  (node-class node))))
		    nodes)
	  (list epoxide-src-node))))

(defun epoxide-get-adjacent-nodes (&optional next-node)
  "Return adjacent nodes of the current output or node.

If optional argument NEXT-NODE is non-nil return the list of next
nodes otherwise return previous nodes."
  (sort (cond
	 ((equal major-mode 'epoxide-mode)
	  (cond
	   ((eq epoxide-jump-to-buffer-type 'node)
	    (epoxide-get-adjacent-nodes-of-node next-node))
	   ((eq epoxide-jump-to-buffer-type 'link)
	    (if next-node
		(mapcar 'identity epoxide-node-outputs)
	      (mapcar 'identity epoxide-node-inputs)))))
	 ((equal major-mode 'epoxide-link-mode)
	  (let ((nodes (epoxide-get-nodes epoxide-root-buffer)))
            (epoxide-get-adjacent-nodes-of-output
             (buffer-name) nodes next-node))))
	#'string-lessp))

(defun epoxide-backward-buffer (&optional n)
    "Backwards one buffer in the chain.

When in an output buffer jump to the previous node buffer.  The
previous node buffer is chosen by lexically sorting those node
buffers that have the current buffer as output.  Form these
buffers that one is chosen that has the highest index less than
or equal to N, if N is non-negative.  If N is negative the
selected index is calculated by substracting N from the highest
index.  In this case the smallest selected index is 0. Optional
argument N defaults to 0.  When in a node buffer the selection is
similar.  The previous buffer is determined by N and the value of
`epoxide-jump-to-buffer-type'.  If that is 'node the previous
buffer should be a node buffer, if it is 'output than an output
buffer."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((nodes (epoxide-get-adjacent-nodes)))
    (epoxide-switch-to-nth-buffer n nodes)))

(defun epoxide-forward-buffer (&optional n)
  "Forwards one buffer in the chain.

When in an output buffer jump to the next node buffer.  The next
node buffer is chosen by lexically sorting those node buffers
that have the current bufferas input.  Form these buffers that
one is chosen that has the highest index less than equal or to N,
if N is non-negative.  If N is negative the selected index is
calculated by substracting N from the highest index.  In this case
the smallest selected index is 0. Optional argument N defaults to
0.  When in a node buffer the selection is similar.  The next
buffer is determined by N and the value of
`epoxide-jump-to-buffer-type'.  If that is 'node the next buffer
should be a node buffer, if it is 'link than an output buffer."
  (interactive "p")
  (when (null n)
    (setq n 0))
  (let ((nodes (epoxide-get-adjacent-nodes t)))
    (epoxide-switch-to-nth-buffer n nodes)))

(defun epoxide-switch-to-nth-buffer (n buffers)
  "Switch to Nth buffer of BUFFERS.

If n is not negative switch to Nth of BUFFERS.  If N is greater
than the highest index BUFFERS can have, switch to the buffer
with the highest index.  If n is negative switch to that buffer
of BUFFERS which index is the maximum index minus N, but at least
0."
  (when buffers
    (if (>= n 0)
	(cond
	 ((>= (length buffers) n)
	  (switch-to-buffer (nth (1- n) buffers)))
	 ((< (length buffers) n)
	  (switch-to-buffer (nth (1- (length buffers)) buffers))))
      (cond
       ((>= (length buffers) n)
	(switch-to-buffer (nth (1- n) buffers)))
       ((< (- (1- (length buffers)) n) 0)
	(switch-to-buffer (nth 0 buffers)))
       (t
	(switch-to-buffer (nth (- (1- (length buffers)) n) buffers)))))))


;;; Draw troubleshooting graph with COGRE

(defcustom epoxide-cogre-enable-unicode t
  "When t COGRE graphs are drawn using unicode characters.
\(The resulting graph will look 'smoother'.)"
  :type 'boolean
  :group 'epoxide)

(defcustom epoxide-tsg-visualizer-show-links nil
  "When t link buffers are shown on the troubleshooting graph."
  :type 'boolean
  :group 'epoxide)

(defcustom epoxide-tsg-visualizer-naming 'full
  "Default setting of how names should appear on the troubleshooting graph.
`full'      : the whole buffer name is dispalyed.
`shortened' : only name appears in case of nodes, node name and output number
appears in case of links."
  :type '(radio
	  (const full)
	  (const shortened))
  :group 'epoxide)

(defclass cogre-small-arrow (cogre-link)
  ((end-glyph :initform [("^") ("v") ("<") (">")]))
  "This type of link is an even simpler arrow.")

(defvar epoxide-tsg-visualizer-buffer nil
  "Buffer where the troubleshooting graph is shown.")

(defvar epoxide-tsg-visualizer-links-shown nil
  "Whether the current TSG shows link buffers.
See `epoxide-tsg-visualizer-show-links'.")

(defvar epoxide-tsg-visualizer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'previous-buffer)
    (define-key map (kbd "C-x C-b") 'epoxide-tsg-visualizer-ibuffer)
    (define-key map (kbd "RET") 'epoxide-tsg-visualizer-ret)
    (define-key map (kbd "<down-mouse-1>")
      (lambda (event)
	(interactive "e")
	(epoxide-tsg-visualizer-down-mouse-1-click event)))
    (define-key map (kbd "n") 'epoxide-tsg-visualizer-next-node)
    (define-key map (kbd "p") 'epoxide-tsg-visualizer-previous-node)
    (define-key map (kbd "m") 'epoxide-tsg-visualizer-mark-element)
    (define-key map (kbd "c") 'epoxide-tsg-visualizer-change-names)
    (define-key map (kbd "l") 'epoxide-tsg-visualizer-toggle-showing-links)
    (define-key map (kbd "e") 'epoxide-switch-to-root-buffer)
    ;; Consistency with epoxide-mode
    (define-key map (kbd "C-x n") 'epoxide-tsg-visualizer-next-node)
    (define-key map (kbd "C-x p") 'epoxide-tsg-visualizer-previous-node)
    (define-key map (kbd "M-g e") 'epoxide-switch-to-root-buffer)
    (define-key map (kbd "C-c C-k") 'epoxide-tsg-kill-all)
    (define-key map (kbd "M-g V") 'epoxide-view-show-views)
    map)
  "Keymap for `epoxide-tsg-visualizer-mode'.")

(define-derived-mode epoxide-tsg-visualizer-mode cogre-mode
  "Mode for displaying a troubleshooting graph.
Key definitions:
\\{epoxide-tsg-visualizer-mode-map}"
  :group 'epoxide
  (let ((vars '(epoxide-nodes epoxide-root-buffer epoxide-ibuffer-origin-buffer
		epoxide-tsg-visualizer-new-layout-requested-p)))
    (dolist (var vars)
      (set (make-local-variable var) nil))
    (setq-local epoxide-tsg-visualizer-links-shown
		epoxide-tsg-visualizer-show-links)
    (setq-local kill-buffer-hook (cons 'epoxide-tsg-kill kill-buffer-hook))
    (setq mode-name "Epoxide troubleshooting graph")))

(defun epoxide-tsg-visualizer-ret ()
  "Handles the RET key event in `epoxide-tsg-visualizer-mode'."
  (interactive)
  (save-excursion
    (when (button-at (point))
      (button-activate (button-at (point))))))

(defun epoxide-tsg-visualizer-down-mouse-1-click (event)
  "Handles the EVENT down mouse 1 in `epoxide-cogre-mode'.
If a button is clicked, the properties window gets refreshed.  If
something else is clicked, provides the same functionality as
`cogre-mode'."
  (let ((posn (elt event 1)))
    (select-window (posn-window posn))
    (with-selected-window (posn-window posn)
      (goto-char (posn-point posn))
      (if (button-at (point))
	  (button-activate (button-at (point)))
	(cogre-down-mouse-1 event)
	(epoxide-tsg-visualizer-mark-node (buffer-name (other-buffer)))))))

(defun epoxide-tsg-visualizer-ibuffer ()
  "Show ibuffer when in a cogre buffer."
  (interactive)
  (with-current-buffer epoxide-ibuffer-origin-buffer
    (epoxide-ibuffer)))

(defun epoxide-tsg-visualizer-show ()
  "Show the visual representation of the current troubleshooting graph.
Switch to the window showing the graph and update if neccesary."
  (interactive)
  (let* ((root-buffer (epoxide-get-root-buffer))
         (buffer-name (buffer-name (current-buffer)))
         (nodes (epoxide-get-nodes root-buffer)))
    (if (buffer-live-p epoxide-tsg-visualizer-buffer)
	(with-current-buffer epoxide-tsg-visualizer-buffer
	  (if (or (not (equal nodes epoxide-nodes))
		  (not
		   (equal epoxide-tsg-visualizer-links-shown
			  epoxide-tsg-visualizer-show-links)))
	      (epoxide-tsg-visualizer-show-helper root-buffer)
	    (switch-to-buffer epoxide-tsg-visualizer-buffer)
	    ;; Mark current node.
	    (epoxide-tsg-visualizer-mark-node buffer-name)))
      (epoxide-tsg-visualizer-show-helper root-buffer))))

(defun epoxide-tsg-visualizer-show-helper (root-buffer)
  "Draw troubleshooting graph using COGRE.
Parse the graph from .tsg file located in ROOT-BUFFER."
  (let* ((buffer-name (buffer-name (current-buffer)))
	 (root-name (buffer-name (get-buffer root-buffer)))
	 (nodes (epoxide-get-nodes root-buffer))
	 (pos (vector 1 1))
	 (arrow-type (if epoxide-cogre-enable-unicode
			 cogre-arrow
		       cogre-small-arrow))
	 (dot-file-buffer-name (epoxide-tsg-visualizer-switch-to-tsg-buffer
				root-name))
	 (cogre-old-nodes (if cogre-graph (epoxide-cogre-get-nodes)))
	 (cogre-old-links (if cogre-graph (epoxide-cogre-get-links)))
	 obj1 obj2 cogre-nodes cogre-links node-name)
    (setq-local epoxide-tsg-visualizer-links-shown
		epoxide-tsg-visualizer-show-links)
    (setq epoxide-tsg-visualizer-new-layout-requested-p nil)
    ;; First create cogre nodes for all nodes and their outputs if requested
    ;; and connect nodes with their outputs.
    (dolist (node nodes)
      ;; Create cogre node for troubleshooting graph node and store it.
      (setq obj1 (epoxide-cogre-new-node (epoxide-tsg-visualizer-get-node-name
					  (node-name node)
					  (node-class node))
					 pos cogre-old-nodes))
      (setq cogre-nodes (cons obj1 cogre-nodes))
      ;; Create cogre nodes for output buffers.
      (when epoxide-tsg-visualizer-show-links
	;; Collect output buffers to obj2.
	(setq obj2 nil)
	(dolist (output (node-output-buffers node) obj2)
	  (setq obj2 (cons (epoxide-cogre-new-node
			    (epoxide-tsg-visualizer-get-node-name-by-buffer-name
			     output)
			    pos cogre-old-nodes)
			   obj2)))
	;; Save them.
	(setq cogre-nodes (append obj2 cogre-nodes))
	;; Connect node with its outputs on the graph.
	(dolist (obj (delete nil obj2))
	  (setq cogre-links (append
			     (epoxide-cogre-new-link obj1 obj arrow-type
						     cogre-old-links)
			     cogre-links)))))
    ;; Then create links between output buffer cogre nodes and cogre nodes
    ;; or just between nodes.
    (dolist (node nodes)
      (if epoxide-tsg-visualizer-show-links
	  ;; Connect output cogre nodes with nodes having them as inputs.
	  (dolist (input (node-input-buffers node))
	    ;; Get cogre node that has the name of the current input.
	    (setq obj1 (object-assoc
			(epoxide-tsg-visualizer-get-node-name-by-buffer-name
			 input)
				     :object-name cogre-nodes))
	    ;; Get cogre node representing the current node.
	    (setq obj2 (object-assoc (epoxide-tsg-visualizer-get-node-name
				      (node-name node)
				      (node-class node))
				     :object-name cogre-nodes))
	    ;; When both are found create a link between them.
	    (when (and obj1 obj2)
	      (setq cogre-links (append
				 (epoxide-cogre-new-link obj1 obj2 arrow-type
							 cogre-old-links)
				 cogre-links))))
	;; Connect nodes directly (without having outputs).
	;; Fetch cogre node for NODE.
	(setq obj1 (object-assoc (epoxide-tsg-visualizer-get-node-name
				  (node-name node)
				  (node-class node))
				 :object-name cogre-nodes))
	;; Fetch those nodes that have NODE as inputs.
	(setq obj2 nil)
	(dolist (name (epoxide-ibuffer-get-next-nodes (node-name node)
						  (node-class node)
						  nodes))
	  (setq obj2 (cons (object-assoc
			    (epoxide-tsg-visualizer-get-node-name-by-buffer-name
			     name)
			    :object-name cogre-nodes) obj2)))
	;; Connect these.
	(dolist (obj (delete nil obj2))
	  (setq cogre-links (append
			     (epoxide-cogre-new-link obj1 obj arrow-type
						     cogre-old-links)
			     cogre-links)))))
    (setq cogre-delete-dont-ask t)
    ;; Remove unused links.
    (dolist (link cogre-old-links)
      (unless (member link cogre-links)
	(cogre-delete link)))
    ;; Remove unused nodes.
    (dolist (node cogre-old-nodes)
      (unless (member node cogre-nodes)
	(cogre-delete node)))
    ;; Create new layout using dot.
    (let ((msg (current-message)))
      ;; Silence some cogre messages.
      (epoxide-log "Calling cogre ...")
      (when epoxide-tsg-visualizer-new-layout-requested-p
	(setq cogre-dot-node-position-scale '(4 . 10))
	(cogre-layout))
      (cogre-refresh)
      (epoxide-log "Calling cogre ... done")
      (if msg
	  (message "%s" msg)
	(message nil)))
    ;; Use special keycombinations.
    (unless (equal major-mode 'epoxide-tsg-visualizer-mode)
      (epoxide-tsg-visualizer-mode))
    (setq-local epoxide-nodes (mapcar 'copy-node nodes))
    (setq-local epoxide-root-buffer root-buffer)
    ;; Kill cedet dot buffer.
    (epoxide-kill-buffer "*CEDET graphviz dot*")
    ;; Kill compilation log buffer.
    (epoxide-kill-buffer "*Compile-Log*")
    ;; Kill dot buffer.
    (epoxide-kill-buffer dot-file-buffer-name)
    ;; Mark current node and place buttons on node names.
    (epoxide-tsg-visualizer-mark-node buffer-name)))

(defun epoxide-tsg-visualizer-switch-to-tsg-buffer (root-name)
  "Switch to the buffer displaying the troubleshooting graph.

When buffer is nonexistent, create one.  ROOT-NAME is used for
setting the buffer's name.  Set font in buffer to one that looks
OK.  Return the name of the .dot file buffer."
  (let (dot-file-buffer-name
       (file-name (epoxide-get-value 'epoxide-tsg-visualizer-file root-name)))
    (unless (buffer-live-p epoxide-tsg-visualizer-buffer)
      (setq epoxide-tsg-visualizer-buffer (find-file file-name))
      (setq dot-file-buffer-name
	    (concat
             (file-name-sans-extension
	      (buffer-name epoxide-tsg-visualizer-buffer))
	     ".dot"))
      (setq epoxide-tsg-visualizer-buffer
	    (get-buffer (rename-buffer
			 (epoxide-tsg-visualizer-create-buffer-name
			  root-name))))
      (when epoxide-cogre-enable-unicode
	(cogre-uml-enable-unicode))
      ;; Set font to one that looks OK.
      (let ((display-font "DejaVu Sans Mono"))
	(when (member display-font (font-family-list))
	  (buffer-face-set `(:family ,display-font)))))
    (unless (get-buffer-window epoxide-tsg-visualizer-buffer)
      (switch-to-buffer epoxide-tsg-visualizer-buffer))
    dot-file-buffer-name))

(defun epoxide-tsg-visualizer-create-buffer-name (root-name)
  "Create a name for the buffer that displays the graph.
ROOT-NAME is the name of the .tsg file, the buffer name to be
created will contain it."
  (concat "*troubleshooting graph of " root-name "*"))

(defun epoxide-cogre-get-elements (type)
  "Return those elements of a COGRE graph that are of TYPE."
  (let ((elements (slot-value cogre-graph 'elements))
	items)
    (dolist (element elements items)
      (when (object-of-class-p element type)
	(setq items (cons element items))))))

(defun epoxide-cogre-get-links ()
  "Return links of a COGRE graph."
  (epoxide-cogre-get-elements cogre-link))

(defun epoxide-cogre-get-nodes ()
  "Return nodes of a COGRE graph."
  (epoxide-cogre-get-elements cogre-node))

(defun epoxide-cogre-new-node (name pos &optional nodes)
  "Create a new coge-node.
Node will be named NAME and have position POS if it isn't already among
NODES.  Otherwise fetch the node from NODES.

Set `epoxide-tsg-visualizer-new-layout-requested-p' to t if a new
node is created.  Return the node."
  (let ((obj (object-assoc name :object-name nodes)))
    (unless obj
      (setq epoxide-tsg-visualizer-new-layout-requested-p t)
      (setq obj (cogre-new-node 1 'cogre-node :position pos))
      ;; Set name of node.
      (cogre-set-element-name obj name))
    obj))

(defun epoxide-cogre-new-link (obj-1 obj-2 arrow-type &optional links)
  "Create a new coge-link between OBJ-1 and OBJ-2.
ARROW-TYPE is the type of arrow used when drawing.  If there is
no such links among LINKS, create one, otherwise fetch the
link(s) from LINKS.

Return the link(s) as a list."
  (let* ((l (object-assoc obj-1 :start links))
	 (links-with-start-node (cond
				 ((object-p l)
				  (list l))
				 ((listp l)
				  l)))
	 (ret-links (object-assoc obj-2 :end links-with-start-node)))
    (unless ret-links
      (setq ret-links (cogre-new-link obj-1 obj-2 arrow-type)))
    (cond
     ((object-p ret-links)
      (list ret-links))
     ((listp ret-links)
      ret-links))))

(defun epoxide-tsg-visualizer-mark-node (name)
  "Mark node having name NAME on graph."
  ;; Set origin for ibuffer.
  (when (bufferp name)
      (setq name (buffer-name name)))
  (if (and (epoxide-tsg-visualizer-shortened-name-p name)
	   (equal epoxide-tsg-visualizer-naming 'shortened)
	   (not (string-match "\\.tsg" name)))
      (setq-local epoxide-ibuffer-origin-buffer
		  (epoxide-tsg-visualizer-get-buffer-name-for-name name))
    (setq-local epoxide-ibuffer-origin-buffer name))
  (when (equal epoxide-tsg-visualizer-naming 'shortened)
    (setq name (epoxide-tsg-visualizer-create-shortened-name name)))
  (let ((c-node (cogre-find-node-by-name name)))
    (if (null c-node)
	(progn
	  ;; When no node having NAME was found, remove previous highlighting
	  ;; but keep buttons.
	  (remove-overlays)
	  (epoxide-tsg-visualizer-place-buttons))
      ;; When node having NAME was found clear previous highlighting.
      (remove-overlays)
      ;; Jump to where name should start
      (epoxide-cogre-go-to-element-name c-node)
      ;; and highlight the text.
      (overlay-put (make-overlay (point) (+ (point) (length name)))
		   'face '(:background "yellow"))
      ;; Jump to the middle of the name.
      (forward-char (/ (length name) 2))
      ;; Since remove-overlays cleared the buttons too, replace them now.
      (epoxide-tsg-visualizer-place-buttons))))

(defun epoxide-cogre-go-to-element-name (element)
  "On a COGRE graph go to the first character of the name of ELEMENT.
ELEMENT should be a node like COGRE object."
  ;; The position attribute of ELEMENT gives the x and y coordinates of where
  ;; the box of the element starts.
  (let* ((element-position (oref element :position))
	 (element-x (1+ (aref element-position 0)))
	 (element-y (1+ (aref element-position 1))))
    (goto-char (point-min))
    (forward-line element-y)
    (forward-char element-x)))

(defun epoxide-tsg-visualizer-place-buttons ()
  "Create buttons on nodes and assign `switch-to-buffer' when clicking on them."
  (save-excursion
    ;; Look up every cogre node.
    (dolist (node (epoxide-cogre-get-nodes))
      ;; Jump to the position in the buffer where the name of this node starts
      (epoxide-cogre-go-to-element-name node)
      ;; then create a button from the string that is as long as the name.
      ;; This way the button will be created even when a line crosses through
      ;; it and covers part of the name.
      (make-button (point) (+ (point) (length (oref node object-name)))
		   'action (lambda (x)
			     ;; Switch to node or output buffer.
			     (switch-to-buffer (button-get x 'buffer-name)))
		   'buffer-name (epoxide-tsg-visualizer-get-node-buffer-name
				 (oref node object-name))
		   'follow-link t)))
  (set-buffer-modified-p nil))

(defun epoxide-tsg-visualizer-buffer-name-p (cogre-node-name)
  "Return t if COGRE-NODE-NAME is also a buffer name."
  (when (or (string-match "*node:" cogre-node-name)
	    (string-match "*link:" cogre-node-name))
    t))

(defun epoxide-tsg-visualizer-get-node-buffer-name (cogre-node-name)
  "Return the buffer name of COGRE-NODE-NAME."
  (if (epoxide-tsg-visualizer-buffer-name-p cogre-node-name)
      cogre-node-name
    (epoxide-tsg-visualizer-get-buffer-name-for-name cogre-node-name)))

(defun epoxide-tsg-visualizer-next-node ()
  "Jump to one of the following nodes of the current COGRE node on the graph."
  (interactive)
  (epoxide-tsg-visualizer-adjacent-node t))

(defun epoxide-tsg-visualizer-previous-node ()
  "Jump to one of the preceding nodes of the current COGRE node on the graph."
  (interactive)
  (epoxide-tsg-visualizer-adjacent-node))

(defun epoxide-tsg-visualizer-adjacent-node (&optional next)
    "Jump to one of the adjacent nodes to the current node  on the graph.
If optional NEXT-NODE is t, jump to next node, else jump to
preceding node.

If only one adjacent node is located jump to that, if more than
one is found, the available options are offered using ido.  If
none is found, a notification is given.

Ibuffer grouping is set according to the selected node.

Optional argument NEXT signifies the direction in which to
jump.  When t, jump to the succeeding node, otherwise jump to the
preceeding node."
  (let (next-node)
    (cond
     ((not (member epoxide-ibuffer-origin-buffer
		   (with-current-buffer epoxide-root-buffer
		     (apply 'append (mapcar
				     (lambda (node)
				       (cons
					(epoxide-tsg-create-node-buffer-name
					 (node-name node)
					 (node-class node))
					(node-output-buffers node)))
				     epoxide-tsg-node-list)))))
      ;; If no node is selected, select the one from the node buffers
      ;; and mark that.
      (with-current-buffer epoxide-root-buffer
	(setq next-node (epoxide-tsg-create-node-buffer-name
			   (node-name (car epoxide-tsg-node-list))
			   (node-class (car epoxide-tsg-node-list))))))
     (t
      (with-current-buffer epoxide-ibuffer-origin-buffer
	(if (null epoxide-tsg-visualizer-show-links)
	    ;; In this case major mode of the origin buffer can only be
	    ;; epoxide-mode so call the corresponding function
	    (setq next-node (epoxide-get-adjacent-node next))
	  ;; In this case next cogre node can either be an output or a node
	  ;; buffer.
	  (if (equal major-mode 'epoxide-link-mode)
	      ;; When buffer is an output.
	      (setq next-node (epoxide-link-get-adjacent-node next))
	    ;; When  buffer is a node.
	    (if next
		(setq next-node
		      (epoxide-get-in-or-output epoxide-node-outputs
						     "output"))
	    (setq next-node (epoxide-get-in-or-output
			       epoxide-node-inputs "input"))))))))
    ;; When a next node is found mark its name, jump to it and set ibuffer
    ;; origin to that node.
    (when next-node
      (epoxide-tsg-visualizer-mark-node next-node))))

(defun epoxide-tsg-visualizer-mark-element ()
  "Highlight node on graph where point is at."
  (interactive)
  (epoxide-tsg-visualizer-mark-node
   (oref (cogre-node-at-point-interactive) object-name)))

(defun epoxide-tsg-visualizer-change-names ()
  "Change names on graph."
  (interactive)
  (let* ((options '(full shortened full))
	 (new-naming (cadr (member epoxide-tsg-visualizer-naming
				   options))))
    (cond
     ((equal new-naming 'full)
      (epoxide-tsg-visualizer-use-full-names))
     ((equal new-naming 'shortened)
      (epoxide-tsg-visualizer-use-shortened-names)))
    (setq epoxide-tsg-visualizer-naming new-naming)
    (cogre-refresh)
    (epoxide-tsg-visualizer-mark-node epoxide-ibuffer-origin-buffer)))

(defun epoxide-tsg-visualizer-toggle-showing-links ()
  "Toggle showing link buffers in the current TSG."
  (interactive)
  (setq epoxide-tsg-visualizer-show-links
        (not epoxide-tsg-visualizer-show-links))
  (epoxide-tsg-visualizer-show-helper epoxide-root-buffer))

(defun epoxide-tsg-visualizer-use-full-names ()
  "Set names of cogre-nodes to be the full ones."
  (dolist (node (epoxide-cogre-get-nodes))
    (cogre-set-element-name
     node
     (epoxide-tsg-visualizer-get-buffer-name-for-name
      (oref node object-name)))))

(defun epoxide-tsg-visualizer-use-shortened-names ()
  "Set names of cogre-nodes to be the shortened ones."
  (let (name)
    (dolist (node (epoxide-cogre-get-nodes))
      (setq name (oref node object-name))
      (cogre-set-element-name
       node
       (epoxide-tsg-visualizer-create-shortened-name name)))))

(defun epoxide-tsg-visualizer-create-shortened-name (name)
  "Shorten NAME."
  (if (string-match "*node:" name)
      (substring name 6 (string-match ":" name 6))
    (if (string-match "*link:" name)
	(substring name 6 (1- (length name)))
      name)))

(defun epoxide-tsg-visualizer-shortened-name-p (name)
  "Return t if NAME is shortened on the graph.
Return nil otherwise."
  (when (equal epoxide-tsg-visualizer-naming 'shortened)
    (unless (get-buffer name)
      (unless (or (string-match "*node:" name) (string-match "*link:" name))
	t))))

(defun epoxide-tsg-visualizer-get-node-name (node-name node-class)
  "Return a node's name on graph for a specific node.
Node is specified by its NODE-NAME and NODE-CLASS."
  (epoxide-tsg-visualizer-get-node-name-by-buffer-name
   (epoxide-tsg-create-node-buffer-name node-name node-class)))

(defun epoxide-tsg-visualizer-get-node-name-by-buffer-name (buffer-name)
  "Return node name on the graph for BUFFER-NAME."
  (let ((name buffer-name))
    (cond
     ((equal epoxide-tsg-visualizer-naming 'full)
      name)
     ((equal epoxide-tsg-visualizer-naming 'shortened)
      (epoxide-tsg-visualizer-create-shortened-name name)))))

(defun epoxide-tsg-visualizer-get-buffer-name-for-name (name)
  "Return the buffer name for the node on the graph named NAME."
  (let ((orig-node (with-current-buffer epoxide-root-buffer
		     (epoxide-tsg-fetch-node-by-name name))))
    (if orig-node
	(epoxide-tsg-create-node-buffer-name (node-name orig-node)
					  (node-class orig-node))
      (epoxide-tsg-assign-name-to-output-buffer
       (substring name 0 (string-match ":" name))
       (string-to-number (substring name
				    (1+ (string-match ":" name))))))))


;; --------------    EPOXIDE JSON processing functions -------------------------

(defun epoxide-json-insert-flowstat-helper (json to-buffer)
  "Process the given JSON object for displaying, and insert it to TO-BUFFER."
  (cond
   ((equal json nil)
    nil)
   ((and (listp json)
	 (and (epoxide-json-insertable (car json))
	      (epoxide-json-insertable (cdr json))))
    (with-current-buffer to-buffer
      (insert (epoxide-json-object-to-string (car json)) "="
	      (epoxide-json-object-to-string (cdr json)) ",")))
   ((epoxide-json-insertable json)
    (with-current-buffer to-buffer
      (insert (epoxide-json-object-to-string json) ",")))
   ((listp json)
      (epoxide-json-insert-flowstat-helper (car json) to-buffer)
      (epoxide-json-insert-flowstat-helper (cdr json) to-buffer))
   ((arrayp json)
    (dotimes (i (length json))
      (epoxide-json-insert-flowstat-helper (aref json i) to-buffer)))))

(defun epoxide-json-insertable (object)
  "Return t if OBJECT is not a list or an array, and when it is a string.
Otherwise return nil."
  (if (not (listp object))
      (if (or (not (arrayp object))
	      (stringp object))
	  t
	nil)
    nil))

(defun epoxide-json-object-to-string (object &optional round)
  "Convert OBJECT to string if OBJECT is either one of the followings:
string, keyword, symbol or number.
Return `undefined_type' if it is something else.
If optional argument ROUND is t, the result (when it is a number) is
rounded to five digits."
  (cond
   ((stringp object)
    object)
   ((keywordp object)
    (substring (symbol-name object) 1))
   ((symbolp object)
    (symbol-name object))
   ((numberp object)
    (if round
	(format "%0.5f" (* 0.00001 (round (* 100000 object))))
      (number-to-string object)))
   (t
    "undefined_type")))


;; --------------    Handling remote shell calls    ----------------------------

(defun epoxide-get-default-directory (hostinfo)
  "Convert HOSTINFO to a (tramp) directory.

HOSTINFO is either an IP address, a hostname, or a tramp
directory like '/ssh:remote.host:/' or '/mininet:h1:/'.  Empty
HOSTINFO means localhost.

The return value can be used as a `default-directory' for
`start-file-process' like functions.  See Info
node `(Tramp)Remote processes'."
  (cond
   ((eq 0 (length hostinfo))
    default-directory)                  ; localhost
   ((member hostinfo '("localhost" "127.0.0.1"))
    default-directory)                  ; localhost
   ((tramp-tramp-file-p hostinfo)
    hostinfo)
   (t                                   ; a hostname
    (concat "/ssh:" hostinfo ":/"))))

(defun epoxide-shell-command-to-string (host command)
  "On host HOST, execute shell command COMMAND.
Return the output as a string, see `shell-command-to-string' for
details.  HOST is a host definition, see
`epoxide-get-default-directory'."
  (let ((default-directory (epoxide-get-default-directory host)))
    (setq tramp-verbose epoxide-tramp-verbose-level)
    (shell-command-to-string command)))

(defun epoxide-start-process (host &rest args)
  "Start a program in a subprocess on HOST.  Return the process object for it.
ARGS are passed to `start-file-process'.  HOST is a host
definition, see `epoxide-get-default-directory'."
  (let ((default-directory (epoxide-get-default-directory host))
	process)
    (setq tramp-verbose epoxide-tramp-verbose-level)
    (setq process (apply 'start-file-process args))
    (when process
      (set-process-query-on-exit-flag process nil))
    process))


;; --------------    Support for autocomplete    -------------------------------

;; Following functions provide support for text completion with Auto-Complete.
;; When Auto-Complete is installed new sources are added when being in a buffer
;; of an .tsg file. Otherwise no extra functionality is provided.

(defvar epoxide-ac-source-class-candidates nil
  "Variable for containing node class names as candidates for autocomplete.")

(defvar ac-source-tsg-node-classes
  '((candidates . epoxide-ac-source-class-candidates)
    (document . epoxide-ac-source-tsg-node-classes-document)
    (candidate-face . epoxide-ac-classes-candidate-face)
    (selection-face . epoxide-ac-classes-selection-face))
  "Autocomplete source for node class name candidates.")

(defun epoxide-ac-source-tsg-node-classes-document (candidate)
  "Return documentation for CANDIDATE.

CANDIDATE is assumed to be a node class."
  (let* ((node-doc (concat "epoxide-" (downcase candidate) "-node-info"))
	 (doc-types '("node" "inputs" "config" "outputs"))
	 (doc (concat "CLASS " candidate ":"
		      (if (boundp (intern node-doc))
			  (concat "\n\n" (symbol-value (intern node-doc)))
			"")))
	 res i)
    (dolist (type doc-types)
      (setq i 0)
      (setq res (epoxide-tsg-eldoc-get-doc-string candidate type))
      (when res
    	(setq doc (concat doc "\n\n" (upcase type) ":\n"))
    	(dolist (r res)
    	  (setq doc (concat doc "- " (number-to-string i) ": "
			    (replace-regexp-in-string "\n" ". " r) "\n"))
    	  (incf i))))
    doc))

(defvar epoxide-ac-classes-color-1
  (face-attribute font-lock-type-face :foreground)
  "Color matching node class coloring.")

(defvar epoxide-ac-classes-color-2 "black"
  "The other color used when displaying a class candidate.")

(defface epoxide-ac-classes-candidate-face
  `((t (:background ,epoxide-ac-classes-color-1
        :foreground ,epoxide-ac-classes-color-2)))
  "Face for tsg class candidates."
  :group 'epoxide)

(defface epoxide-ac-classes-selection-face
  `((t (:weight ultra-bold
	:foreground ,epoxide-ac-classes-color-1
        :background ,epoxide-ac-classes-color-2)))
  "Face for tsg class selection."
  :group 'epoxide)

(defun epoxide-ac-source-dabbrev-candidates ()
  "Query possible expansions of current word using dabbrev."
  (epoxide-ac-dabbrev-get-candidates (current-word)))

(defun epoxide-ac-dabbrev-get-candidates (abbrev)
  "Get possible expansions for ABBREV.

A maximum of 100 candidates are queried using dabbrev, every buffer is checked.
Funcion based on ac-dabbrev.el by Kenichirou Oyama."
  (let ((dabbrev-check-other-buffers t)
	(i 0)
	(limit 100)
	expansion all-expansions)
    (dabbrev--reset-global-variables)
    (while (and (< i limit)
		(setq expansion (dabbrev--find-expansion abbrev 0 nil)))
      (setq all-expansions (cons expansion all-expansions))
      (incf i))
    all-expansions))

(defvar ac-source-epoxide-dabbrev
  '((candidates . epoxide-ac-source-dabbrev-candidates))
  "Autocomplete source for candidates returned by dabbrev.")

(defun epoxide-ac-init ()
  "Initialize autocomplete when used in an tsg file's buffer."
  ;; Add epoxide-tsg-mode to those supported by autocomplete, when ac is
  ;; available.
  (when (boundp 'ac-modes)
    (add-to-list 'ac-modes 'epoxide-tsg-mode))
  (when (boundp 'ac-sources)
    (setq-local epoxide-ac-source-class-candidates
		(epoxide-tsg-get-node-classes))
    (add-to-list 'ac-sources 'ac-source-words-in-all-buffer)
    (add-to-list 'ac-sources 'ac-source-epoxide-dabbrev)
    (add-to-list 'ac-sources 'ac-source-tsg-node-classes)))


;; --------------    EPOXIDE global functions    -------------------------------

(defun epoxide-string-or-nil (thing)
  "Return nil when THING is a string nil, return THING itself otherwise."
  (if (equal thing "nil")
      nil
    thing))

(defun epoxide-substract (list-a list-b)
  "Substract from LIST-A the items of LIST-B and return the resulting list."
  (let (ret)
    (dolist (a list-a ret)
      (unless (member a list-b)
	(setq ret (cons a ret))))))

(defun epoxide-change-list (list items-to-add items-to-remove)
  "Add and remove items from LIST and sort it.

LIST: quoted name of the list to change.
ITEMS-TO-ADD: these items are added to LIST when they are not already in it.
ITEMS-TO-REMOVE: these items are removed from the list."
  (dolist (item items-to-add)
    (unless (member item (symbol-value list))
      (set list (append items-to-add (symbol-value list)))))
  (dolist (item items-to-remove)
    (set list (delete item (symbol-value list))))
  (set list (sort (symbol-value list) #'string-lessp)))

(defun epoxide-list-ip-addresses ()
  "Return list of known IP addresses."
  '("127.0.0.1" "192.168.56.101"))

(defun epoxide-list-output-buffers ()
  "Return the list of all output buffers of the current epoxide session."
  (let ((nodes (epoxide-get-nodes epoxide-root-buffer))
	 outputs)
    (dolist (node nodes outputs)
      (setq outputs (append (node-output-buffers node) outputs)))))

(defun epoxide-list-clock-output-buffers ()
  "Return the list of all clock output buffers of the current epoxide session."
  (let ((nodes (epoxide-get-nodes epoxide-root-buffer))
	clock-outputs)
    (dolist (node nodes clock-outputs)
      (when (equal (node-class node) "Clock")
	    (setq clock-outputs
		  (append (node-output-buffers node) clock-outputs))))))

(defun epoxide-list-dpids ()
  "Return DPIDS."
  (with-current-buffer epoxide-root-buffer
    epoxide-dpids))

(defun epoxide-list-switch-names ()
  "Return switch names."
  (with-current-buffer epoxide-root-buffer
    epoxide-switch-names))

(defun epoxide-log (format-string &rest args)
  "Append text to message buffer, without using the echo area.
See `message' for the desciption of FORMAT-STRING and ARGS."
  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((read-only buffer-read-only))
      (when read-only
	(setq-local buffer-read-only nil))
      (save-excursion
	(goto-char (point-max))
	(insert (format format-string args) "\n"))
      (setq buffer-read-only read-only))))

(defun epoxide-ip-address-p (string)
  "Return t if STRING is either an IPv4 or an IPv6 address."
  (or (epoxide-ipv4-address-p string)
      (epoxide-ipv6-address-p string)))

(defun epoxide-ipv4-address-p (string)
  "Return t if STRING is an IPv4 address."
  (let* ((n "\\([0-9]\\{1,3\\}\\)")
         (d "\\.")
         (addr-regexp (format "\\`%s%s%s%s%s%s%s\\'" n d n d n d n)))
    (when (eq 0 (string-match addr-regexp string))
      (cl-every (lambda (x) (< (string-to-number (match-string x string)) 256))
                '(1 2 3 4)))))

(defun epoxide-ipv6-address-p (string)
  "Dummy function.  Argument STRING is ignored.  Return nil."
  ;; TODO: to be implemented.
  nil)

(defun epoxide-ip-address-or-name-p (string)
  "Return t if STRING is either an IP address or a name."
  (or (epoxide-ip-address-p string)
      (epoxide-name-p string)))

(defun epoxide-name-p (string)
  "Return t if STRING is a hostname."
  (when (or (eq (string-match "www\\.[a-z]+\\.[a-z]+" string) 0)
	    (eq (string-match "[a-z]+\\.[a-z]+" string) 0)
	    (eq (string-match "localhost" string) 0))
    t))

(defun epoxide-positive-number-p (string)
  "Return t if STRING is a positive number."
  (when (or (eq (string-match "^[1-9][0-9]*$" string) 0)
	    (eq (string-match "^[0-9]+\\.[0-9]+$" string) 0))
    t))

(defun epoxide-output-buffer-p (string)
  "Dummy function.  Argument STRING is ignored.  Return nil."
  ;; TODO: to be implemented.
  t)

(defun epoxide-number-p (string)
  "Dummy function.  Argument STRING is ignored.  Return nil."
  ;; TODO: to be implemented.
  t)

(defun epoxide-relation-sign-p (string)
  "Dummy function.  Argument STRING is ignored.  Return nil."
  ;; TODO: to be implemented.
  t)

(defun epoxide-dpid-p (string)
  "Return t if STRING is a dpid."
  (with-current-buffer epoxide-root-buffer
    (when (member string epoxide-dpids)
      t)))

(defun epoxide-switch-name-p (string)
  "Return t if STRING is a dpid."
  (with-current-buffer epoxide-root-buffer
    (when (member string epoxide-switch-names)
      t)))

(defun epoxide-ip-address-in-network (ip-address network-address net)
  "Check whether an IPv4 address is in the scope of a subnetwork.

IP-ADDRESS defines the address of a host, NETWORK-ADDRESS defines
the subnet and NET the netmask that could either be quad-dotted
notation or the prefix length and.  All arguments must be
strings."
  (let* ((f-1 (lambda (x)
		(mapcar 'string-to-number (split-string x "\\."))))
	 (f-2 (lambda (ipv4 netmask)
		(let* ((net (mapconcat
			     'epoxide-decimal-to-binary ipv4 ""))
		       (netmask (split-string
				 (mapconcat 'epoxide-decimal-to-binary
					    netmask "") ""))
		      (netmask (butlast (cdr netmask))))
		  (substring net 0 (length (delete "0" netmask))))))
	 (netmask (if (string-match "\\." net)
		      net
		    (epoxide-prefix-length-to-quad-dotted net)))
	 (n (funcall f-2 (funcall f-1 network-address) (funcall f-1 netmask)))
	 (ip (substring (mapconcat 'epoxide-decimal-to-binary
				   (funcall f-1 ip-address) "") 0 (length n))))
    (when (equal n ip)
      t)))

(defun epoxide-prefix-length-to-quad-dotted (network)
  "Convert the NETWORK prefix length to quad-dotted notation."
  (when (stringp network)
    (setq network (string-to-number network)))
  (let ((b (concat (make-string network ?1) (make-string (- 32 network) ?0)))
	(ret ""))
    (dotimes (i 4)
      (let ((d (string-to-number (substring b (* i 8) (+ 8 (* i 8))) 2)))
	(setq ret (format "%s.%s" ret d))))
    (substring ret 1)))

(defun epoxide-decimal-to-binary (decimal)
  "Convert DECIMAL to an at least 8 bit binary and return it as a string."
  (let ((b ""))
    (while (not (equal decimal 0))
      (setq b (concat (if (equal 1 (logand decimal 1)) "1" "0") b))
      (setq decimal (lsh decimal -1)))
    (when (equal b "")
      (setq b "0"))
    (if (< (length b) 8)
	(concat (make-string (- 8 (length b)) ?0) b)
      b)))


;; --------------    EPOXIDE node support functions    -------------------------

(defun epoxide-write-node-output (output output-buffer-name &optional face)
  "Insert OUTPUT into buffer named OUTPUT-BUFFER-NAME with FACE.
When point is at `point-max' in the buffer, insertion moves point,
otherwise point stays where it has been.  When buffer is displayed
in a window but not selected, point acts the same."
  (when (and output output-buffer-name)
    (let ((output (if face
		      (propertize output 'face face)
		    output)))
    (with-current-buffer output-buffer-name
      (if (not (equal (point) (point-max)))
	  (save-excursion
	    (goto-char (point-max))
	    (insert output))
	(insert output)
	(let (window)
	  (when (setq window (get-buffer-window (current-buffer)))
	    (set-window-point window (point-max)))))))))

(defun epoxide-node-read-inputs (&optional wait-for-all-inputs
					   set-markers)
  "Read every input of a node.

Return a list buffer name (from where the input was read) read
string pairs.  When optional argument WAIT-FOR-ALL-INPUTS is
non-nil, return nil until every input has some data and do not
move marker positions.  When SET-MARKERS is non-nil new marker
positions are set even when WAIT-FOR-ALL-INPUTS is set and there
are inputs having no data."
  (unless (boundp 'epoxide-input-markers)
    (set (make-local-variable 'epoxide-input-markers) nil))
  (let ((node-buffer (current-buffer))
	(markers (copy-sequence epoxide-input-markers))
	(input-list (copy-sequence epoxide-node-inputs))
	new-markers inputs)
    (dolist (i (reverse input-list))
      (let* ((r (epoxide-node-read-input
		 i (epoxide-node-get-input-pos i markers)))
    	     (pos (car r))
    	     (input (cadr r)))
	(when r
	  (push `(,i ,pos) new-markers)
	  (push `(,i ,input) inputs))))
    (with-current-buffer node-buffer
      (if wait-for-all-inputs
	  (when (or set-markers
		    (epoxide-node-all-inputs-ready inputs))
	    (setq-local epoxide-input-markers (copy-sequence new-markers)))
    	(setq-local epoxide-input-markers (copy-sequence new-markers)))
      inputs)))

(defun epoxide-node-get-input-pos (input markers)
  "Return the current marker position for INPUT from MARKERS."
  (let ((ret (cadr (assoc input markers))))
    (unless ret
      (setq ret 1))
    ret))

(defun epoxide-node-read-input (buffer start)
  "Read node input from BUFFER with starting position START."
  (let ((buffer (get-buffer buffer))
	ret)
    (unless start
      (setq start 1))
    (setq ret `(,start nil))
    (when (and buffer
	       (buffer-live-p buffer))
      (with-current-buffer buffer
	(let ((input (epoxide-chomp
		      (buffer-substring-no-properties start (point-max)))))
	  (when (> (length input) 0)
	    (setq ret (cons (point-max) `(,input)))))))
    ret))

(defun epoxide-node-all-inputs-ready (inputs)
  "Return t if every INPUTS have data, nil otherwise."
  (unless (member 0
		  (mapcar (lambda (x)
			    (condition-case nil
				(length (cadr x))
			      (error 0))) inputs))
    t))

(defun epoxide-node-get-inputs-as-string (inputs)
  "Concatenate read data from the list INPUTS."
  (let (ret)
    (dolist (i inputs)
      (when (cadr i)
	(push (cadr i) ret)))
    (when ret
      (concat (mapconcat 'identity (nreverse ret) "\n") "\n"))))

(defun epoxide-node-get-inputs-as-list (inputs)
  "Return a list of only the read data of the list INPUTS."
  (mapcar (lambda (x)
	       (cadr x))
	     inputs))

(defun epoxide-node-enable-input-active (inputs enable-input)
  "Return t when the node has new data on its enable input.

INPUTS contains the data from the node's every
input.  ENABLE-INPUT specifies the input buffer where the enable
signal is expected.  ENABLE-INPUT can either be a buffer or a
buffer name or number specifing the index of the input in
INPUTS."
  (let* ((in (cond
	      ((stringp enable-input)
	       enable-input)
	      ((bufferp enable-input)
	       (buffer-name enable-input))
	      ((numberp enable-input)
	       (car (nth enable-input inputs)))
	      (t nil)))
	 (e (cadr (assoc in inputs))))
    (when (and e (> (length e) 0))
      t)))

(defun epoxide-node-get-config (&optional index)
  "Return the current node's configuration arguments.

Iterate through the node's static and dynamic configuration
arguments and return the currently active ones.  If a node has
dynamic configuration arguments (that is the arguments are read
from a buffer) those take priority over the static ones (those
specified in the configuration list).

When optional argument INDEX is present return only the specified
argument."
  (if index
      (let* ((dynamic (nth index epoxide-node-dynamic-config-list))
	     (ret (if dynamic
		      (epoxide-node-read-config-buffer dynamic)
		    (nth index epoxide-node-config-list))))
	(epoxide-string-or-nil ret))
    (let ((max-length (if (> (length epoxide-node-config-list)
			     (length epoxide-node-dynamic-config-list))
			  (length epoxide-node-config-list)
			(length epoxide-node-dynamic-config-list)))
	  (i 0)
	  ret)
      (while (< i max-length)
	(if (null (nth i epoxide-node-dynamic-config-list))
	    (push (nth i epoxide-node-config-list) ret)
	  (push (epoxide-node-read-config-buffer
		 (nth i epoxide-node-dynamic-config-list))
		ret))
	(incf i))
      (mapcar 'epoxide-string-or-nil (nreverse ret)))))

(defun epoxide-node-read-config-buffer (buffer-or-name)
  "Read the last line of BUFFER-OR-NAME without moving point."
  (when (and buffer-or-name
	     (buffer-live-p (get-buffer buffer-or-name)))
    (with-current-buffer buffer-or-name
      (save-excursion
	(goto-char (point-max))
	(goto-char (line-beginning-position))
	(when (equal major-mode 'epoxide-link-mode)
	  (forward-line -1))
	(buffer-substring-no-properties (line-beginning-position)
					(line-end-position))))))

(defun epoxide-eval-function (function args)
  "Evaluate FUNCTION on ARGS.

FUNCTION should be a string representation of a function name,
lambda function or special form (or, and).  ARGS should be a list
of arguments with the correct type."
  (let* ((function (epoxide-interpret-function function))
	 (type (car function))
	 (function (cadr function)))
    (pcase type
      (`elisp
       (cond
	((special-form-p function)
	 (condition-case err
	     (eval (cons function args))
	   (error (epoxide-signal-function-eval-error function err))))
	(t
	 (condition-case err
	     (apply function args)
	   (error (epoxide-signal-function-eval-error function err)))))))))

(defun epoxide-interpret-function (function)
  "Create a real function from the string parameter FUNCTION."
  (cond
   ((equal (string-match "elisp:" function) 0)
    `(elisp ,(car (read-from-string (substring function 6)))))
   (t
    `(elisp ,(car (read-from-string function))))))


;; --------------    Event Queue    --------------------------------------------

(defun epoxide-event-add-task (buffer-name)
  "Add BUFFER-NAME to the event queue."
  (if epoxide-event-queue
      (push buffer-name (cdr (last epoxide-event-queue)))
    (push buffer-name epoxide-event-queue))
  (run-with-idle-timer 0 nil #'epoxide-event--process-queue))

(defun epoxide-event-start ()
  "Start scheduling events from the event queue."
  (when (timerp epoxide-event-timer)
    (cancel-timer epoxide-event-timer))
  (setq epoxide-event-timer
	(tf-run-with-idle-timer 0.1 t 0.1 t nil
				'epoxide-event--process-queue)))

(defun epoxide-event-stop ()
  "Stop event queue scheduler by stoping its timer."
  (when (timerp epoxide-event-timer)
    (cancel-timer epoxide-event-timer)
    (setq epoxide-event-timer nil)
    (setq epoxide-event-queue nil)))

(defun epoxide-event-running-p ()
  "Return t if the event scheduler is running."
  (timerp epoxide-event-timer))

(defun epoxide-event--process-queue (&rest ignore)
  ;; checkdoc-params: (ignore)
  "Process the event queue until it becomes empty when Emacs is idle."
  (unless epoxide-event--processing-flag
    (let ((epoxide-event--processing-flag t))
      ;; (info "(elisp) Idle Timers") discourages to use
      ;; `input-pending-p', but here it's not a problem to block other
      ;; processes.1
      (while (and (not (input-pending-p))
                  epoxide-event-queue)
        (epoxide-event--process-first)))))

(defun epoxide-event--process-first ()
  "Process and remove the first item of the event queue."
  (setq epoxide-event-queue (delete-dups epoxide-event-queue))
  (let ((epoxide-task (pop epoxide-event-queue)))
    (if epoxide-task
	(when (buffer-live-p (get-buffer epoxide-task))
	  (with-current-buffer epoxide-task
	    (condition-case err
		(funcall epoxide-exec-function)
	      (error (message "Error when executing %s: %s"
			      (symbol-name epoxide-exec-function)
			      (error-message-string err)))))))))


;; --------------    Task Handling    ------------------------------------------

(defun epoxide-task-start (buf)
  "Start an initiated epoxide task BUF by scheduling it."
  (epoxide-event-add-task buf))

(defun epoxide-task-kill (buf)
  "Delete epoxide task BUF by removing it from the queue."
  (if (member buf epoxide-event-queue)
      (setq epoxide-event-queue
            (delete (car (member buf epoxide-event-queue))
                    epoxide-event-queue)))
  (epoxide-kill-buffer buf))


;; --------------    Link Init    ----------------------------------------------

(defun epoxide--link-notify (beg end len)
  "Notify outputs about new content.
See `after-change-functions' for the desciption of BEG, END, LEN."
  (let ((link (buffer-name)))
    (when (< 2 (- end beg))
      (dolist (node (epoxide-get-nodes epoxide-root-buffer))
        (when (member link (node-input-buffers node))
          (epoxide-event-add-task
           (epoxide-tsg-create-node-buffer-name
            (node-name node)
            (node-class node))))))))

(defun epoxide-link-init ()
  "Init function for links."
  (setq inhibit-modification-hooks nil)
  (add-hook 'after-change-functions 'epoxide--link-notify nil t))


;; --------------    Node recommender    ---------------------------------------

(defgroup epoxide-recommender nil
  "Epoxide node recommender defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-tsg-file-collection
  (concat (file-name-directory load-file-name) "../examples/")
  "Location of .tsg file collection."
  :type 'directory
  :group 'epoxide-recommender)

(defcustom epoxide-tsg-index-file-location
  (concat (file-name-directory load-file-name))
  "Location of .tsg index file."
  :type 'directory
  :group 'epoxide-recommender)

(defcustom epoxide-recommender-method 'most-popular
  "Define which method should be used for node recommendation.
`most-popular': recommend the most frequently used nodes that do
                not appear in the current TSG.
`most-similar': choose TSGs that are the most similar to the current
                TSG and and recommend their most frequently used
                nodes that do not appear in the current TSG."
  :type '(radio
	  (const most-popular)
	  (const most-similar))
  :group 'epoxide-recommender)

(defcustom epoxide-recommender-display-method 'ido
  "Define which method should be used for node recommendation.
`ido': use IDO to select from among the possible choices."
  :type '(radio
	  (const ido))
  :group 'epoxide-recommender)

(defvar epoxide-tsg-file-index nil
  "Current data collected by indexing .tsg files.")

(defun epoxide-recommender-index-tsg-files ()
  "Collect which nodes are used in all the .tsg files.

For better performance data is saved in a file and updated only
when the respecitve .tsg files are changed."
  (let* ((file-list
	  (delq
	   nil (mapcar (lambda (x)
			 (when (equal 0 (string-match "[[:alnum:]-]+.tsg" x))
			   (unless (string-match "~" x)
			     (let ((file
				    (expand-file-name
				     (concat epoxide-tsg-file-collection x))))
			       `(,file ,(nth 5 (file-attributes file)))))))
		       (directory-files epoxide-tsg-file-collection))))
	 (index-file
	  (find-file-noselect
	   (concat epoxide-tsg-index-file-location ".tsg-index.dat")))
	 (indexed-tsgs
	  (or epoxide-tsg-file-index
	      (with-current-buffer index-file
		(let ((string (buffer-substring-no-properties (point-min)
							      (point-max))))
		  (if (equal string "")
		      nil
		    (car (read-from-string string)))))))
	 (node-classes (epoxide-tsg-get-node-classes))
	 new-index update)
    (dolist (f file-list)
      (let ((indexed (assoc (car f) indexed-tsgs)))
	(if (equal (cadr f) (cadr indexed))
	    (push indexed new-index)
	  (push (epoxide-recommender-index-tsg-file
		 (car f) node-classes) new-index)
	  (setq update t))))
    (setq new-index (nreverse (delq nil new-index)))
    (when update
      (with-current-buffer index-file
	(erase-buffer)
	(insert (pp-to-string new-index))
	(save-buffer)
	(setq epoxide-tsg-file-index new-index)))
    (unless epoxide-tsg-file-index
      (setq epoxide-tsg-file-index new-index))
    (kill-buffer index-file))
  epoxide-tsg-file-index)

(defun epoxide-recommender-index-tsg-file (file classes)
  "Collect which nodes are used in FILE.

CLASSES defines the strings that should be considered as a node
class."
  (let ((buf-list (buffer-list))
	(buf
	 (or (get-file-buffer file) ;; Get buffer is file is already open.
	     (find-file-noselect file t t))) ;; Open file without `epoxide-mode'
	                                     ;; otherwise.
	node-classes)
    (with-current-buffer buf
      (setq file (buffer-file-name))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
		"\\([A-Z][[:alnum:]-]*?\\)\\s-*?[(|;]" nil t)
	  (re-search-backward "\\([A-Z][[:alnum:]-]*?\\)\\s-*?[(|;]")
	  (let* ((end (1+ (point)))
		 (name-end (if (re-search-backward "::" nil t)
			       (+ (point) 2)
			     (line-beginning-position)))
		 (exp-end (progn
			    (goto-char end)
			    (if (re-search-backward ";" nil t)
				(1+ (point))
			      (line-beginning-position))))
		 (prev-node (progn
			      (goto-char end)
			      (if (re-search-backward ">" nil t)
				  (1+ (point))
				(line-beginning-position))))
		 (start (max name-end exp-end prev-node))
		 (class (epoxide-chomp
			 (buffer-substring-no-properties start end))))
	    (goto-char end)
	    (unless (equal class "View")
	      (when (member class classes)
		(push class node-classes))))))
      (unless (member (current-buffer) buf-list)
	(kill-buffer)))
    `(,file ,(nth 5 (file-attributes file)) ,(nreverse node-classes))))

(defun epoxide-recommender-recommend ()
  "Show recommended nodes."
  (interactive)
  (let* ((tsgs (epoxide-recommender-index-tsg-files))
	 (recommender-name (symbol-name epoxide-recommender-method))
	 (display (symbol-name epoxide-recommender-display-method))
	 (base "epoxide-recommender-")
	 (recommender (intern (concat base "recommend-" recommender-name)))
	 (display (intern (concat base "display-" display))))
    (insert (funcall display recommender-name
		     (funcall recommender tsgs (buffer-file-name))))))

(defun epoxide-recommender-recommend-most-popular (tsgs current-tsg)
  "Create a list of the nodes ordered by their popularity.

The must frequently used node will be at the head of the
list.  Those nodes that appear in the current TSG get removed from
the suggestion list.

TSGS contain the indexed nodes of every TSG.  CURRENT-TSG is the
buffer of the currently used TSG."
  (let ((current-nodes (delete-dups (nth 2 (assoc current-tsg tsgs))))
	nodes)
    (dolist (tsg tsgs)
      (dolist (node (nth 2 tsg))
	(let ((item (assoc node nodes)))
	  (if (null item)
	      (unless (member node current-nodes)
		(push `(,node 1) nodes))
	    (setq nodes (delete item nodes))
	    (push `(,(car item) ,(1+ (cadr item))) nodes)))))
    (setq nodes (sort nodes (lambda (x y)
			      (> (cadr x) (cadr y)))))
    (mapcar 'car nodes)))

(defun epoxide-recommender-recommend-most-similar (tsgs current-tsg)
  ;; checkdoc-params: (tsgs current-tsg)
  "Dummy function."
  ;; TODO: to be implemented.
  )

(defun epoxide-recommender-display-ido (recommender suggestions)
  "Display RECOMMENDER's SUGGESTIONS using IDO."
  (ido-completing-read
     (concat recommender ": ") suggestions))


;; --------------    Function calls    -----------------------------------------

;; Open and parse files with .tsg extension.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsg\\'" . epoxide-tsg-mode))

;; Open files with .topo extension.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.topo\\'" . epoxide-topology-mode))

;; -----------------------------------------------------------------------------

(provide 'epoxide)

;;; epoxide.el ends here
