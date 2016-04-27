;;; Decision-Summary.el --- EPOXIDE Decision-Summary node definition file

;; Copyright (C) 2015-2016 István Pelle

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

;; Node is able to collect information from Decision nodes' status
;; outputs and display them in a table format.  The node takes an
;; indefinite number of inputs that should be the status outputs of
;; Decision nodes.  Its configuration arguments are optional.  It takes
;; as many configuration arguments as many inputs the node has, these
;; should be nil or non-nil values.  When configuration arguments are
;; present, inputs are checked against these.  The node does not has
;; any outputs.

;; Results are displayed using the following columns: decision node
;; name, result, timestamp, reason.  The first row is always the
;; evaluation of the current TSG scenario that is the decision nodes'
;; statuses connected to the summary node.  When no configuration
;; argument is present, this test passes only when all inputs have
;; passed as well.  If configuration arguments are present, an input
;; passes the test iif its value (nil if failed, non-nil if passed)
;; matches the one specified in the respective configuration
;; argument.  Other rows in the table list each inputs and their
;; results.

;;; Code:

(require 'epoxide)
(require 'org-table)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-decision-summary--data)
  (defvar epoxide-decision-summary--default-header-format)
  (defvar epoxide-decision-summary--header-line))

(defun epoxide-decision-summary-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "decision status"))
    ((doc-string . "..."))))

(defun epoxide-decision-summary-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "mask of input-0"))
    ((doc-string . "mask of input-1"))
    ((doc-string . "..."))))

(defun epoxide-decision-summary-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "not used")
     (auto-create . nil))))

(defun epoxide-decision-summary-init ()
  "Initialize buffer-local variables."
  (set (make-local-variable 'epoxide-decision-summary--data) nil)
  (set (make-local-variable 'epoxide-decision-summary--default-header-format)
       header-line-format)
  (set (make-local-variable 'epoxide-decision-summary--header-line) "")
  (setq-local header-line-format
	      '(:eval (epoxide-decision-summary--update-header))))

(defun epoxide-decision-summary-exec ()
  "Display read decision statuses in a table format."
  (let ((window (get-buffer-window))
	(separator "\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*")
	(input (epoxide-node-get-inputs-as-list (epoxide-node-read-inputs))))
    ;; Display when window is shown.
    (when (and window input)
      (let ((i 0))
	(while (< i (length input))
	  (when (nth i input)
	    (setq-local epoxide-decision-summary--data
			(epoxide-setl epoxide-decision-summary--data i
				      (car (last (split-string (nth i input) separator t "\s\n\t") 2)))))
	  (incf i)))
      (epoxide-decision-summary--insert-data))))

(defun epoxide-decision-summary--insert-data ()
  "Parse inputs and display them in a table format."
  (let* ((pos (point))
	 (window (get-buffer-window))
	 (win-start (window-start window))
	 (name-len 0)
	 (reason-len 0)
	 results)
    (setq-local header-line-format
		epoxide-decision-summary--default-header-format)
    (erase-buffer)
    (setq-local truncate-lines t)
    (insert "Decision node | Result | Timestamp | Reason\n")
    (dolist (d epoxide-decision-summary--data)
      (if (null d)
	  (insert "empty\n")
	(let* ((report (split-string (epoxide-chomp d) "\n"))
	       (node (pop report))
	       (timestamp (pop report))
	       (reason (mapconcat 'identity (cdr report) "; ")))
	  (when (> (length reason) reason-len)
	    (setq reason-len (length reason)))
	  (cond
	   ((string-match "[[:alnum:]-]+:\spassed" node)
	    (let ((n (split-string node ":" t "\s")))
	      (push t results)
	      (setq node (cons (car n)
			       `(,(propertize (cadr n) 'face 'success))))))
	   ((string-match "[[:alnum:]-]+:\sfailed" node)
	    (let ((n (split-string node ":" t "\s")))
	      (push nil results)
	      (setq node (cons (car n)
			       `(,(propertize (cadr n) 'face 'isearch-fail))))))
	   (t
	    (push t results)
	    (setq node `(,node))))
	  (when (> (length (car node)) name-len)
	    (setq name-len (length (car node))))
	  (insert (car node) " | " (if (cadr node)
				       (cadr node)
				     "") " | "
				     timestamp " | " reason "\n"))))
    (goto-char (point-min))
    (forward-line)
    (insert "TSG scenario | "
	    (if (epoxide-decision-summary--evaluate-results (nreverse results))
		(propertize "passed" 'face 'success)
	      (propertize "failed" 'face 'isearch-fail)) "\n"
	      (make-string name-len ?-) " | "
	      (make-string 6 ?-) " | "
	      (make-string 26 ?-) " | "
	      (make-string reason-len ?-) "\n")
    (org-table-convert-region (point-min) (point-max) '(4))
    (goto-char (point-min))
    (setq-local epoxide-decision-summary--header-line
		    (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position)))
    (delete-region (line-beginning-position) (line-end-position))
    (delete-char 1)
    (set-window-start window win-start)
    (goto-char pos)
    (setq-local header-line-format
	      '(:eval (epoxide-decision-summary--update-header)))))

(defun epoxide-decision-summary--evaluate-results (results)
  "Check input RESULTS against node configuration parameters."
  (let ((config (copy-sequence (epoxide-node-get-config)))
	(i 0))
    (while (< i (length config))
      (setq results (epoxide-setl results i
				  (if (equal (nth i config) (nth i results))
				      t
				    nil)))
      (incf i))
    (unless (member nil results)
      t)))

(defun epoxide-decision-summary--update-header ()
  "Display table header in window header."
  (let* ((j (if (scroll-bar-columns 'left)
		 (1+ (scroll-bar-columns 'left))
	       0))
	 (o (window-hscroll))
	 (col (current-column))
	 (i 0)
	 (header (concat (make-string j ?\s)
			 epoxide-decision-summary--header-line))
	 (text (concat header
		       (make-string (- (+ (line-end-position) 2)
				       (length header))
				    ?\s)))
	 (ruler (substring text o)))
    ;; Show the `current-column' marker.
    (setq i (+ j (- col o)))
    (when (and (>= i 0) (< i (length ruler))
	       (> (length epoxide-decision-summary--header-line) 0))
      (aset ruler i ?¤))
    ruler))

(defun epoxide-decision-summary-stop ()
  "Dummy function."
  nil)

(provide 'decision-summary)

;;; Decision-Summary.el ends here
