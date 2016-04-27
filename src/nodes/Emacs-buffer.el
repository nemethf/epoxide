;;; Emacs-Buffer.el --- EPOXIDE Emacs-Buffer node definition file

;; Copyright (C) 2016 István Pelle

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

;; The node is able to connect to any Emacs buffer and relay the
;; changes occurring to these on its outputs.

;; The node has no inputs and takes and indefinite number of
;; configuration arguments and provides the same number of outputs as
;; the number of its configuration arguments. The configuration
;; arguments specify which Emacs buffer the node has to look for
;; changes and the changes in these buffers are copied to the node's
;; outputs in the same order as they were given by the configuration
;; arguments (i.e. the first specified buffer (in the configuration
;; list) is connected the to first output, the second to the second
;; and so on).

;; The node walks through each of the buffers listed in its
;; configuration list and creates the `epoxide-emacs-buffer-outputs'
;; buffer local variable in them and appends the respective outputs to
;; the variable. It adds the function
;; `epoxide-emacs-buffer-copy-buffer-content' as an
;; `after-change-function' to the buffer in order for the outputs to
;; get notified when a change occurs.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-outputs)
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-emacs-buffer-current-buffers)
  (defvar epoxide-emacs-buffer-outputs))

(defgroup epoxide-emacs-buffer nil
  "Emacs-buffer node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-emacs-buffer-create-buffer t
  "When t, when assigning a nonexistent buffer to an output create the buffer."
  :type 'boolean
  :group 'epoxide-emacs-buffer)

(defun epoxide-emacs-buffer-input-info ()
  "Provide documentation, value tips and validation for input fields."
  nil)

(defun epoxide-emacs-buffer-config-info ()
  "Provide documentation, value tips and validation for config fields."
  `(((doc-string . "buffers..."))))

(defun epoxide-emacs-buffer-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "copies of buffer contents..."))))

(defun epoxide-emacs-buffer-init ()
  "Perform initial notification configuration of the specified buffers.

Create buffer local variable
`epoxide-emacs-buffer-current-buffers' in order to contain the
currently active node configuration."
  (set (make-local-variable 'epoxide-emacs-buffer-current-buffers)
       (copy-sequence (epoxide-node-get-config)))
  (let* ((len (length epoxide-emacs-buffer-current-buffers)))
    (dotimes (i len)
      (let ((output
	     (epoxide-tsg-assign-name-to-output-buffer epoxide-node-name i))
	    (buffer (nth i epoxide-emacs-buffer-current-buffers)))
	(epoxide-emacs-buffer--assign-buffer-to-output buffer output)))))

(defun epoxide-emacs-buffer-copy-buffer-content (begin end &rest ignore)
  ;; checkdoc-params: (ignore)
  "Copy changed contents of the current buffer to the assigned outputs.

Changes from buffer position BEGIN until END are copied to all
the outputs that need to know about the changes."
  ;; TODO: decide what to do when not the last line has been changed
  ;; and whether or not to insert a new line after every change. Now:
  ;; changes happening anywhere in the source buffer are appended to
  ;; the end of the assigned ouitput buffer(s) and new line is only
  ;; inserted when it is read from the source buffer.
  (combine-after-change-calls
    (when (> (- end begin) 0)
      (let ((data (buffer-substring begin end)))
	;; Iterate through the list of output buffers that need to be updated.
	(dotimes (i (length epoxide-emacs-buffer-outputs))
	  (let ((o (nth i epoxide-emacs-buffer-outputs)))
	    (if (buffer-live-p (get-buffer o))
		;; Update when output exists.
		(with-current-buffer o
		  (let ((beg (point-max)))
		    (epoxide-write-node-output data o)
		    ;; Call notification function "manually" because
		    ;; the current function is an
		    ;; `after-change-function' and modification of the
		    ;; output buffer will not result in calling any
		    ;; other `after-change-function' including the
		    ;; notification function.
		    (epoxide--link-notify beg (point-max) nil)))
	      ;; Remove output from list when it ceased to exist.
	      (setq-local epoxide-emacs-buffer-outputs
			  (epoxide-setl epoxide-emacs-buffer-outputs i nil))
	      ;; Remove after-change-function hook when there is no
	      ;; output that needs to be notified.
	      (epoxide-emacs-buffer-remove-hook))))
	(setq-local epoxide-emacs-buffer-outputs
		    (delete nil epoxide-emacs-buffer-outputs))))))

(defun epoxide-emacs-buffer-config-change-handler ()
  "Handle configuration parameter change.

Assign outputs to new buffers and remove assignments from buffers
that were deleted from the configuration list."
  (let ((config (epoxide-node-get-config)))
    (epoxide-emacs-buffer--remove-buffers config)
    (epoxide-emacs-buffer--add-buffers config)
    (setq-local epoxide-emacs-buffer-current-buffers config)))

(defun epoxide-emacs-buffer-remove-hook ()
  "Remove node's after-change-function from the current buffer."
  (when (null epoxide-emacs-buffer-outputs)
    (remove-hook 'after-change-functions
		 'epoxide-emacs-buffer-copy-buffer-content t)))

(defun epoxide-emacs-buffer--remove-buffers (config)
  "Clear notification of buffers that were affected by the config change.
CONFIG contains the new node configuration arguments."
  (dotimes (i (length epoxide-emacs-buffer-current-buffers))
    (let ((buffer (nth i epoxide-emacs-buffer-current-buffers))
	  (output (epoxide-tsg-assign-name-to-output-buffer
		   epoxide-node-name i)))
      ;; Check which buffers disappeared or changed place in the
      ;; configuration list.
      (unless (equal buffer (nth i config))
	(when (buffer-live-p (get-buffer buffer))
	  (with-current-buffer buffer
	    (when (boundp 'epoxide-emacs-buffer-outputs)
	      ;; And remove notification of the output buffer when a
	      ;; change happened.
	      (setq-local epoxide-emacs-buffer-outputs
			  (delete output epoxide-emacs-buffer-outputs))
	      ;; If there is nothing to notify anymore remove the hook
	      ;; function.
	      (epoxide-emacs-buffer-remove-hook))))))))

(defun epoxide-emacs-buffer--add-buffers (config)
  "Add notification to those buffer that are new in this confiuration.
CONFIG contains the new node configuration arguments."
  (dotimes (i (length config))
    (let ((buffer (nth i config))
	  (output (epoxide-tsg-assign-name-to-output-buffer
		   epoxide-node-name i)))
      (unless (equal buffer (nth i epoxide-emacs-buffer-current-buffers))
	(epoxide-emacs-buffer--assign-buffer-to-output buffer output)))))

(defun epoxide-emacs-buffer--assign-buffer-to-output (buffer output)
  "Modify BUFFER's local variable to notify OUTPUT on change.

When the specific BUFFER is nonexistent and
`epoxide-emacs-buffer-create-buffer' is nil, write a warning to
into the messages buffer.  When
`epoxide-emacs-buffer-create-buffer' is non-nil write a
notification into the messages buffer and create the buffer
before assignment."
  (when (and buffer output)
    (if (buffer-live-p (get-buffer buffer))
	(epoxide-emacs-buffer--add-output buffer output)
      (let ((msg
	     (format
	      "Node %s :: %s cannot connect to buffer %s: no buffer named %s"
		  epoxide-node-name epoxide-node-class buffer buffer)))
	(if (null epoxide-emacs-buffer-create-buffer)
	    (epoxide-log msg)
	  (epoxide-log (concat ". Creating buffer"))
	  (get-buffer-create buffer)
	  (epoxide-emacs-buffer--add-output buffer output))))))

(defun epoxide-emacs-buffer--add-output (buffer output)
  "Modify BUFFER's local variable to notify OUTPUT on change.

Create buffer local variable `epoxide-emacs-buffer-outputs' in
order to contain output buffers that need to be notified when a
change occurred."
  (with-current-buffer buffer
    (unless (boundp 'epoxide-emacs-buffer-outputs)
      (set (make-local-variable 'epoxide-emacs-buffer-outputs) nil))
    (setq-local epoxide-emacs-buffer-outputs
		(delete-dups (cons output epoxide-emacs-buffer-outputs)))
    (add-hook 'after-change-functions
	      'epoxide-emacs-buffer-copy-buffer-content t t)))

(defun epoxide-emacs-buffer-exec ()
  "Dummy function."
  nil)

(defun epoxide-emacs-buffer-stop ()
  "Clear notification of outputs.
Remove hook function when no output is listening to the changes."
  (let ((buffers
	 (delq nil (delete-dups (append epoxide-emacs-buffer-current-buffers
					(epoxide-node-get-config)))))
	(outputs epoxide-node-outputs))
    (dolist (b buffers)
      (when (buffer-live-p (get-buffer b))
	(with-current-buffer b
	  (dolist (o outputs)
	    (when (boundp 'epoxide-emacs-buffer-outputs)
	      (setq-local epoxide-emacs-buffer-outputs
			  (delete o epoxide-emacs-buffer-outputs))
	      (epoxide-emacs-buffer-remove-hook))))))))

(provide 'emacs-buffer)

;;; Emacs-Buffer.el ends here
