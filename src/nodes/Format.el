;;; Format.el --- EPOXIDE Format node definition file

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

;; The node is able to format strings coming in on its inputs.  The
;; node works like the `format' elisp function but it only accepts
;; string control symbols (since everything arriving on the inputs is
;; string).

;; The node accepts indefinite number of inputs that is processes
;; according to its configuration.  The node has an indefinite number
;; of configuration arguments, the first of which should be the format
;; string the rest should define the order of the inputs to pass to
;; the format function (e.g. listing 0, 2, 1 would pass inputs #0, #2
;; and #1 to the format function in this order). If no special input
;; order is given, the standard input order is used (i.e. input #0 is
;; passed as the first argument, input #1 as second and so on). If
;; less input is connected then required by the format string then the
;; unspecified inputs are initialized to empty strings.  If applying the
;; format function signals an error, it is written into the messages
;; buffer.  The node's only output displays the formatted text.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs))

(defun epoxide-format-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "arguments to format..."))))

(defun epoxide-format-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "format string"))
    ((doc-string . "inputs as arguments..."))))

(defun epoxide-format-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "formatted expression"))))

(defun epoxide-format-init ()
  "Dummy function."
  nil)

(defun epoxide-format-exec ()
  "Format input strings."
  (let* ((inputs (epoxide-node-get-inputs-as-list
		  (epoxide-node-read-inputs t)))
	 (config (copy-sequence epoxide-node-config-list))
	 (format-string (pop config))
	 (count (epoxide-format--count-control-symbols format-string))
	 tmp output)
    (when (and (delq nil inputs) format-string)
      (dolist (i inputs)
	(when i
	  (push (car (last (split-string (epoxide-chomp i) "\n"))) tmp)))
      (setq tmp (nreverse tmp))
      (if (null config)
      	  (setq inputs tmp)
	(setq inputs nil)
      	(dolist (c config)
      	  (push (nth (string-to-number c) tmp) inputs))
      	(setq inputs (nreverse inputs)))
      ;; When there are less inputs then required by the format
      ;; string, fill the rest up with empty strings.
      (setq tmp (- count (length inputs)))
      (when (> tmp 0)
      	(setq inputs (append inputs (make-list tmp ""))))
      (condition-case err
	  (progn
	    (setq output (apply 'format (cons format-string inputs)))
	    (when (> (length output) 0)
	      (epoxide-write-node-output
	       (concat output "\n")
	       (car epoxide-node-outputs))))
	(error
	 (epoxide-log (format "Epoxide node error in %s::%s: %s"
			      epoxide-node-name
			      epoxide-node-class
			      (error-message-string err))))))))

(defun epoxide-format--count-control-symbols (text)
  "Count format control symbols in TEXT."
  (let ((control-symbols '("%s" "%d" "%o" "%x" "%e" "%f" "%g" "%c"))
	(count 0))
    (with-temp-buffer
      (insert text)
      (dolist (s control-symbols)
	(incf count (how-many s (point-min) (point-max)))))
    count))

(defun epoxide-format-stop ()
  "Dummy function."
  nil)

(provide 'format)

;;; Format.el ends here
