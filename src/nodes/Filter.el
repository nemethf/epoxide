;;; Filter.el --- EPOXIDE Filter node definition file

;; Copyright (C) 2015-2016 István Pelle
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

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs))

(defun epoxide-filter-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "input")
     (value-helper . epoxide-list-output-buffers)
     (value-validator . epoxide-output-buffer-p))))

(defun epoxide-filter-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "regular expression"))))

(defun epoxide-filter-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "lines that match the criterion are marked"))))

(defun epoxide-filter-init ()
  "Dummy function."
  nil)

(defun epoxide-filter-exec ()
  "Emphasize lines matching regular expression with italic font."
  (let* ((input (car (epoxide-node-get-inputs-as-list
		      (epoxide-node-read-inputs))))
	 (input (if input
		    (split-string input "\n")))
	(regexp (nth 0 epoxide-node-config-list))
	(output (nth 0 epoxide-node-outputs))
	(destructive (nth 1 epoxide-node-config-list)))
    (while input
      (if (string-match regexp (car input))
	  (epoxide-write-node-output (propertize (concat (car input) "\n")
						 'face 'italic)
				     output)
	(unless destructive
	  (epoxide-write-node-output (concat (car input) "\n") output)))
      (setq input (cdr input)))))

(defun epoxide-filter-stop ()
  "Dummy function."
  nil)

(provide 'filter)

;;; Filter.el ends here
