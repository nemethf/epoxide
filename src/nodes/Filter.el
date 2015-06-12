;;; Filter.el --- EPOXIDE Filter node definition file

;; Copyright (C) 2015      István Pelle
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
  (defvar epoxide-input-marker)
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
  "Init filter's input marker"
  (make-local-variable 'epoxide-input-marker)
  (setq-local epoxide-input-marker 1) )

(defun epoxide-filter-exec ()
  "Emphasize lines matching regular expression with italic font"
  (let ((marker epoxide-input-marker)
	(name1 (epoxide-tsg-create-node-buffer-name
		epoxide-node-name
		epoxide-node-class))
	(regexp1 (nth 0 epoxide-node-config-list))
	(input1 (nth 0 epoxide-node-inputs))
	(output1 (nth 0 epoxide-node-outputs))
	(destructive1 (nth 1 epoxide-node-config-list))
        line)
    (with-current-buffer input1
      (save-excursion
	(goto-char marker)
	(while (< marker (point-max))
	  (setq line (thing-at-point 'line))
	  (if (string-match regexp1 line)
	      (epoxide-write-node-output (propertize line 'face 'italic)
					 output1)
	    (when (null destructive1)
	      (epoxide-write-node-output line output1)))
	  (forward-line)
	  (let ((in-pos (point)))
	    (setq marker (point))
	    (with-current-buffer name1
	      (setq-local epoxide-input-marker in-pos))))))))

(defun epoxide-filter-stop ()
  "Dummy function."
  nil)

(provide 'filter)

;;; Filter.el ends here
