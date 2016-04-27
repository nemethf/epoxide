;;; Json-list-filter.el --- EPOXIDE Json-list-filter node definition file

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

;; The node is able to search through a JSON formatted expression and
;; find lists that contain a specific key-value pair.

;; The node has one input that supplies the JSON expressions to be
;; processed.  It has two configuration arguments and one output where
;; the result is displayed.  The first configuration argument
;; specifies a key the second a value, the node looks up those lists
;; that have a dictionary of {<config argument 1>:<config argument 2>}
;; as an item.  Results are relayed to the output as JSON formatted
;; objects.  When multiple occurrences of the same key-value pair is
;; found, a JSON array is created.

;;; Code:

(require 'epoxide)
(require 'json)

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-list-filter--match-list))

(defun epoxide-json-list-filter-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "JSON expression"))))

(defun epoxide-json-list-filter-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "filter key"))
    ((doc-string . "filter value"))))

(defun epoxide-json-list-filter-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "filtered results"))))

(defun epoxide-json-list-filter-init ()
  "Initialize local variables."
  (set (make-local-variable 'epoxide-list-filter--match-list) nil))

(defun epoxide-json-list-filter-exec ()
  "Execute filter and send results to the output."
  (let* ((input (car (epoxide-node-get-inputs-as-list
		      (epoxide-node-read-inputs))))
	 (json (if (> (length input) 0)
		   (condition-case nil
		       (json-read-from-string input)
		     (error nil))
		 nil))
	 (config (epoxide-node-get-config))
	 (key (intern (pop config)))
	 (value (pop config))
	 (output (when (and key value)
		   (epoxide-json-list-filter--filter-helper json key value)))
	 (output (when output
		   (if (< (length output) 2)
		       (json-encode (car output))
		     ;; Create a JSON array when more than one has been found.
		     (json-encode (vconcat output nil))))))
    (when output
      (epoxide-write-node-output
       (concat output "\n")
       (car epoxide-node-outputs)))))

(defun epoxide-json-list-filter--filter-helper (json key value)
  "Filter JSON based on KEY and VALUE.

Call `epoxide-json-list-filter--filter' end return
`epoxide-list-filter--match-list'."
  (setq-local epoxide-list-filter--match-list nil)
  (epoxide-json-list-filter--filter json key value)
  (nreverse epoxide-list-filter--match-list))

(defun epoxide-json-list-filter--filter (json key value)
  "Filter JSON based on KEY and VALUE.

Iterate through JSON and return those lists that have a
dictionary of {KEY:VALUE} as an item.  JSON should be a parsed
JSON expression, KEY should be a symbol and VALUE should be a
string.

Accumulate matching lists to `epoxide-list-filter--match-list'."
  (unless (or (stringp json)
	      (symbolp json))
    (when (arrayp json)
      (let ((i 0))
	(while (< i (length json))
	  (epoxide-json-list-filter--filter (aref json i)
					    key value)
	  (incf i))))
    (when (and (listp json)
	       (listp (cdr json)))
      (if (equal (cdr (assoc key json)) value)
	  (push (nreverse json) epoxide-list-filter--match-list)
	(epoxide-json-list-filter--filter (car json)
					  key value)
	(epoxide-json-list-filter--filter (cdr json)
					  key value)))))

(defun epoxide-json-list-filter-stop ()
  "Dummy function."
  nil)

(provide 'json-list-filter)

;;; Json-list-filter.el ends here
