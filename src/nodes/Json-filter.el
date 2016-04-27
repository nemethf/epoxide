;;; Json-filter.el --- EPOXIDE Json-Filter node definition file

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
;; find all values associated with a given key.

;; The node has one input that supplies the JSON expressions to be
;; processed.  It has one configuration argument that specifies the key
;; to be looked up and one output where the result is displayed.  Only
;; exact matches with the specified key result in successful
;; matches.  Results are relayed to the output as JSON formatted
;; objects without the keys.  When multiple occurences of the same key
;; is found, a JSON array is created.

;;; Code:

(require 'epoxide)
(require 'json)

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs))

(defun epoxide-json-filter-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "JSON expression"))))

(defun epoxide-json-filter-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "filter key"))))

(defun epoxide-json-filter-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "filtered results"))))

(defun epoxide-json-filter-init ()
  "Dummy function."
  nil)

(defun epoxide-json-filter-exec ()
  "Execute filter and send results to the output."
  (let* ((input (car (epoxide-node-get-inputs-as-list
		      (epoxide-node-read-inputs))))
	 (json (if (> (length input) 0)
		   (condition-case nil
		       (json-read-from-string input)
		     (error nil))
		 nil))
	 (config (epoxide-node-get-config))
	 (key (intern (epoxide-node-get-config 0)))
	 (output (epoxide-json-filter--flatten
	 	  (epoxide-json-filter--filter json key)))
	 (output (if (< (length output) 2)
	 	     (car output)
	 	   (concat "[" (mapconcat 'identity output ",") "]"))))
    (when output
      (epoxide-write-node-output
       (concat output "\n")
       (car epoxide-node-outputs)))))

(defun epoxide-json-filter--flatten (list)
  "Flatten LIST of lists while creating JSON objects from inner expressions."
  (cond
   ((null list) nil)
   ((and (listp list)
	 (equal (car list) 'result))
    (list (json-encode (cdr list))))
   (t
    (append (epoxide-json-filter--flatten (car list))
	    (epoxide-json-filter--flatten (cdr list))))))

(defun epoxide-json-filter--filter (json key)
  "Iterate through JSON and values corresponding to KEY."
  (let (ret)
    (unless (or (stringp json)
		(symbolp json))
      (when (arrayp json)
	(setq json (append json nil)))
      (when (listp json)
	(if (equal (car json) key)
	    (push `(result . ,(cdr json)) ret)
	  (if (listp (cdr json))
	      (dolist (j json)
		(push (epoxide-json-filter--filter j key) ret))
	    (when (arrayp (cdr json))
	      (push (epoxide-json-filter--filter (car json) key) ret)
	      (push (epoxide-json-filter--filter (cdr json) key) ret))))))
    (delq nil ret)))

(defun epoxide-json-filter-stop ()
  "Dummy function."
  nil)

(provide 'json-filter)

;;; Json-filter.el ends here
