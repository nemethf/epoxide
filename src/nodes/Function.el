;;; Function.el --- EPOXIDE Function node definition file

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

;; The node is able to call elisp functions or lambda expressions.

;; The node has one input that serves as an enabling input, when it
;; changes the function is called.  The node has an indefinite number
;; of configuration arguments the first of which should be the name of
;; an elisp function or an elisp lambda expression.  The rest of the
;; config arguments are passed to the function when it is called.  By
;; default all configuration arguments are processed as string, in
;; case of a different type is required it has to be directly
;; specified by appending the type name and a colon (:) before the
;; argument.  In case the argument is dynamic, the type inscription is
;; still taken into account.  Possible types are: number, symbol and
;; sequence.  The configuration argument can also specify an input,
;; and how to process it.  Specifying the input can be done by using
;; the 'input-<x> formula, where <x> is the 0 based number of the node
;; input.  The processing method can be added to the end of the
;; configuration argument with a colon.  The default method is bulk
;; processing (where everything is used that changed from the last
;; reading cycle). The other method takes only the last line of the
;; input.  In this latter case the anything (except "bulk") can be
;; written after the colon to enable the processing method.  The node
;; has one output where it displays the result of the function call
;; converted to string.  If the function call fails, an error message
;; is written to the messages buffer.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs))

(defun epoxide-function-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-function-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "function name or lambda expression"))
    ((doc-string . "arguments..."))))

(defun epoxide-function-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "function output"))))

(defun epoxide-function-init ()
  "Dummy function."
  nil)

(defun epoxide-function-exec ()
  "Function input strings."
  (let* ((inputs (epoxide-node-get-inputs-as-list
		  (epoxide-node-read-inputs t)))
	 (config (copy-sequence (epoxide-node-get-config)))
	 (function (pop config))
	 (config (epoxide-function--convert-arguments config inputs))
	 output)
    (when (and (car inputs) function)
      (condition-case err
    	  (setq output (epoxide-eval-function function config))
    	(error
    	 (epoxide-log (format "Epoxide node error in %s::%s: %s"
    			      epoxide-node-name
    			      epoxide-node-class
    			      (error-message-string err)))))
      (when output
    	(epoxide-write-node-output (format "%s\n" output)
				   (car epoxide-node-outputs))))))

(defun epoxide-function--convert-arguments (config inputs)
  "Convert the node's CONFIG arguments based on their type inscription.

When a config parameter specifies a input process it from INPUTS."
  (let ((static-config (cdr (copy-sequence epoxide-node-config-list)))
	(type-list '("number" "symbol" "sequence"))
	types ret)
    (dolist (i static-config)
      (let ((type (car (split-string i ":"))))
	(if (member type type-list)
	    (push type types)
	  (push nil types))))
    (setq types (nreverse types))
    (dolist (c config)
      (when (and (equal c (car static-config))
		 (car types))
      	(setq c (substring c (1+ (length (car types))))))
      (setq c (epoxide-function--interpret-argument c inputs))
      (push
       (pcase (car types)
	 ("number"
	  (string-to-number c))
	 ("symbol"
	  (intern c))
	 ("sequence"
	  (car (read-from-string c)))
	 (other-type
	  c)) ret)
      (pop types)
      (pop static-config))
    (nreverse ret)))

(defun epoxide-function--interpret-argument (arg inputs)
  "Interpret node configuration argument ARG.

ARG can specify a standard configuration argument or one of the
node's inputs from INPUTS.  If an input is specified, one can set
the method of how to process it: either using the last line or in
a bulk."
  (let ((len (length "'input-")))
    (if (not (and (> (length arg) len)
		  (equal (substring arg 0 len) "'input-")))
	arg
      (let* ((input (substring arg len))
	     (pos (string-match ":" input))
	     (input (if pos
			(substring input 0 pos)
		      input))
	     (input (string-to-number input))
	     (line-processing
	      (if pos
		  (if (equal (substring arg (+ pos len 1)) "bulk")
		      nil
		    t)
		nil))
	     (ret (or (nth input inputs) ""))
	     (ret (if line-processing
		      (car (last (split-string ret "\n")))
		    ret)))
	ret))))

(defun epoxide-function-stop ()
  "Dummy function."
  nil)

(provide 'function)

;;; Function.el ends here
