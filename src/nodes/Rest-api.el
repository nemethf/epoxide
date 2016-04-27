;;; Rest-api.el --- EPOXIDE Rest-api node definition file

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

;; The node is able to make a REST API call and display its results on
;; its output.

;; The node has one input that serves as an enabling input: each time
;; this input is updated, the API call is performed.  The node has six
;; configuration arguments: the first and second define respectively
;; the host and port where the REST API accessible at; the third
;; specifies the API call to be made (that appears as part of the
;; URL); the optional fourth argument is a message.  If the fourth
;; configuration argument is present an HTTP POST message is sent
;; relaying the message with the POST request.  If this argument is
;; not present an HTTP GET message is sent.  The fifth argument
;; specifies the Content-Type extra header.  If the sixth argument is
;; non-nil, an HTTP PUT message is sent.  The node has one output
;; argument where it displays the result of the query as JSON
;; formatted string.

;;; Code:

(require 'epoxide)
(require 'url)     ; For accessing REST APIs.

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defun epoxide-rest-api-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-rest-api-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host where REST API runs at"))
    ((doc-string . "port where REST API is available at"))
    ((doc-string . "REST API"))
    ((doc-string . "message"))
    ((doc-string . "Content-Type (nil to ignore)"))
    ((doc-string . "use HTTP put (if non-nil)"))))

(defun epoxide-rest-api-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "results"))))

(defun epoxide-rest-api-init ()
  "Dummy function."
  nil)

(defun epoxide-rest-api-exec ()
  "Execute REST API call and display data."
  (let* ((enable (epoxide-node-enable-input-active
	  	  (epoxide-node-read-inputs) 0))
	 (config (copy-sequence (epoxide-node-get-config)))
	 (host (pop config))
	 (port (pop config))
	 (rest-api (pop config))
	 (message (pop config))
	 (content-type (pop config))
	 (put (pop config)))
    (when (and host (> (length host) 0) rest-api enable)
      (let* ((output (epoxide-rest-api--call host rest-api message port
					     content-type put))
    	     (len (length output))
    	     (output (when (> len 0)
		      (if (equal (substring output (1- len) len) "\n")
			  output
			(concat output "\n")))))
    	(epoxide-write-node-output
    	 output (car epoxide-node-outputs))))))

(defun epoxide-rest-api--call (host rest-api &optional message port
				    content-type put)
  "Connect to HOST and call REST-API and return the results.

When optional argument MESSAGE is present a POST HTTP request is
made and the message is sent with the call.  Optional argument
PORT can specify a proprietary port where the REST API is
accessible at.  Optional argument CONTENT-TYPE specifies the
Content-Type extra header.  If optional argument PUT is non-nil,
use the HTTP PUT method to send MESSAGE."
  (setq-local url-show-status nil)
  (let* ((url (concat "http://" host (if port
  					 (concat ":" port)
  				       "")
  		      rest-api))
  	 (url-request-method (if put
				 "PUT"
			       (if message
				   "POST"
				 "GET")))
  	 (url-request-extra-headers
	  (when content-type
	    `(("Content-Type" . ,content-type))))
  	 (url-request-data message)
  	 (result-buffer (url-retrieve-synchronously url))
  	 (result (with-current-buffer result-buffer
  		   (goto-char (point-min))
  		   (when (> (point-max) 2)
  		     (condition-case nil
  			 (progn
  			   (goto-char url-http-end-of-headers)
  			    (buffer-substring-no-properties
  			     (1+ (point) ) (point-max)))
  		       (error nil))))))
    (kill-buffer result-buffer)
    result))

(defun epoxide-rest-api-stop ()
  "Dummy function."
  nil)

(provide 'rest-api)

;;; Rest-api.el ends here
