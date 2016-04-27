;;; Dpids-pox.el --- EPOXIDE DPIDs POX node definition file

;; Copyright (C) 2014-2016 István Pelle

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

;; Node collects DPID and switch name information from a POX controller.
;; It has one input that should be an output of a Clock node.
;; It takes one configuration parameter: the IP address of the host where the
;; POX controller is running at.
;; The node has two optional outputs.  Output 0 lists the available DPIDs
;; while output 1 lists switch names.  DPIDs and switch names are passed to
;; the EPOXIDE frameworks aggregation variables even when outputs are
;; disabled.
;; DPID and switch name queries are done by accessing POX's web service.

;;; Code:

(require 'epoxide)
(require 'url)     ; For accessing REST APIs.

(eval-when-compile
  (defvar epoxide-previous-dpids)
  (defvar epoxide-previous-switch-names)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-previous-names))

(defun epoxide-dpids-pox-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . nil))))

(defun epoxide-dpids-pox-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "IP address of controller")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-p))))

(defun epoxide-dpids-pox-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "dpids"))
    ((doc-string . "switch names"))))

(defun epoxide-dpids-pox-init ()
  "Initialize variables."
  (set (make-local-variable 'epoxide-previous-dpids) nil)
  (set (make-local-variable 'epoxide-previous-switch-names) nil))

(defun epoxide-dpids-pox-exec ()
  "Query a POX controller for DPIDs and switch names."
  (let* ((clock (nth 0 epoxide-node-inputs))
	 (host (nth 0 epoxide-node-config-list))
	 (dpids-out (nth 0 epoxide-node-outputs))
	 (names-out (nth 1 epoxide-node-outputs))
	 (i 0)
	 (prev-dpids epoxide-previous-dpids)
	 (prev-names epoxide-previous-switch-names)
	 (enable (epoxide-node-enable-input-active
	  	  (epoxide-node-read-inputs) clock))
	 results dpids names
	 dpids-to-remove dpids-to-add
	 names-to-remove names-to-add)
    (when (and clock host (or dpids-out names-out))
      (when enable
	(setq results (epoxide-dpids--query host))
	(when results
	  (setq dpids (mapcar (lambda (result)
				(cdr (assoc 'dpid result))) results))
	  (setq names (mapcar (lambda (result)
				(cdr (assoc 'name result))) results))
	  ;; Notify the framework of the changes.
	  (with-current-buffer epoxide-root-buffer
	    (epoxide-change-list 'epoxide-dpids
                                 (epoxide-substract dpids prev-dpids)
                                 (epoxide-substract prev-dpids dpids))
	    (epoxide-change-list 'epoxide-switch-names
                                 (epoxide-substract names prev-names)
                                 (epoxide-substract prev-names names)))
	  (setq-local epoxide-previous-dpids dpids)
	  (setq-local epoxide-previous-names names)
	  ;; Write outputs.
	  (epoxide-write-node-output
	   (concat (mapconcat 'identity dpids ",") "\n") dpids-out)
	  (epoxide-write-node-output
	   (concat (mapconcat 'identity names ",") "\n") names-out))))))

(defun epoxide-dpids-pox-stop ()
  "Dummy function."
  nil)

(defun epoxide-dpids--query (host)
  "Query POX for a list of properties of connected network elements.

Create a JSON message, then use it in an HTTP POST request.
HOST: IP address of the host where POX is running at."
  (setq-local url-show-status nil)
  (let* ((json-message (json-encode (json-add-to-object
				     (json-add-to-object nil "id" "0")
				     "method" "get_switches")))
	 (url (concat "http://" host ":8000/OF/"))
	 (url-request-method "POST")
	 (url-request-extra-headers)
	 (url-request-data json-message)
	 (result-buffer (url-retrieve-synchronously url))
	 (switches-in-json (with-current-buffer result-buffer
			     (goto-char (point-min))
			     (when (> (point-max) 1)
			       (condition-case nil
				   (progn
				     (re-search-forward "{" (point-max) t)
				     (json-read-from-string
				      (buffer-substring-no-properties
				       (- (point) 1) (point-max))))
				 (error nil)))))
	 (number-of-switches (if (> (length switches-in-json) 0)
				 (length (cdr (assoc 'result switches-in-json)))
			       0))
	 (results (if (> (length switches-in-json) 0)
		      (cdr (assoc 'result switches-in-json))))
	 (switches)
	 (dpid)
	 (name)
	 (len)
	 (i 0))
    (while (< i number-of-switches)
      (setq dpid (assoc 'dpid (aref results i)))
      (setq len (- (length (cdr (assoc 'ports (aref results i)))) 1))
      (setq name (assoc 'name (aref (cdr (assoc 'ports (aref results i))) len)))
      (setq switches (cons (list name dpid) switches))
      (setq i (1+ i)))
    (kill-buffer result-buffer)
    (nreverse switches)))

(provide 'dpids-pox)

;;; Dpids-pox.el ends here
