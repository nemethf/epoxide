;;; Dpids-fl.el --- EPOXIDE DPIDs Floodlight node definition file

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

;; Node collects DPID information from a Floodlight controller.
;; It has one input that should be an output of a Clock node.
;; It takes one configuration parameter: the IP address of the host where the
;; POX controller is running at.
;; The node has one output that lists the available DPIDs.  DPIDs are passed to
;; the EPOXIDE framework's aggregation variables even when the output is
;; disabled.
;; DPID queries are done by using Floodlight's REST API.

;;; Code:

(require 'epoxide)
(require 'url)     ; For accessing REST APIs.

(eval-when-compile
  (defvar epoxide-previous-dpids)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defun epoxide-dpids-fl-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . nil))))

(defun epoxide-dpids-fl-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "IP address of controller")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-p))))

(defun epoxide-dpids-fl-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "dpids"))))

(defun epoxide-dpids-fl-init ()
  "Initialize variables."
  (set (make-local-variable 'epoxide-previous-dpids) nil))

(defun epoxide-dpids-fl-exec ()
  "Query a Floodlight controller for DPIDs."
  (let* ((clock (nth 0 epoxide-node-inputs))
	 (host (nth 0 epoxide-node-config-list))
	 (dpids-out (nth 0 epoxide-node-outputs))
	 (prev-dpids epoxide-previous-dpids)
	 (enable (epoxide-node-enable-input-active
	  	  (epoxide-node-read-inputs) clock))
	 results dpids
	 dpids-to-remove dpids-to-add)
    (when (and clock host dpids-out)
      (when enable
	(setq results (epoxide-dpids-fl-query host))
	(when results
	  (setq dpids (mapcar (lambda (result)
				(cdr (assoc 'dpid result))) results))
	  ;; Notify the framework of the changes.
	  (with-current-buffer epoxide-root-buffer
	    (epoxide-change-list 'epoxide-dpids
                                 (epoxide-substract dpids prev-dpids)
                                 (epoxide-substract prev-dpids dpids)))
	  (setq-local epoxide-previous-dpids dpids)
	  ;; Write outputs.
	  (epoxide-write-node-output
	   (concat (mapconcat 'identity dpids ",") "\n") dpids-out))))))

(defun epoxide-dpids-fl-stop ()
  "Dummy function."
  nil)

(defun epoxide-dpids-fl-query (host)
  "Query a Floodlight controller using its REST API for the connected DPIDs.

Return them as an alist of dpids and IP addresses.
HOST: IP address of the host where the Floodlight controller is running at."
  (setq-local url-show-status nil)
  (let* ((url (concat "http://" host
		      ":8080/wm/core/controller/switches/json"))
	 (url-request-method "GET")
	 (url-request-extra-headers)
	 (url-request-data nil)
	 (result-buffer (condition-case nil
			    (url-retrieve-synchronously url)
			  (error nil)))
	 (all-data (with-current-buffer result-buffer
		     (when (> (point-max) 1)
		       (goto-char url-http-end-of-headers)
		       (json-read-from-string
			(buffer-substring-no-properties
			 (point) (point-max))))))
	 (number-of-switches (length all-data))
	 (i 0)
	 dpid ip-address switches)
    (kill-buffer result-buffer)
    (while (< i number-of-switches)
      (setq dpid (cdr (assoc 'switchDPID (aref all-data i))))
      (setq ip-address (cdr (assoc 'inetAddress (aref all-data i))))
      (when (> (length ip-address) 0)
	(setq ip-address (substring ip-address 1)))
      (setq switches (append (list `((dpid . ,dpid)
				     (ip-address . ,ip-address)))
			     switches))
      (setq dpid nil)
      (setq ip-address nil)
      (incf i))
    (nreverse switches)))

(provide 'dpids-fl)

;;; Dpids-fl.el ends here
