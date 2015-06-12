;;; Dpids-odl.el --- EPOXIDE DPIDs OpenDaylight node definition file

;; Copyright (C) 2014-2015 István Pelle

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

;; Node collects DPID information from an OpenDaylight controller.
;; It has one input that should be an output of a Clock node.
;; It takes one configuration parameter: the IP address of the host where the
;; controller is running at.
;; The node has one output that lists the available DPIDs.  DPIDs are also
;; passed to the EPOXIDE framework's aggregation variables even when the
;; output is disabled.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-input-marker)
  (defvar epoxide-previous-dpids)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defun epoxide-dpids-odl-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . nil))))

(defun epoxide-dpids-odl-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "IP address of controller")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-p))))

(defun epoxide-dpids-odl-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "dpids"))))

(defun epoxide-dpids-odl-init ()
  "Initialize variables."
  (set (make-local-variable 'epoxide-input-marker) 1)
  (set (make-local-variable 'epoxide-previous-dpids) nil))

(defun epoxide-dpids-odl-exec ()
  "Query an OpenDaylight controller for DPIDs."
  (let ((marker epoxide-input-marker)
	(clock (nth 0 epoxide-node-inputs))
	(host (nth 0 epoxide-node-config-list))
	(username (nth 1 epoxide-node-config-list))
	(password (nth 2 epoxide-node-config-list))
	(dpids-out (nth 0 epoxide-node-outputs))
	(prev-dpids epoxide-previous-dpids)
	enable dpids
	dpids-to-remove dpids-to-add)
    (when (and clock host username password dpids-out)
      ;; Check changes in the input buffer.
      (with-current-buffer clock
      	(when (< marker (point-max))
      	  (setq enable t)
      	  (setq marker (point-max))))
      (when enable
	(setq-local epoxide-input-marker marker)
	(setq dpids (epoxide-dpids-odl-query host username password))
	(when dpids
	  ;; Notify the framework of the changes.
	  (with-current-buffer epoxide-root-buffer
	    (epoxide-change-list 'epoxide-dpids
                                 (epoxide-substract dpids prev-dpids)
                                 (epoxide-substract prev-dpids dpids)))
	  (setq-local epoxide-previous-dpids dpids)
	  ;; Write outputs.
	  (epoxide-write-node-output
	   (concat (mapconcat 'identity dpids ",") "\n") dpids-out))))))

(defun epoxide-dpids-odl-stop ()
  "Dummy function."
  nil)

(defun epoxide-dpids-odl-query (host username password)
  "Query available DPIDs from an OpenDaylight controller running at HOST.

Use USERNAME and PASSWORD for authentication.  Queried data is returned as a
list."
  (let* ((url (concat "http://" host
		      ":8080/controller/nb/v2/statistics/default/flow"))
	 (url-request-method "GET")
	 (url-request-extra-headers
	  `(("Authorization" . ,(concat "Basic "
					(base64-encode-string
					 (concat username ":" password))))))
	 (url-request-data)
	 (url-show-status nil)
	 (result-buffer (condition-case nil
			    (url-retrieve-synchronously url)
			(error nil)))
	 json)
    (with-current-buffer result-buffer
      (when (> (point-max) 1)
	(goto-char url-http-end-of-headers)
	(setq json (json-read-from-string (buffer-substring-no-properties
					   (point) (point-max)))))
      (kill-buffer))
    (let* ((all-flow-stats (cdr (assoc 'flowStatistics json)))
	   (number-of-switches (length all-flow-stats))
	   (i 0)
	   dpids)
      (while (< i number-of-switches)
	(setq dpids (cons (cdr (assoc 'id (cdr (assoc 'node
						 (aref all-flow-stats i)))))
			  dpids))
	(incf i))
      dpids)))

(provide 'dpids-odl)

;;; Dpids-odl.el ends here
