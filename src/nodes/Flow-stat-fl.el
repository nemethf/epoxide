;;; Flow-stat-fl.el -- EPOXIDE Flow stat Floodlight node definition file

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

;; This node provides functionality to query a Floodlight controller for flow
;; statistics of a specific switch given with its DPID.
;; The node has one input that should be an output buffer of a Clock node.
;; It takes three configuration parameters.  #0 specifies the IP address of the
;; host where the controller is running at, #1 is the DPID of interest and
;; optional #3 is the requested stat type.  This last argument defaults to
;; 'flow'.  Other possible values are: port, aggregate, desc, features and
;; queue.
;; The node has one output that lists queried flow statistics.
;; The node connects to the specified Floodlight controller via its REST API.
;; Received data is then processed by giving standard names to the parameters.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-input-marker)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defvar epoxide-flow-stat-fl-translation-table
  `(("eth_src" . ,epoxide-l2-source)
    ("eth_dst" . ,epoxide-l2-destination)
    ("ipv4_src" . ,epoxide-l3-source)
    ("ipv4_dst" . ,epoxide-l3-destination)
    ("eth_type" . ,epoxide-l2-type)
    ("tableID" . ,epoxide-table-id)
    ("durationSeconds" . ,epoxide-duration)
    ("byteCount" . ,epoxide-byte-count)
    ("packetCount" . ,epoxide-packet-count))
  "Define phrases to change in original query output.")

(defun epoxide-flow-stat-fl-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . epoxide-output-buffer-p))))

(defun epoxide-flow-stat-fl-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "IP address of controller")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-p))
    ((doc-string . "DPID")
     (value-helper . epoxide-list-dpids)
     (value-validator . epoxide-dpid-p))
    ((doc-string . "stat type")
     (value-helper . epoxide-flow-stat-fl-stat-types)
     (value-validator . epoxide-flow-stat-fl-stat-type-p))))

(defun epoxide-flow-stat-fl-stat-types ()
  "List possible flow stat types."
  '("flow" "port" "aggregate" "desc" "features" "queue"))

(defun epoxide-flow-stat-fl-stat-type-p (type)
  "Return t when TYPE is a valid stat type.  Return nil otherwise."
  (when (member type (epoxide-flow-stat-fl-stat-types))
    t))

(defun epoxide-flow-stat-fl-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "flow stats of a datapath"))))

(defun epoxide-flow-stat-fl-init ()
  "Initialize node's input marker."
  (set (make-local-variable 'epoxide-input-marker) 1))

(defun epoxide-flow-stat-fl-exec ()
  "Query a Floodlight controller for the flow stats of a specified DPID."
  (let ((marker epoxide-input-marker)
	(clock (nth 0 epoxide-node-inputs))
	(host (nth 0 epoxide-node-config-list))
	(dpid (nth 1 epoxide-node-config-list))
	(stat-type (if (nth 2 epoxide-node-config-list)
		       (nth 2 epoxide-node-config-list)
		     "flow"))
	(flow-stat-out (nth 0 epoxide-node-outputs))
	enable json result)
    (when (and clock host dpid flow-stat-out)
      ;; Check clock.
      (with-current-buffer clock
      	(when (< marker (point-max))
      	  (setq enable t)
      	  (setq marker (point-max))))
      (when enable
	(setq-local epoxide-input-marker marker)
	;; Rename parameters to have standard names.
	(with-temp-buffer
	  (epoxide-fl-json-insert-flowstat
	   dpid
	   (epoxide-fl-get-flow-stats dpid host stat-type)
	   (current-buffer))
	  (setq result (buffer-substring-no-properties (point-min) (point-max)))
	  (dolist (phrase epoxide-flow-stat-fl-translation-table)
	    (setq result (replace-regexp-in-string
			  (car phrase) (cdr phrase) result))))
	;; Write output.
	(epoxide-write-node-output result flow-stat-out)))))

(defun epoxide-flow-stat-fl-stop ()
  "Dummy function."
  nil)

(defun epoxide-fl-get-flow-stats (dpid host stat-type)
  "Query a Floodlight controller using its REST API for flow statistics.

DPID is the datapath ID DPID of the switch to be queried.
HOST ip address of the host where the Floodlight controller is running at.
STAT-TYPE is the stat type to be queried."
  (let* ((url (concat "http://" host
		      ":8080/wm/core/switch/" dpid "/"
		      stat-type "/json"))
	 (url-request-method "GET")
	 (url-request-extra-headers)
	 (url-request-data nil)
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
    json))

(defun epoxide-fl-json-insert-flowstat (dpid json to-buffer)
  "Process the given flow stat and insert it to a buffer.

DPID is inserted to mark the switch.
JSON is the flow stat to be inserted.
TO-BUFFER is the buffer where results will be inserted."
  (with-current-buffer to-buffer
    (let ((i 0))
      (while (< i (length (cdar json)))
	(cond
	 ((listp (cdar json))
	  (when (cdar json)
	    (insert "dpid=" dpid ",")
	    (epoxide-json-insert-flowstat-helper (cdar json)
						 to-buffer)
	    (delete-char -1)
	    (insert "\n")
	    (setq i (length (cdr (car json))))))
	 ((arrayp (cdar json))
	  (when (aref (cdar json) i)
	    (insert "dpid=" dpid ",")
	    (epoxide-json-insert-flowstat-helper (aref (cdar json) i)
						 to-buffer)
	    (delete-char -1)
	    (insert "\n"))))
	(incf i)))))

(provide 'flow-stat-fl)

;;; Flow-stat-fl.el ends here
