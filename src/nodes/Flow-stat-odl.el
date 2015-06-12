;;; Flow-stat-odl.el -- EPOXIDE Flow stat OpenDaylight node definition file

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

;; This node provides functionality to query an OpenDaylight controller for
;; flow statistics of a specific switch given with its DPID.
;; The node has one input that should be an output buffer of a Clock node.
;; It takes four configuration parameters. #0 specifies the IP address of the
;; where the controller is running at, #1 is the DPID of interest and #3 is the
;; username and #4 is the password that enables a connection to be set up with
;; the controller 
;; The node has one output that lists queried flow statistics.
;; The node connects to the specified OpenDaylight controller via its REST API.
;; Received data is then processed by giving standard names to the parameters.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-input-marker)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defvar epoxide-flow-stat-odl-translation-table
  `(("nw_dst" . ,epoxide-l3-destination)
    ("eth_type" . ,epoxide-l2-type)
    ("tableId" . ,epoxide-table-id)
    ("durationSeconds" . ,epoxide-duration)
    ("byteCount" . ,epoxide-byte-count)
    ("packetCount" . ,epoxide-packet-count))
  "Define phrases to change in original query output.")

(defun epoxide-flow-stat-odl-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . epoxide-output-buffer-p))))

(defun epoxide-flow-stat-odl-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "IP address of controller")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-p))
    ((doc-string . "DPID")
     (value-helper . epoxide-list-dpids)
     (value-validator . epoxide-dpid-p))
    ((doc-string . "stat type")
     (value-helper . epoxide-flow-stat-odl-stat-types)
     (value-validator . epoxide-flow-stat-odl-stat-type-p))))

(defun epoxide-flow-stat-odl-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "flow stats of a datapath"))))

(defun epoxide-flow-stat-odl-init ()
  "Initialize the node's input marker."
  (set (make-local-variable 'epoxide-input-marker) 1))

(defun epoxide-flow-stat-odl-exec ()
  "Query an OpenDaylight controller for the flow stats of a specified DPID."
  (let ((marker epoxide-input-marker)
	(clock (nth 0 epoxide-node-inputs))
	(host (nth 0 epoxide-node-config-list))
	(dpid (nth 1 epoxide-node-config-list))
	(username (nth 2 epoxide-node-config-list))
	(password (nth 3 epoxide-node-config-list))
	(flow-stat-out (nth 0 epoxide-node-outputs))
	enable json result)
    (when (and clock host dpid username password flow-stat-out)
      ;; Check clock.
      (with-current-buffer clock
      	(when (< marker (point-max))
      	  (setq enable t)
      	  (setq marker (point-max))))
      (when enable
	(setq-local epoxide-input-marker marker)
	;; Rename parameters to have standard names.
	(with-temp-buffer
	  (epoxide-odl-json-insert-flowstat
	   dpid
	   (epoxide-odl-get-flow-stats dpid host username password)
	   (current-buffer))
	  (setq result (buffer-substring-no-properties (point-min) (point-max)))
	  (dolist (phrase epoxide-flow-stat-odl-translation-table)
	    (setq result (replace-regexp-in-string
			  (car phrase) (cdr phrase) result))))
	;; Write output.
	(epoxide-write-node-output result flow-stat-out)))))

(defun epoxide-flow-stat-odl-stop ()
  "Dummy function."
  nil)

(defun epoxide-odl-get-flow-stats (dpid host username password)
  "Query flow statistics of DPID from an OpenDaylight controller running at
HOST. Use USERNAME and PASSWORD for authentication.

Queried data is returned."
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
	   flow-stats)
      (while (< i number-of-switches)
	(when (equal dpid 
		     (cdr (assoc 'id (cdr (assoc 'node 
						 (aref all-flow-stats i))))))
	  (setq flow-stats 
		(cdr (assoc 'flowStatistic (aref all-flow-stats i)))))
	(incf i))
      flow-stats)))

(defun epoxide-odl-json-insert-flowstat (dpid json to-buffer)
  "Process the given JSON object for displaying, and insert it to TO-BUFFER.
Insert DPID too to identify the datapath."
  (with-current-buffer to-buffer
    (let ((i 0)
	  (id "type=NW_DST,value=")
	  data part-1 part-2)
      (while (< i (length json))
	(with-temp-buffer
	  (epoxide-json-insert-flowstat-helper (aref json i) (current-buffer))
	  (setq data
		(buffer-substring-no-properties (point-min) (point-max))))
	(when (string-match id data)
	  (setq part-1 (substring data 0 (string-match id data)))
	  (setq part-2 (substring data (+ (string-match id data) (length id))))
	  (setq data (concat part-1 "nw_dst=" part-2)))
	(when data
	  (insert (concat "dpid=" dpid "," data))
	  (delete-char -1)
	  (insert "\n"))
	(incf i)))))

(provide 'flow-stat-odl)

;;; Flow-stat-odl.el ends here
