;;; Flow-stat-pox.el --- EPOXIDE Flow stat POX node definition file

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

;; This node provides functionality to query a POX controller for flow
;; statistics of a specific switch given with its DPID.
;; The node has one input that should be an output buffer of a Clock node.
;; It takes two configuration parameters.  #0 specifies the IP address of the
;; where the controller is running at and #1 is the DPID of interest.
;; The node has one output that lists queried flow statistics.
;; The node connects to the specified POX controller via its web service.
;; Received data is then processed by giving standard names to the parameters.

;;; Code:

(require 'epoxide)
(require 'url)     ; For accessing REST APIs.

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs)
  (defvar epoxide-node-inputs))

(defvar epoxide-flow-stat-pox-translation-table
  `(("dl_src" . ,epoxide-l2-source)
    ("dl_dst" . ,epoxide-l2-destination)
    ("nw_src" . ,epoxide-l3-source)
    ("nw_dst" . ,epoxide-l3-destination)
    ("dl_vlan" . ,epoxide-vlan-id)
    ("nw_proto" . ,epoxide-l3-proto)
    ("dl_type" . ,epoxide-l2-type)
    ("max_len" . ,epoxide-max-length))
  "Define phrases to change in original query output.")

(defun epoxide-flow-stat-pox-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . epoxide-output-buffer-p))))

(defun epoxide-flow-stat-pox-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "IP address of controller")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-p))
    ((doc-string . "DPID")
     (value-helper . epoxide-list-dpids)
     (value-validator . epoxide-dpid-p))))

(defun epoxide-flow-stat-pox-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "flow stats of a datapath"))))

(defun epoxide-flow-stat-pox-init ()
  "Perform node initialization."
  nil)

(defun epoxide-flow-stat-pox-exec ()
  "Query a POX controller for the flow stats of a specified DPID."
  (let* ((clock (nth 0 epoxide-node-inputs))
	 (host (nth 0 epoxide-node-config-list))
	 (dpid (nth 1 epoxide-node-config-list))
	 (flow-stat-out (nth 0 epoxide-node-outputs))
	 (i 0)
	 (enable (epoxide-node-enable-input-active
		  (epoxide-node-read-inputs) clock))
	 json result)
    (when (and clock host dpid flow-stat-out)
      (when enable
	(setq json (epoxide-flow-stat-pox--query-flow-stats dpid host))
	;; Rename parameters to have standard names.
	(with-temp-buffer
	  (while (< i (length json))
	    (epoxide-json-insert-flowstat-helper (aref json i) (current-buffer))
	    ;; Assign dpid to stats.
	    (goto-char (line-beginning-position))
	    (insert "dpid=" dpid ",")
	    (goto-char (line-end-position))
	    (delete-char -1)
	    (insert "\n")
	    (incf i))
	  (setq result (buffer-substring-no-properties (point-min) (point-max)))
	  (dolist (phrase epoxide-flow-stat-pox-translation-table)
	    (setq result (replace-regexp-in-string
			  (car phrase) (cdr phrase) result))))
	;; Write output.
	(epoxide-write-node-output result flow-stat-out)))))

(defun epoxide-flow-stat-pox-stop ()
  "Dummy function."
  nil)

(defun epoxide-flow-stat-pox--query-flow-stats (dpid host)
  "Query POX for flow table of a switch.

Create a JSON message, then use it in an HTTP POST request.
DPID: datapath ID of the requested switch.
HOST: IP address of the host where POX is running at."
  (setq-local url-show-status nil)
  (let* ((json-message (json-encode
			(reverse (json-add-to-object
				  (reverse
				   (json-add-to-object
				    (json-add-to-object
				     nil
				     "params"
				     (json-add-to-object nil "dpid" dpid))
				    "method" "get_flow_stats")) "id" "0"))))
	 (url (concat "http://" host ":8000/OF/"))
	 (url-request-method "POST")
	 (url-request-extra-headers)
	 (url-request-data json-message)
	 (result-buffer (url-retrieve-synchronously url))
	 json)
    (with-current-buffer result-buffer
      (goto-char (point-min))
      (condition-case nil
	  (progn
	    (re-search-forward "{" (point-max) t)
	    (setq json (cdr
			(assoc 'flowstats
			       (cdr
				(assoc 'result
				       (json-read-from-string
					(buffer-substring-no-properties
					 (- (point) 1) (point-max)))))))))
	(error (message "Problem parsing POX flow statistics.")))
      (kill-buffer)
      json)))

(provide 'flow-stat-pox)

;;; Flow-stat-pox.el ends here
