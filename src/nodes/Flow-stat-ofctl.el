;;; Flow-stat-ofctl.el --- EPOXIDE Flow stat OFCTL node definition file

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

;; The node queries flow statistics for a specific switch name by way of using
;; `ovs-ofctl dump-flows' shell command.  After data is received it goes through
;; some modifications that replace proprietary parameter naming to EPOXIDE
;; standard.
;; It has one input that should be the output of a Clock node.
;; It takes two configuration parameters: #0 is the connection inforation, #1 is
;; the name of the switch to query.  Connection information could be 127.0.0.1
;; when asking for a local query and <username>@<IP address> or whatever alias
;; there is in your SSH configuration file for remote connections.
;; The node has one output that lists the received flow statistics.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-input-marker)
  (defvar epoxide-node-prev-conf-list))

(defvar epoxide-flow-stat-ofctl-translation-table
  `(("dl_src" . ,epoxide-l2-source)
    ("dl_dst" . ,epoxide-l2-destination)
    ("nw_src" . ,epoxide-l3-source)
    ("nw_dst" . ,epoxide-l3-destination)
    ("vlan_tci" . ,epoxide-vlan-id)
    ("duration" . ,epoxide-duration)
    ("n_bytes" . ,epoxide-byte-count)
    ("n_packets" . ,epoxide-packet-count)
    ("table" . ,epoxide-table-id))
  "Define phrases to change in original query output.")

(defun epoxide-flow-stat-ofctl-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock")
     (value-helper . epoxide-list-clock-output-buffers)
     (value-validator . epoxide-output-buffer-p))))

(defun epoxide-flow-stat-ofctl-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "connection information")
     (value-helper . nil)
     (value-validator . nil))
    ((doc-string . "switch name")
     (value-helper . epoxide-list-switch-names)
     (value-validator . epoxide-switch-name-p))))

(defun epoxide-flow-stat-ofctl-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "flow stats of a datapath"))))

(defun epoxide-flow-stat-ofctl-init ()
  "Initialize variables."
  (set (make-local-variable 'epoxide-input-marker) 1)
  (set (make-local-variable 'epoxide-node-prev-conf-list) nil))

(defun epoxide-flow-stat-ofctl-exec ()
  "Connect to a host and query it for flow statistics using ovs-ofctl dump-dps."
  (let ((marker epoxide-input-marker)
	(clock (nth 0 epoxide-node-inputs))
	(host (nth 0 epoxide-node-config-list))
	(switch-name (nth 1 epoxide-node-config-list))
	(flow-stat-out (nth 0 epoxide-node-outputs))
	enable results parts name line lines pos part-1)
    (unless (eq epoxide-node-prev-conf-list epoxide-node-config-list)
      ;; configuration parameters have changed
      (setq epoxide-node-prev-conf-list epoxide-node-config-list))
    (when (and clock host switch-name flow-stat-out)
      ;; Check clock.
      (with-current-buffer clock
      	(when (< marker (point-max))
      	  (setq enable t)
      	  (setq marker (point-max))))
      (when enable
	(setq-local epoxide-input-marker marker)
	;; Initate query.
	(setq results (epoxide-shell-command-to-string
                       host
		       (concat "ovs-ofctl dump-flows " switch-name)))
	(when results
	  (dolist (line (split-string results "\n"))
	    ;; Filter out the reply notification -- it's not needed.
	    (unless (or (string-match "NXST_FLOW reply" line)
			(equal line ""))
	      ;; Assign switch name to stats.
	      (setq line (concat "dpid=" switch-name "," line))
	      (when (setq pos (string-match "actions=" line))
		(setq part-1 (concat (substring line 0 (1- pos)) ", actions="))
		(setq line (substring line pos))
		(setq line (replace-regexp-in-string
			    "," " "
			    (substring line (1+ (string-match "=" line)))))
		(setq line (concat part-1 line)))
	      (dolist (phrase epoxide-flow-stat-ofctl-translation-table)
		(setq line
		      (replace-regexp-in-string (car phrase) (cdr phrase)
						line)))
	      (setq lines (cons line lines))))
	  (setq lines (nreverse lines))
	  ;; Write output.
	  (epoxide-write-node-output (concat
				      (mapconcat 'identity lines "\n") "\n")
				     flow-stat-out))))))

(defun epoxide-flow-stat-ofctl-stop ()
  "Dummy function."
  nil)

(provide 'flow-stat-ofctl)

;;; Flow-stat-ofctl.el ends here
