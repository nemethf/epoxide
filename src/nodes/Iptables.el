;;; Iptables.el --- EPOXIDE Iptables node definition file

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

;; This node is a wrapper for the `iptables' shell command.

;; It has one input that serves as an enabling input.  Its first
;; configuration argument is mandatory that specifies the host where
;; the command should be run at.  When the second configuration argument
;; is present it should be a `;' separated list of those IP
;; addresses.  These are checked against the ACLs and those ACLs that
;; cover the specified IP addresses are kept in the output the rest
;; will not be displayed.  When this argument is non-nil, the
;; `iptables' command is always called with the `-L -n' options.  The
;; remaining configuration arguments are extra arguments to be passed
;; to the `iptables' command.  The node has one output where it
;; displays the (filtered) results of the `iptables' call.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-outputs))

(defun epoxide-iptables-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-iptables-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host"))
    ((doc-string . "IP address filter (`;' separated)"))
    ((doc-string . "extra arguments..."))))

(defun epoxide-iptables-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "iptables"))))

(defun epoxide-iptables-init ()
  "Dummy function."
  nil)

(defun epoxide-iptables-exec ()
  "Call`iptables'."
  (let* ((f (lambda (host command extra-args)
	      (epoxide-shell-command-to-string
	       host (mapconcat 'identity (cons command extra-args) " "))))
	 (config (copy-sequence (epoxide-node-get-config)))
	 (host (pop config))
	 (ip-addresses (pop config))
	 (ip-addresses (when ip-addresses
			 (split-string ip-addresses ";")))
	 (extra-arguments config)
	 (ret (when (epoxide-node-enable-input-active
		     (epoxide-node-read-inputs) 0)
		(if (null ip-addresses)
		    (funcall f host "iptables" extra-arguments)
		  (epoxide-iptables-collect-rules
		   (funcall f host "iptables -L -n" extra-arguments)
		   ip-addresses)))))
    (epoxide-write-node-output ret (car epoxide-node-outputs))))

(defun epoxide-iptables-collect-rules (iptables ip-addresses)
  "Collect ACLs from IPTABLES that cover IP-ADDRESSES.

IPTABLES is the output of the `iptables' shell call.
IP-ADDRESSES is a list of IP addresses that should be checked."
  (let* ((chains (split-string iptables "\n\n"))
	 rules ret)
    (dolist (c chains)
      (let* ((lines (split-string c "\n" t))
	     (chain-name-line (pop lines))
	     (chain-name (cadr (split-string chain-name-line " " t " ")))
	     (heading (pop lines))
	     chain-ret)
	(if (member chain-name rules)
	    (push c ret)
	  (dolist (l lines)
	    (let* ((data (split-string l " " t " "))
		   (rule-name (car data)))
	      (dolist (d data)
		(let* ((s (split-string d "/"))
		       (network (car s))
		       (prefix (cadr s))
		       (prefix-len (when prefix
				     (string-to-number prefix))))
		  (when (and (epoxide-ipv4-address-p network)
			     prefix-len
			     (>= prefix-len 0) (< prefix-len 33))
		    (dolist (ip-address ip-addresses)
		      (when (epoxide-ip-address-in-network
			      ip-address network prefix)
			(unless chain-ret
			  (push chain-name-line chain-ret)
			  (push heading chain-ret))
			(unless (member l chain-ret)
			  (push l chain-ret)
			  (push rule-name rules)
			  (return))))))))))
	(when chain-ret
	  (push (mapconcat 'identity (nreverse chain-ret) "\n") ret))))
    (mapconcat 'identity (nreverse ret) "\n\n")))

(defun epoxide-iptables-stop ()
  "Dummy function."
  nil)

(provide 'iptables)

;;; Iptables.el ends here
