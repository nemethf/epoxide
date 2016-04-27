;;; Route.el --- EPOXIDE Route node definition file

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

;; This node is a wrapper for the `route' shell command.

;; It has one input that serves as an enabling input.  The first of its
;; configuration arguments is the host where the shell command should
;; be run.  All other arguments are optional.  The second argument is a
;; (`;' separated) list of IP addresses.  When they are present only
;; those routes are listed that cover these IP addresses (in this case
;; the `route' shell command is always called with the `-n'
;; option). All further configuration arguments are considered as
;; extra arguments that are passed to the `route' command when calling
;; it.  The node has one output that lists the appropriate routes.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-outputs))

(defun epoxide-route-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-route-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host"))
    ((doc-string . "IP address filter (`;' separated)"))
    ((doc-string . "extra arguments..."))))

(defun epoxide-route-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "routes"))))

(defun epoxide-route-init ()
  "Dummy function."
  nil)

(defun epoxide-route-exec ()
  "Call`route'.

When IP address filter is present, remove those routes that are
not related to the listed IP addresses."
  (let* ((f (lambda (host command extra-args)
	      (epoxide-shell-command-to-string
	       host (mapconcat 'identity (cons command extra-args) " "))))
	 (config (copy-sequence (epoxide-node-get-config)))
	 (host (pop config))
	 (ip-addresses (pop config))
	 (ip-addresses (when ip-addresses
			 (split-string ip-addresses ";")))
	 (extra-arguments config)
	 tmp ret)
    (when (epoxide-node-enable-input-active
	   (epoxide-node-read-inputs) 0)
      (if (null ip-addresses)
	  (setq ret (funcall f host "route" extra-arguments))
	(setq tmp (funcall f host "route -n" extra-arguments))
	(let* ((f-1 (lambda (item list)
		      (member item (mapcar 'downcase list))))
	       (f-2 (lambda (data)
		      (or (funcall f-1 "genmask" data)
			  (funcall f-1 "netmask" data))))
	       mask)
	  (dolist (l (split-string tmp "\n"))
	    (let* ((data (split-string l " " t " "))
		   (m (funcall f-2 data))
		   (m (when m
			(- (length data) (length m))))
		   (ip (car data)))
	      (when m
		(setq mask m))
	      (when (and mask ip
			 (epoxide-ipv4-address-p ip))
		(dolist (i ip-addresses)
		  (when (epoxide-ip-address-in-network i ip (nth mask data))
		    (push l ret)
		    (return)))))))
	(setq ret (concat (mapconcat 'identity (nreverse ret) "\n") "\n")))
      (epoxide-write-node-output ret (car epoxide-node-outputs)))))

(defun epoxide-route-stop ()
  "Dummy function."
  nil)

(provide 'route)

;;; Route.el ends here
