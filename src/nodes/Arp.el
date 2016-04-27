;;; Arp.el --- EPOXIDE ARP node definition file

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

;; The node is able to query the ARP cache by evoking the `arp'
;; command.  It has one input that serves as a clock input, each time
;; it changes a query is performed.  The node's first configuration
;; argument specifies the host where the query should be run at and
;; the second one specifies those interfaces that are to be kept
;; (while all the others are discarded) when relaying data to the
;; node's only output.  The list of these interfaces should appear as
;; a semicolon (;) separated list.  When the second argument is not
;; present or set to nil, data is displayed exactly the same as
;; returned by the `arp' command. All subsequent configuration
;; arguments are passed to the `arp' command as command line
;; arguments.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-outputs))

(defun epoxide-arp-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-arp-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host"))
    ((doc-string . "optional interface filter"))))

(defun epoxide-arp-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "ARP cache"))))

(defun epoxide-arp-init ()
  "Dummy function."
  nil)

(defun epoxide-arp-exec ()
  "Use arp command to query ARP cache."
  (let* ((config (copy-sequence (epoxide-node-get-config)))
	 (host (pop config))
	 (filter-criteria (pop config))
	 (filter-criteria (when filter-criteria
			    (split-string filter-criteria ";")))
	 (command (mapconcat 'identity (cons "arp" config) " "))
	 tmp ret)
    (when (epoxide-node-enable-input-active
	   (epoxide-node-read-inputs) 0)
      (setq tmp (epoxide-shell-command-to-string host command))
      (if (null filter-criteria)
	  (setq ret tmp)
	(dolist (l (split-string tmp "\n"))
	  (when (delq nil (mapcar (lambda (x)
				    (string-match x l)) filter-criteria))
	    (push (concat l "\n") ret)))
	(setq ret (mapconcat 'identity (nreverse ret) "")))
      (epoxide-write-node-output ret (car epoxide-node-outputs)))))

(defun epoxide-arp-stop ()
  "Dummy function."
  nil)

(provide 'arp)

;;; Arp.el ends here
