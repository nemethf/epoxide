;;; Ifconfig.el --- EPOXIDE Ifconfig node definition file

;; Copyright (C) 2015-2016 István Pelle

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

;; Node wraps the ifconfig shell command.  It has an enable input: when
;; there is any data on it, a new ifconfig call is executed.  The node
;; has indefinit number of configuration arguments.  The first
;; specifies the host where the call should be executed, when the
;; second is non-nil the results are checked and the rest of the
;; arguments denote the interfaces that should be left out of the
;; check.  The node has two outputs: the first holds the results of the
;; ifconfig call while the second displays those interfaces that do
;; not satisfy the check conditions.
;; When checking the interfaces IP addresses and packet counts are
;; checked.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs))

(defun epoxide-ifconfig-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "enable signal"))))

(defun epoxide-ifconfig-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host"))
    ((doc-string . "check"))
    ((doc-string . "interfaces to exclude"))))

(defun epoxide-ifconfig-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "result"))
    ((doc-string . "negative output"))))

(defun epoxide-ifconfig-init ()
  "Dummy function."
  nil)

(defun epoxide-ifconfig-exec ()
  "Run an ifconfig call."
  (let ((host (nth 0 epoxide-node-config-list))
	(check (nth 1 epoxide-node-config-list))
	(excludes (cddr epoxide-node-config-list))
	(output (nth 0 epoxide-node-outputs))
	(negative-output (nth 1 epoxide-node-outputs))
	result)
    (when (epoxide-node-get-inputs-as-string (epoxide-node-read-inputs))
      (setq result (epoxide-shell-command-to-string host "ifconfig"))
      (when (equal check nil)
	(setq check nil))
      (when check
      	(let ((check-result
	       (concat
		(epoxide-ifconfig-check-interfaces result excludes)
		"\n\n")))
      	  (if (> (length check-result) 0)
      	      (setq result (concat check-result "\n\n"))
      	    (epoxide-write-node-output result negative-output))
      	  (setq result check-result)))
      (epoxide-write-node-output result output))))

(defun epoxide-ifconfig-check-interfaces (text &rest interfaces-to-exclude)
  "Check interfaces whether they have a valid configuration.

Go through TEXT that describes each interfaces excluding
INTERFACES-TO-EXCLUDE and check each of them."
  (when (listp (nth 0 interfaces-to-exclude))
    (setq interfaces-to-exclude (nth 0 interfaces-to-exclude)))
  (let ((interfaces (split-string text "\n\n"))
	ret)
    (dolist (i interfaces)
      (unless (member (car (split-string i " ")) interfaces-to-exclude)
	(when (epoxide-ifconfig--check-interface i)
	  (setq ret (cons i ret)))))
    (mapconcat 'identity (nreverse (delq nil ret)) "\n\n")))

(defun epoxide-ifconfig--check-interface (interface)
  "Check a single interface configuration contained in INTERFACE."
  (when (and (epoxide-ifconfig--check-ip-address interface)
	     (epoxide-ifconfig--check-rx interface)
	     (epoxide-ifconfig--check-rx interface))
    t))

(defun epoxide-ifconfig--check-ip-address (interface)
  "Check whether INTERFACE has an IP address (either IPv4 or IPv6)."
  (when (or (string-match "inet addr:" interface)
	    (string-match "inet6 addr:" interface))
    t))

(defun epoxide-ifconfig--check-packets (interface type)
  "Check whether INTERFACE successfully relayed RX or TX traffic.
TYPE specifies which direction should be checked.  It has to be
either rx or tx.  Return t when the number of packets is greater
than the number of errors, dropped or overruns."
  ;; Jump to the specific line of the ifconfig output.
  (let* ((exp (concat type " packets:"))
	 (x-beg (string-match exp interface))
  	 (x (if x-beg
		(car (split-string (substring interface
					      (+ (length exp) x-beg)) "\n"))
	      nil))
  	 (parameters '("errors:" "dropped:" "overruns:"))
  	 (ret t)
  	 packets failures)
    (when x
      (setq packets (car (split-string x " ")))
      (setq x (substring x (1+ (length packets))))
      (setq packets (string-to-number packets))
      ;; Walk through the read line and collect error, drop and
      ;; overrun occurrences by constantily shortening the read string.
      (dolist (p parameters)
      	(let ((failure (cadr (split-string x p))))
      	  (unless (equal failure x)
	    ;; Collect the occurrence of failure p.
      	    (setq failure (car (split-string failure " ")))
	    ;; Remove the currently processed failure counter from the
	    ;; read line.
	    (setq x (substring x (length failure)))
      	    (when (< 0 (length failure))
	      ;; Keep failure counter when it has data.
      	      (setq failures (cons failure failures))))))
      (dolist (f failures)
    	(when (<= packets (string-to-number f))
	  ;; Return nil when one of the counters signals that all
	  ;; packets had some problems.
    	  (setq ret nil))))
    ret))

(defun epoxide-ifconfig--check-rx (interface)
  "Check RX traffic of INTERFACE."
  (epoxide-ifconfig--check-packets interface "rx"))

(defun epoxide-ifconfig--check-tx (interface)
  "Check TX traffic of INTERFACE."
  (epoxide-ifconfig--check-packets interface "tx"))

(defun epoxide-ifconfig-stop ()
  "Dummy function."
  nil)

(provide 'ifconfig)

;;; Ifconfig.el ends here
