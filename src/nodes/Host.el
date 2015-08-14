;;; Host.el --- EPOXIDE Host node definition file

;; Copyright (C) 2015      István Pelle

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

;; A node to perform DNS lookup using the 'host' shell-call.  The node
;; takes one input that is used as an enable signal.  When new data is
;; present on the input a new DNS lookup is executed.  The node has two
;; configuration parameters.  The first is the host where the query
;; should be executed, the second the name or IP address that should
;; be looked up.  The node has one output that conveys the result of
;; the query.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-input-marker)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs))

(defun epoxide-host-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "enable signal"))))

(defun epoxide-host-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host where query is executed"))
    ((doc-string . "target that should be looked up"))))

(defun epoxide-host-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "result"))))

(defun epoxide-host-init ()
  "Initialize input markers."
  (set (make-local-variable 'epoxide-input-marker) 1))

(defun epoxide-host-exec ()
  "Run a DNS lookup using the 'host' shell-call."
  (let ((marker epoxide-input-marker)
	(enable-input (nth 0 epoxide-node-inputs))
	(host (nth 0 epoxide-node-config-list))
	(name (nth 1 epoxide-node-config-list))
	(output (nth 0 epoxide-node-outputs))
	enabled)
    (when enable-input
      (with-current-buffer enable-input
	(setq marker (point-max)))
      (when (> marker epoxide-input-marker)
	(setq enabled t))
      (setq-local epoxide-input-marker marker))
    (when enabled
      (epoxide-write-node-output
       (epoxide-shell-command-to-string host (concat "host " name)) output))))

(defun epoxide-host-stop ()
  "Dummy function."
  nil)

(provide 'host)

;;; Host.el ends here
