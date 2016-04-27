;;; Ping.el --- EPOXIDE Ping node definition file

;; Copyright (C) 2015      Felicián Németh
;; Copyright (C) 2015-2016 István Pelle
;; Copyright (C) 2015      Tamás Lévai

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

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs))

(defvar epoxide-process "Handle to ping backgroud process.")

(defun epoxide-ping-input-info ()
  "Provide documentation, value tips and validation for input fields."
  nil)

(defun epoxide-ping-config-info ()
  "Provide documentation, value tips and validation for config fields."
  `(((doc-string . ,(concat "Source address\nEither IP address/hostname "
                            "or a tramp directory like '/ssh:remote.host:/' "
                            "or '/mininet:h1:/' see "
                            "(info \"(Tramp)Remote processes\"). "
                            "Empty means localhost."))
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-or-name-p))
    ((doc-string . "target IP address")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-or-name-p))
    ((doc-string . "ping count")
     (value-validator . epoxide-positive-number-p))
    ((doc-string . "extra cmd args"))))

(defun epoxide-ping-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "ping results"))))

(defun epoxide-ping-init ()
  "Init connection buffer."
  (set (make-local-variable 'epoxide-process) nil))

(defun epoxide-ping-start ()
  "Start the ping subprocess."
  (let* ((source (nth 0 epoxide-node-config-list))
         (target (nth 1 epoxide-node-config-list))
         (count (nth 2 epoxide-node-config-list))
         (extra-args (cdddr epoxide-node-config-list))
         (output-buffer (nth 0 epoxide-node-outputs))
         (command "ping")
         (args `(,output-buffer ,output-buffer ,command ,target)))
    (when count
      (setq args (append args `("-c" ,count))))
    (setq args (append args extra-args))
    (setq epoxide-process (apply 'epoxide-start-process source args))))

(defun epoxide-ping-exec ()
  "Wrapper for external `ping' command."
  (if (and epoxide-process
           (process-live-p epoxide-process))
      ;; The framework called epoxide-ping-exec, because there's a
      ;; change in one of its input buffers, or becuase of something
      ;; else, but the ping process is still running, so don't restart
      ;; the process.
      nil
    (epoxide-ping-start)))

(defun epoxide-ping-stop ()
  "Kill connection buffer."
  (when (boundp 'epoxide-process)
    (epoxide-stop-process epoxide-process)))

(defun epoxide-ping-min-time (&rest operands)
  "Return from OPERANDS the one having the lowest RTT."
  (when operands
    (epoxide-ping-compare-time '< 1.0e+INF operands)))

(defun epoxide-ping-max-time (&rest operands)
  "Return from OPERANDS the one having the highest RTT."
  (when operands
    (epoxide-ping-compare-time '> -1.0 operands)))

(defun epoxide-ping-compare-time (operator init operands)
  "Compare ping RTT times.

Using OPERATOR and INIT, iterate trough OPERANDS and determine
which to choose.  E.g. when OPERATOR is '<' and INIT is infinity,
the ping message having the lowest RTT time is returned."
  ;; FIXME: case of operands having multiple lines is not dealt with.
  (let (ret current)
    (dolist (o operands)
      (when (and o
		 (setq current (string-match "time=[0-9]+\\(\\.[0-9]+\\)*" o))
		 (> current 0))
	(setq current
	      (string-to-number (car (split-string (substring o (+ 5 current))
						   " " t "[ \f\t\n\r\v]+"))))
	(when (apply operator (list current init))
	  (setq init current)
	  (setq ret o))))
    ret))

(provide 'ping)
;;; Ping.el ends here
