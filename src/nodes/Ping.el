;;; Ping.el --- EPOXIDE Ping node definition file

;; Copyright (C) 2015 István Pelle, Tamás Lévai, Felicián Németh

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

(provide 'ping)
;;; Ping.el ends here
