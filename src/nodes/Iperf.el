;;; Iperf.el --- EPOXIDE Iperf node definition file

;; Copyright (C) 2015      Tamás Lévai, Felicián Németh, István Pelle

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
  (defvar epoxide-node-outputs)
  (defvar epoxide-node-class)
  (defvar epoxide-node-name)
  (defvar epoxide-tsg-node-list))

(defvar epoxide-process-server "Handle to iperf server connection." nil)
(defvar epoxide-process-client "Handle to iperf client connection." nil)

(defun epoxide-iperf-input-info ()
  "Provide documentation, value tips and validation for input fields."
  nil)

(defun epoxide-iperf-config-info ()
  "Provide documentation, value tips and validation for config fields."
  (let ((tramp-addr 
         (concat "Either IP address/hostname or a tramp directory like "
                 "'/ssh:remote.host:/' or '/mininet:h1:/' see (info "
                 "\"(Tramp)Remote processes\").  Empty means localhost.")))
    `(((doc-string . ,(concat "Server address\n" tramp-addr))
       (value-helper . epoxide-list-ip-addresses)
       (value-validator . epoxide-ip-address-or-name-p))
      ((doc-string . ,(concat "Client address\n" tramp-addr))
       (value-helper . epoxide-list-ip-addresses)
       (value-validator . epoxide-ip-address-or-name-p))
      ((doc-string . "extra server cmd args"))
      ((doc-string . "extra client cmd args")))))

(defun epoxide-iperf-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "iperf server output"))
    ((doc-string . "iperf client output"))))

(defun epoxide-iperf--guess-testbed-addr (addr)
  "Guess the testbed address of host given by its management address ADDR."
  ;; For IPv4 only.
  (let* ((m-addr (if (tramp-tramp-file-p addr)
                     (with-parsed-tramp-file-name addr nil
                       host)
                   addr))
         (cmd "getent hosts \"%s\"|head -n1|cut -f1 -d\" \"")
         (cmd (format cmd (shell-quote-argument m-addr)))
         (tramp-addr (if (tramp-tramp-file-p addr)
                         addr
                       (format "/ssh:%s:/" addr)))
         (cmd-ip "ifconfig|grep -oP 'inet.addr:\\K[0-9.]*'")
         local-addrs)
    (unless (epoxide-ipv4-address-p m-addr)
      (setq m-addr (epoxide-chomp (shell-command-to-string cmd))))
    (setq local-addrs (split-string
                       (epoxide-shell-command-to-string tramp-addr cmd-ip)))
    (dolist (non-testbed-addr (list "127.0.0.1" m-addr))
      (setq local-addrs (delete non-testbed-addr local-addrs)))
    (if (< 0 (length local-addrs))
        ;; Possibly multiple candidates, return the first one.
        (car local-addrs)
      ;; No addresses besides the management addr, return that one.
      m-addr)))

(defun epoxide-iperf-init ()
  "Init Iperf output buffers and connection buffers."
  (set (make-local-variable 'epoxide-process-server) nil)
  (set (make-local-variable 'epoxide-process-client) nil))

(defun epoxide-iperf-server-start ()
  "Start 'iperf' server subprocesses."
  (let* ((source (nth 0 epoxide-node-config-list))
         (extra-args (cadddr epoxide-node-config-list))
         ;; FIXME: this won't work when there's a space in an argument
         (extra-args (and extra-args
                          (split-string extra-args " ")))
         (output-buffer (nth 0 epoxide-node-outputs))
         (command "iperf")
         (args `(,output-buffer ,output-buffer ,command "-s")))
    (setq args (append args extra-args))
    (setq epoxide-process-server
          (apply 'epoxide-start-process source args))))

(defun epoxide-iperf-client-start ()
  "Start 'iperf' client subprocesses."
  (let* ((source (nth 1 epoxide-node-config-list))
         (target-mngt (nth 0 epoxide-node-config-list))
         (target-testbed (epoxide-iperf--guess-testbed-addr target-mngt))
         (extra-args (cadddr epoxide-node-config-list))
         ;; FIXME: this won't work when there's a space in an argument
         (extra-args (and extra-args
                          (split-string extra-args " ")))
         (output-buffer (nth 1 epoxide-node-outputs))
         (command "iperf")
         (args `(,output-buffer ,output-buffer ,command "-c" ,target-testbed))
	 pos)
    (setq args (append args extra-args))
    (erase-buffer)
    (epoxide-insert-node-basics)
    (insert (propertize (concat "\n\nSTARTING IPERF FOR: " source
			       " -> " target-mngt " (" target-testbed ")")
			'face 'font-lock-doc-face))
    (setq pos (line-beginning-position))
    (insert "\n\n")
    (epoxide-insert-key-bindings "epoxide-mode-map")
    (goto-char pos)
    (setq epoxide-process-client
          (apply 'epoxide-start-process source args))))

(defsubst epoxide-iperf--process-live-p (process)
  "Returns non-nil if PROCESS is non-nil and alive."
  (and process
       (process-live-p process)))

(defun epoxide-iperf-exec ()
  "Wrapper for external `iperf' command."
  (unless (epoxide-iperf--process-live-p epoxide-process-server)
    (epoxide-iperf-server-start)
    (sleep-for 1))
  (unless (epoxide-iperf--process-live-p epoxide-process-client)
    (epoxide-iperf-client-start)))

(defun epoxide-iperf-stop ()
  "Kill buffers used for remote connection."
  (epoxide-stop-process epoxide-process-server)
  (epoxide-stop-process epoxide-process-client)
  ;; Clear output buffers.  Why?
  ;(dolist (output epoxide-node-outputs)
  ;  (with-current-buffer output
  ;    (erase-buffer)))
  )


(provide 'iperf)
;;; Iperf.el ends here
