;;; tramp-mininet.el --- Mininet method for tramp

;; Copyright (C) 2015 Felicán Németh

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
;;
;; It adds a tramp connection method for accessing mininet hosts.  It
;; assumes standard mininet VM installation.  Customize
;; `tramp-mininet-m-command' in case of non-standard installation.
;;
;; Usually, mininet doesn't virtualize the filesystem, so a tramp
;; method doesn't make too much sense in itself, however it makes
;; possible to run a command in the networking namespace of a host.
;; See Ping.el for an example.

;;; Code:

(require 'tramp)

(defcustom tramp-mininet-m-command
  "/home/mininet/mininet/util/m"
  "Command name for the mininet's 'm' executable."
  :type 'file
  :group 'tramp
  :group 'epoxide)

(defun tramp-mininet-list-hosts (filename)
  "Return a list of (nil host) tuples allowed to access.
FILENAME is ignored."
  (let* ((command (concat "ps ax"
                          "|grep \"mininet:[a-z0-9]*$\""
                          "|grep bash"
                          "|cut -f3 -d\":\""))
         (hosts (shell-command-to-string command))
         (hosts (split-string hosts "\n" t)))
    (mapcar (lambda (host) `(nil ,host)) hosts)))

;;;###autoload
(defun tramp-mininet-setup ()
  "Configure the `mininet' method."
  (let ((m-cmd (executable-find tramp-mininet-m-command)))
    (unless m-cmd
      (warn "Can't find 'm'; Customize `tramp-mininet-m-command'"))
    (add-to-list
     'tramp-methods
     `("mininet"
       (tramp-login-program        ,m-cmd)
       (tramp-login-args           (("%h") ("bash")))
       (tramp-remote-shell         "/bin/sh")
       (tramp-remote-shell-args    ("-c"))
       (tramp-connection-timeout   10)))
    (tramp-set-completion-function "mininet"
                                   '((tramp-mininet-list-hosts "")))))

;;;###autoload
(eval-after-load 'tramp '(tramp-mininet-setup))

(provide 'tramp-mininet)

;;; tramp-mininet.el ends here
