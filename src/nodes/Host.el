;;; Host.el --- EPOXIDE Host node definition file

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

;; A node to perform DNS lookup using the calling the 'host' command.
;; The node takes one input that is used as an enable signal.  When
;; new data is present on the input a new DNS lookup is executed.  The
;; node has two configuration parameters.  The first is the host where
;; the query should be executed, the second the name or IP address
;; that should be looked up.  The node has one output that conveys the
;; result of the query.

;; When `epoxide-host-run-as-process' is non-nil (default setting), in
;; case when the query takes a long time to complete it will not block
;; other actions performed in Emacs. Otherwise the node calls the
;; 'host' command as a (possibly blocking) shell call.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-process-buffer-name)
  (defvar epoxide-host-output-buffer)
  (defvar epoxide-process))

(defgroup epoxide-host nil
  "Epoxide Host node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-host-run-as-process t
  "When t run the query as a process, otherwise as a shell call.
In the former case when the query takes a long time to complete
it will not block other actions performed in Emacs."
  :type 'boolean
  :group 'epoxide-host)

(defun epoxide-host-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "enable signal"))))

(defun epoxide-host-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host where query is executed"))
    ((doc-string . "target that should be looked up"))
    ((doc-string . "extra arguments..."))))

(defun epoxide-host-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "result"))))

(defun epoxide-host-init ()
  "Initialize node variables."
  (set (make-local-variable 'epoxide-process) nil)
  (when epoxide-host-run-as-process
    (set (make-local-variable 'epoxide-process-buffer-name)
	 (concat
	  (substring (buffer-name) 0 (1- (length (buffer-name))))
	  "-process*"))
    (let ((output (nth 0 epoxide-node-outputs)))
      (with-current-buffer (get-buffer-create epoxide-process-buffer-name)
	(set (make-local-variable 'epoxide-host-output-buffer) output)))))

(defun epoxide-host-exec ()
  "Run a DNS lookup using the 'host' shell-call."
  (let* ((config (epoxide-node-get-config))
	 (host (pop config))
	 (name (pop config))
	 (output (nth 0 epoxide-node-outputs)))
    (when (epoxide-node-get-inputs-as-string (epoxide-node-read-inputs))
      (if epoxide-host-run-as-process
	  (unless (and epoxide-process
		       (process-live-p epoxide-process))
	    (when (setq-local
		   epoxide-process
		   (apply 'epoxide-start-process host
			  (append `(,epoxide-process-buffer-name
				    ,epoxide-process-buffer-name
				    "host" ,name)
				  config)))
	      (set-process-sentinel epoxide-process
				    #'epoxide-host-sentinel)))
	(epoxide-write-node-output
	 (epoxide-shell-command-to-string
	  host (mapconcat 'identity (cons "host" (cons name config)) " "))
	 output)))))

(defun epoxide-host-sentinel (process &rest args)
  "When PROCESS has finished copy its results to the node's output.

The entirety of the result of the process is copied to the node's
output in a single move this way connected nodes do not have to
deal with a partial result.

The function is a process sentinel thus PROCESS is the process for
which the event occurred.  The first item in ARGS is a string
describing the type of the event."
  (let ((buf (when (processp process)
	       (process-buffer process)))
	(status (car args)))
    (when (and buf status
	       (equal status "finished\n"))
      (with-current-buffer buf
	(epoxide-write-node-output
	 (buffer-substring-no-properties (point-min) (point-max))
	 epoxide-host-output-buffer)
	(erase-buffer)))))

(defun epoxide-host-check (data)
  "Check results of execution.

DATA is the current output of a Host node.  Return nil if the
output means that the DNS lookup was unsuccessful."
  (if (or (equal (length data) 0)
	  (string-match "not found" data))
      nil
    t))

(defun epoxide-host-stop ()
  "Dummy function."
  (when (boundp 'epoxide-process)
    (epoxide-stop-process epoxide-process))
  (when (boundp 'epoxide-process-buffer-name)
    (epoxide-kill-buffer epoxide-process-buffer-name t)))

(provide 'host)

;;; Host.el ends here
