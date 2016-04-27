;;; Command.el --- EPOXIDE Command node definition file

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

;; This node is able to wrap any shell command and call it as an Emacs
;; subprocess.

;; The optional input serves as an enabling input, when it is not
;; connected, the node runs the specified command only once.  When it
;; is connected to an output of another node, the command is evoked
;; when the input changes (unless the previous call has not finished
;; yet).

;; The first configuration argument specifies the host where the
;; command should be run at.  The second defines the command to be
;; evoked and all subsequent configuration arguments are passed to the
;; command as arguments.

;; The only output of the node displays the results returned by the
;; command.

;; When `epoxide-command-verbose' customization variable (in group
;; `epoxide-command') is non-nil the default process sentinel gets
;; associated with the process evoking the specified command thus
;; Emacs adds a message to the result of the command execution stating
;; the the process has finished running.  This message is removed by
;; assigning a dummy sentinel (this is the default setting) to the
;; Emacs subprocess.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-command-run-once)
  (defvar epoxide-process))

(defgroup epoxide-command nil
  "Epoxide Command node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-command-verbose nil
  "When non-nil the default sentinel is used for the process."
  :type 'boolean
  :group 'epoxide-command)

(defun epoxide-command-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "enable signal"))))

(defun epoxide-command-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host where query is executed"))
    ((doc-string . "name of command to run"))
    ((doc-string . "1st argument of command"))
    ((doc-string . "2nd argument of command"))
    ((doc-string . "..."))))

(defun epoxide-command-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "result"))))

(defun epoxide-command-init ()
  "Initialize node variables."
  (set (make-local-variable 'epoxide-process) nil)
  (set (make-local-variable 'epoxide-command-run-once) t))

(defun epoxide-command-exec ()
  "Run a the given command as an Emacs subprocess."
  (unless (and epoxide-process
	       (process-live-p epoxide-process))
    (let* ((enable-input (car epoxide-node-inputs))
	   (enable (epoxide-node-enable-input-active
		    (epoxide-node-read-inputs) 0))
	   (config (epoxide-node-get-config))
	   (host (pop config))
	   (command (car config))
	   (output (nth 0 epoxide-node-outputs)))
      (if (null command)
	  (epoxide-write-node-output "No command was specified\n" output)
	(when (or (and enable-input enable)
		  (and (null enable-input) epoxide-command-run-once))
	  (if (setq-local
	       epoxide-process
	       (apply 'epoxide-start-process host
		      (append `(,output ,output) config)))
	      (unless epoxide-command-verbose
		(set-process-sentinel epoxide-process
				      #'epoxide-command--sentinel))
	    (epoxide-write-node-output
	     (concat "Failed to start command: " command "\n") output))
	  (setq-local epoxide-command-run-once nil))))))

(defun epoxide-command--sentinel (&rest ignore)
  ;; checkdoc-params: (ignore)
  "Dummy function.
Used only for hiding the default sentinel's messages."
  nil)

(defun epoxide-command-stop ()
  "Stop the assigned process."
  (when (boundp 'epoxide-process)
    (epoxide-stop-process epoxide-process)))

(provide 'command)

;;; Command.el ends here
