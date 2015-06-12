;;; Gdb.el --- EPOXIDE GDB node definition file

;; Copyright (C) 2014-2015 István Pelle

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

;; The node is able to attach GDB to a running process.  The node does
;; not have any inputs or outputs and it has three configuration
;; parameters.  #0 is the host where the process to be debugged is
;; running on.  This should be specified by writing '/ssh:<user
;; name>@<IP address>:/'.  #1 is a process name filter: it should
;; specify part of the process' name.  Optional #2 specifies how to
;; start the debugger: when it is present, GDB is started as gud-gdb
;; and this way more then one programs can be debugged otherwise gdb
;; is called and then only one program can be debugged at a time.  In
;; the latter case the gdb-many-window configuration can be used, in
;; the former case only the simple GDB CLI is operational.  When more
;; than process matches the filter string, all of them are listed and
;; the user can choose from these options.  When there is only one
;; possiblity GDB is attached to it without further ado.

;;; Code:

(eval-and-compile
  (require 'epoxide)
  (require 'gud)
  (require 'gdb-mi)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-gdb-buffer)
  (defvar epoxide-node-buffer)
  (defvar epoxide-node-name)
  (defvar gdb-many-windows))

(defgroup epoxide-gdb nil
  "Epoxide GDB defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-gdb-many-windows (when (boundp 'gdb-many-windows)
				      gdb-many-windows)
  "When t, GDB node starts with the many windows configuration.
See `gdb-many-windows' for details."
  :type 'boolean
  :group 'epoxide-gdb)

(defvar epoxide-gdb-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map epoxide-mode-map)
    (define-key map "d" 'epoxide-gdb-switch-to-gud-buffer)
    map)
  "Keymap for GDB nodes.")

(defun epoxide-gdb-input-info ()
  "Provide documentation, value tips and validation for input fields."
  nil)

(defun epoxide-gdb-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host")
     (value-helper . nil)
     (value-validator . nil))
    ((doc-string . "process name")
     (value-helper . nil)
     (value-validator . stringp))
    ((doc-string . "when present, multiple gdb instances can be started")
     (value-helper . nil)
     (value-validator . stringp))))

(defun epoxide-gdb-output-info ()
  "Provide documentation, value tips and validation for output fields."
  nil)

(defun epoxide-gdb-init ()
  "Initialize node and start execution."
  (set (make-local-variable 'epoxide-gdb-buffer) nil)
  (use-local-map epoxide-gdb-map)
  (epoxide-gdb-exec))

(defun epoxide-gdb-exec ()
  "Query host for processes with the specified name and list those.
If one is selected attach GDB."
  (let* ((host (nth 0 epoxide-node-config-list))
	 (process-name (nth 1 epoxide-node-config-list))
	 (gdb-type (nth 2 epoxide-node-config-list))
	 (query (concat "ps -axo pid,command --sort user | grep " process-name))
	 ;; For filtering out process doing coloring.
	 (color (concat "grep --color=auto " process-name))
	 ;; For filtering out grep.
	 (grep (concat "grep " process-name))
	 (enable (when (and host process-name (null epoxide-gdb-buffer)) t))
	 processes tmp-processes)
    (when enable
      ;; Query processes that has `process-name' as substring.
      (setq processes (split-string
		       (epoxide-shell-command-to-string
			host query)
			"\n" t))
      ;; Create alist from PID and command parts of the results.
      (setq processes
	    (nreverse
	     (dolist (p processes tmp-processes)
	       (setq p (epoxide-chomp p))
	       (let* ((space-pos (string-match " " p))
		      pid name)
		 (when space-pos
		   (setq pid (epoxide-chomp (substring p 0 space-pos)))
		   (setq name (epoxide-chomp (substring p space-pos)))
		   ;; Filter out lines that do not describe valid processes,
		   ;; collect the rest.
		   (when (and (> (string-to-number pid) 0)
			      (not (string-match query name))
			      (not (string-match color name))
			      (not (string-match grep name)))
		     (setq tmp-processes (append (list `((pid . ,pid)
							 (name . ,name)))
						 tmp-processes))))))))
      ;; Attach GDB when possible.
      (cond
       ((null processes)
	(epoxide-gdb--no-such-process process-name))
       ((equal (length processes) 1)
	(epoxide-gdb--attach (nth 0 processes) host gdb-type))
       (t
	(let* ((selection
		(ido-completing-read
		 (concat "Available processes are: ")
		 (mapcar (lambda (p)
			   (cdr (assoc 'name p)))
			 processes)))
	       process)
	  (setq process
		(dolist (p processes)
		  (when (equal (cdr (assoc 'name p)) selection)
		    (return p))))
	  (if (null process)
	      (epoxide-gdb--no-such-process selection)
	    (epoxide-gdb--attach process host gdb-type))))))))

(defun epoxide-gdb--no-such-process (process-name)
  "Give notification when requested PROCESS-NAME does not exists."
  (message "No process having name %s. Cannot attach GDB." process-name))

(defun epoxide-gdb-config-change-handler ()
  "Handle configuration parameter change."
  (if (and (boundp 'epoxide-gdb-buffer)
	   epoxide-gdb-buffer
	   (buffer-live-p epoxide-gdb-buffer))
      ;; When there's already a GDB session assiciated with this node,
      ;; give notification.
      (message (concat "GDB node '%s' already has a GUD buffer assigned: %s. "
		       "Terminate GDB first.")
	       epoxide-node-name (buffer-name epoxide-gdb-buffer) )
    (let ((node-buffer (current-buffer)))
      (epoxide-gdb-init)
      (switch-to-buffer node-buffer))))

(defun epoxide-gdb--attach (process host gdb-type)
  "Attach GDB to PROCESS running on HOST.
GDB-TYPE specifies how GDB should be run.  If it is non-nil GDB will be called
by gud-gdb: more than one GDB instance can be run cuncurrently simultaneously
but many window cnfiguration is not available.  Otherwise GDB is called by gdb
that way many window configuration is accessible but only one debugger session
can run in the same Emacs session."
  (let ((node-buffer (current-buffer))
	(root-buffer epoxide-root-buffer))
    (erase-buffer)
    (epoxide-insert-node-basics)
    (insert "\n\nDEBUGGER STARTED ON: " host "\n"
	    "PROCESS: " (cdr (assoc 'pid process)) ", "
	    (cdr (assoc 'name process)) "\n\n"
	    "Press d to switch to debugger.\n\n")
    (epoxide-insert-key-bindings "epoxide-gdb-map")
    (goto-char (point-min))
    (let ((default-directory (epoxide-get-default-directory host))
	  gdb-buffer)
      (if gdb-type
	  (gud-gdb (concat "gdb --pid=" (cdr (assoc 'pid process))))
	(gdb (concat "gdb -i=mi --pid=" (cdr (assoc 'pid process))))
	(when epoxide-gdb-many-windows
	  (sit-for 1)
	  (gdb-setup-windows)))
      (rename-buffer (concat "*gud pid:" (cdr (assoc 'pid process)) "*"))
      (set (make-local-variable 'epoxide-root-buffer) root-buffer)
      (set (make-local-variable 'epoxide-node-buffer) node-buffer)
      (setq gdb-buffer (current-buffer))
      (local-set-key "\M-g e" 'epoxide-switch-to-root-buffer)
      (local-set-key "\M-g t" 'epoxide-tsg-show)
      (local-set-key "\M-g n" 'epoxide-gdb-switch-to-node-buffer)
      (local-set-key "\M-g v" 'epoxide-view-show)
      (gud-tooltip-mode)
      (message "GDB has been started")
      (with-current-buffer node-buffer
	(setq-local epoxide-gdb-buffer gdb-buffer)))))

(defun epoxide-gdb-switch-to-gud-buffer ()
  "Switch to the GDB buffer belonging to this node when it still exists."
  (interactive)
  (let ((gdb-type (nth 2 epoxide-node-config-list)))
    (if (and epoxide-gdb-buffer
	     (buffer-live-p epoxide-gdb-buffer))
	(progn
	  (switch-to-buffer epoxide-gdb-buffer)
	  (unless gdb-type
	    (when (or epoxide-gdb-many-windows gdb-many-windows)
	      (gdb-setup-windows))))
      (message "Debugger's buffer does not exists anymore"))))

(defun epoxide-gdb-switch-to-node-buffer ()
  "Switch to the node's buffer that this GDB belongs to."
  (interactive)
  (if (not (and epoxide-node-buffer
		(buffer-live-p epoxide-node-buffer)))
      (message "Node buffer does not exists anymore")
    (delete-other-windows)
    (switch-to-buffer epoxide-node-buffer)))

(defun epoxide-gdb-stop ()
  "Dummy function."
  nil)

(provide 'gdb)

;;; gdb.el ends here
