;;; Traceroute.el --- EPOXIDE Traceroute node definition file

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

;; This node is a wrapper for the `traceroute' process.  It has one
;; optional input, an indefinite number of configuration arguments,
;; one mandatory and one optional output.

;; The optional input serves as an enabling input, when it is not
;; connected the node runs the `traceroute' process only once.  When it
;; is connected to an output if receives an enabling signal it runs
;; the `traceroute' process once and waits for another enabling signal
;; to run it again.  Enabling signals received during the run of the
;; process are ignored.

;; The first configuration argument specifies the host where the
;; `traceroute' process should be run at.  The second defines the
;; target IP address or name for the `traceroute'.  All subsequent
;; arguments are passed to the `traceroute' process as extra
;; arguments.

;; The first output of the node is always present, it relays the
;; information received from the `traceroute' process.  The second
;; output is only present when it is connected to an input.  It
;; displays information about the evaluation of the current iteration
;; of the `traceroute' process.  The output is updated only when the
;; `traceroute' process is finished.  It can display two types of
;; messages:

;; * Success: when the process finishes without error.  When information
;; about the last hop is available that is displayed also.

;; * Failure: when the `traceroute' runs into and error or it reveals a
;; routing failure.  When information about the last hop is available
;; that is displayed also.  When a routing failure has occurred
;; (e.g. host/network/protocol unreachable) the resolved failure
;; message is also displayed for the last hop.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs)
  (defvar epoxide-traceroute-run-once)
  (defvar epoxide-traceroute-marker))

(defvar epoxide-process "Handle to traceroute backgroud process.")

(defvar epoxide-traceroute-result nil
  "Collect partial results of a trace.")

(defun epoxide-traceroute-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "optional clock"))))

(defun epoxide-traceroute-config-info ()
  "Provide documentation, value tips and validation for config fields."
  `(((doc-string . ,(concat "host\nEither the source IP address/hostname "
                            "or a tramp directory like '/ssh:remote.host:/' "
                            "or '/mininet:h1:/' see "
                            "(info \"(Tramp)Remote processes\"). "
                            "Empty means localhost."))
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-or-name-p))
    ((doc-string . "target IP address")
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-or-name-p))
    ((doc-string . "extra cmd args"))))

(defun epoxide-traceroute-output-info ()
  "Provide documentation, value tips and validation for output fields."
  `(((doc-string . ,(concat "traceroute output\nSame as the string returned "
			    "by the tracerotue process.")))
    ((doc-string . ,(concat "traceroute results\nEither `success' or `failure'"
			    " depending on the process's output with option "
			    "details."))
     (auto-create . nil))))

(defun epoxide-traceroute-init ()
  "Init connection buffer."
  (set (make-local-variable 'epoxide-process) nil)
  (set (make-local-variable 'epoxide-traceroute-run-once) t)
  (when (cadr epoxide-node-outputs)
    (with-current-buffer (car epoxide-node-outputs)
      ;; Create a marker that registers the position until which the
      ;; process's output has been analyzed.
      (set (make-local-variable 'epoxide-traceroute-marker) 1))))

(defun epoxide-traceroute-exec ()
  "Wrapper for external `traceroute' command."
  (unless (and epoxide-process
	       (process-live-p epoxide-process))
    (let* ((enable-input (car epoxide-node-inputs))
	   (enable (epoxide-node-enable-input-active
		    (epoxide-node-read-inputs) 0))
	   (config (epoxide-node-get-config))
	   (source (pop config))
	   (target (pop config))
	   (output-buffer (nth 0 epoxide-node-outputs))
	   (command "traceroute")
	   (args `(,output-buffer ,output-buffer ,command ,target))
	   (args (append args config)))
      (when (or (and enable-input enable)
		(and (null enable-input) epoxide-traceroute-run-once))
	(if (setq-local epoxide-process
			(apply 'epoxide-start-process source args))
	    (set-process-sentinel epoxide-process
				  #'epoxide-traceroute--analyze)
	  (epoxide-write-node-output
	   "Failed to start `traceroute' process.\n" output-buffer))
	(setq-local epoxide-traceroute-run-once nil)))))

;; TODO: decide how to interpret a traceroute result with only
;; `*'s. Currently it is considered as success.
(defun epoxide-traceroute--analyze (process &rest args)
  "Analyze and evaluate the output of the traceroute process.

The function is a process sentinel.  PROCESS is the process for
which the event occurred.  The first item in ARGS is a string
describing the type of the event."
  (let* ((buf (when (processp process)
		(process-buffer process)))
	 (output-buffer (when buf
			  (with-current-buffer buf
			    (with-current-buffer epoxide-src-node
			      (cadr epoxide-node-outputs)))))
	 (status (car args)))
    (when (and buf status output-buffer
	       (equal status "finished\n"))
      (with-current-buffer buf
	(let* ((max (point-max))
	       (data (buffer-substring-no-properties
		      epoxide-traceroute-marker max))
	       (failure (propertize "Failure" 'face 'isearch-fail))
	       output)
	  ;; Perform analysis when the process has finished running.
	  (when (> max epoxide-traceroute-marker)
	    (if (or (string-match "Cannot handle" data)
		    (string-match "exited abnormally" data))
		(setq output failure)
	      (setq output (propertize "Success" 'face 'success))
	      ;; Remove n from the end.
	      (setq data (split-string data "\n"))
	      ;; Process traceroute hops starting with the last one.
	      (dolist (d (reverse (cdr data)))
		(let ((data (cddr (split-string (epoxide-chomp d) " ")))
		      (out "")
		      failed)
		  (dotimes (i (length data))
		    (let ((cur (nth i data)))
		      ;; Search the hops until first (in the reverse
		      ;; order) one is found that has information.
		      (unless (equal cur "*")
			;; Collect router name.
			(setq out (concat ": " out cur))
			(incf i)
			(when (nth i data)
			  ;; Collect router address.
			  (setq out (concat out " " (nth i data)))
			  ;; Collect failure data.
			  (let* ((f (mapconcat 'identity (nthcdr i data) " "))
				 (p (string-match "!" f)))
			    (when p
			      (setq output failure)
			      (setq failed (substring f p (+ p 2))))))
			(return))))
		  (when (> (length out) 0)
		    (setq output (concat output out))
		    ;; Resolve failure message.
		    (setq
		     output
		     (concat output
			     (pcase failed
			       ("!H"
				" host unreachable")
			       ("!N"
				" network unreachable")
			       ("!P"
				" protcol unreachable")
			       ("!S"
				" source route failed")
			       ("!F"
				" fregmentaion needed")
			       ("!X"
				" communication administratively prohibited")
			       (otherwise
				""))))
		    (return))))
	      (setq-local epoxide-traceroute-marker max)))
	  (when output
	    (epoxide-write-node-output (concat output "\n") output-buffer)))))))

(defun epoxide-traceroute-check (data)
  "Check traceroute results.

DATA is the current output from a Traceroute node.  Return nil if
the traceroute process is finished and it had not found a route
to the target.  Otherwise return t."
  ;; FIXME: making `epoxide-traceroute-result' buffer local here means that a
  ;; decision node can take only one traceroute input at a time.
  ;; FIXME: partial results arriving from a traceroute node are forwarded to the
  ;; decision node's positive output. A true decision is only made when
  ;; the traceroute node's process either finishes or exits. This might not
  ;; be the desired outcome: can be solved by modifying the decision process.
  (unless epoxide-traceroute-result
    (set (make-local-variable 'epoxide-traceroute-result) ""))
  (setq-local epoxide-traceroute-result
	      (concat epoxide-traceroute-result data))
  (if (string-match "Process \\*.+\\* exited\\|finished"
		    epoxide-traceroute-result)
      (if (string-match "Cannot handle" epoxide-traceroute-result)
	  nil
	t)
    t))

(defun epoxide-traceroute-stop ()
  "Kill connection buffer."
  (when (boundp 'epoxide-process)
    (epoxide-stop-process epoxide-process)))

(provide 'traceroute)
;;; Traceroute.el ends here
