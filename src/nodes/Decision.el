;;; Decision.el --- EPOXIDE Decision node definition file

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

;; Node provides options for selecting among different inputs.  An
;; unspecified number of inputs can be connected to the node, and
;; after performing evaluation on these inputs a decision is
;; communicated on one of its outputs.  Output #0 serves as a positive
;; output (i.e. there was at least one input that satisfied initial
;; criteria) and output #1 is a negative output that displays a
;; timestamp whenever there were no inputs satisfying initial
;; criteria.  The number of the node configuration arguments depend on
;; the number of inputs:

;; * First argument decides whether decision should be made only when
;; data is available on every inputs.  If some inputs lag behind or
;; need more time to produce data this option can be used to wait for
;; them.  nil value of this argument specifies not to wait, non-nil
;; values make the node wait for every inputs.

;; * Second argument specifies a timeout.  When using it together with
;; a non-nil value of the first argument, a decision is always made at
;; the timeout even if not every input is present.  In case of the
;; first argument having a nil value and a timeout is set, when no
;; input has any data at the timeout, a negative output is given.
;; Giving nil as the timeout argument switches the function off
;; whereas giving it a value greater than 0 turns it on.  The Decision
;; node itself does not have a timer, so in case a timeout is to be
;; used a Clock node should be connected to the Decision node.  It is
;; advised that the Clock node is connected to the last input of the
;; Decision node (this way node configuration arguments should be
;; given corresponding to this input). Connecting more than one Clock
;; nodes to the Decision node results in the Decision node selecting
;; the first Clock node for the timeout calculation.  The second
;; config argument of the decision node should be set on the tick
;; count when we would like to have the timeout to occur (since the
;; Clock node only supplies tick counts). E.g. if we want to have a
;; timeout of 5 seconds, Clock(1) (that is a Clock giving out a tick
;; in every second) can be specified in conjunction with a Decision
;; node having its second argument set to 5 (or a Clock(0.1) and a
;; timeout value of 50 would have almost the same effect).

;; * Third argument is used for selecting the proper input to be
;; dispatched to the output.  The elisp `and' and `or' or any user
;; defined function can also be used here.  E.g. using the `or' macro
;; when there is at least one input satisfying criteria, the one
;; having the lowest index is going to be displayed on the positive
;; output.  In case no input satisfies its corresponding criteria, the
;; Decision node's negative output will be triggered.

;; * Following arguments should be defined on a per input basis.  Let's
;; assume we are dealing with the 1st input:

;; ** Argument 5 defines whether input 1 should be processed
;; line-by-line.  When giving a nil value, everything read in the
;; current iteration is processed in one bulk, whereas giving a
;; non-nil value results in splitting the read data to lines and
;; making an individual decision on each line.

;; ** Argument 6 specifies whether the result displayed on the
;; Decision node's positive output should be the same as on the input
;; (a non-nil value activates this option).  Passing nil to this argument
;; would mean that the result displayed on the positive output should
;; be taken from the output of the input's decision function.  Using
;; this case, processing can be made on the data within the Decision
;; node.

;; ** Argument 7 is the decision function assigned to input 1. A
;; decision function can be any elisp function that takes at least one
;; argument, and its output is either nil or non-nil.  The decision
;; function serves as the way to examine whether input 1 satisfies
;; some criteria or not.

;; ** Argument 8 defines on which argument of the decision function
;; should the data of input 1 be passed.

;; ** Argument 9 defines the number of arguments of the decision
;; function additional to the data of input 1.

;; ** Arguments 10+ should be the additional arguments of the decision
;; function.  When specifying the arguments, they should be listed in
;; the order in which the elisp function requires them but the
;; argument requiring the data from input 1 should be left out.  Here
;; other inputs can be passed to the decision function using the form
;; 'input-x where x denotes the index of the input.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-decision-first-tick)
  (defvar epoxide-decision-last-tick)
  (defvar epoxide-decision-timed-out))

(defgroup epoxide-decision nil
  "Decision node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-decision-verbose nil
  "When t, negative output displays processed characters also."
  :type 'boolean
  :group 'epoxide-decision)

(defun epoxide-decision-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "input to decide on"))
    ((doc-string . "..."))))

(defun epoxide-decision-config-info ()
  "Provide documentation, value tips and validation for config fields."
  (let ((processing (concat "\nnil: bulk processing\n"
			    "non-nil: line-by-line processing"))
	(result (concat "\nnil: result is the same as returned "
			"by the decision function\n"
			"non-nil: result is the same as on the input")))
  `(((doc-string . "wait for all inputs"))
    ((doc-string . "timeout"))
    ((doc-string . "selection function"))
    ((doc-string . ,(concat "process input 1 line-by-line?" processing)))
    ((doc-string . ,(concat "result is the same as on input 1?")))
    ((doc-string . "decision function"))
    ((doc-string . "pass input as nth argument"))
    ((doc-string . "number of extra arguments passed to the decision function"))
    ((doc-string . "extra arguments of the decision function"))
    ((doc-string . ,(concat "process input 2 line-by-line?" processing)))
    ((doc-string . "...")))))

(defun epoxide-decision-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "output when \"true\""))
    ((doc-string . "output when \"false\"\ntimestamp"))
    ((doc-string . "status"))))

(defun epoxide-decision-init ()
  "Initialize input markers."
  (set (make-local-variable 'epoxide-decision-first-tick) 1)
  (set (make-local-variable 'epoxide-decision-last-tick) 1)
  (set (make-local-variable 'epoxide-decision-timed-out) nil))

(defun epoxide-decision-exec ()
  "Execute decision.

Collect inputs and apply decision and selection functions to
them, enforce timeout if given."
  (let* ((config (copy-sequence (epoxide-node-get-config)))
	 (wait-for-all-inputs (epoxide-string-or-nil (pop config)))
	 (timeout (epoxide-decision--number-or-nil (pop config)))
	 (inputs (epoxide-node-get-inputs-as-list
		  (epoxide-node-read-inputs wait-for-all-inputs
					    epoxide-decision-timed-out)))
	 timed-out in)
    (dolist (i inputs)
      (if i
  	  (push i in)
  	(push "" in)))
    (setq in (nreverse in))
    (setq timed-out (epoxide-decision--timed-out-p in timeout))
    (setq-local epoxide-decision-timed-out timed-out)
    (if wait-for-all-inputs
    	(if (epoxide-decision--all-inputs-ready in)
    	    (epoxide-decision--decide config in)
    	  (when timed-out
    	    (epoxide-decision--decide config in)))
      (if (epoxide-decision--no-inputs-ready in)
    	  (when timed-out
    	    (epoxide-decision--write-negative-output 0))
    	(epoxide-decision--decide config in)))))

(defun epoxide-decision--all-inputs-ready (inputs)
  "Return t when every INPUTS of the node have data."
  (when (and (not (member nil inputs))
	     (not (member "" inputs)))
       t))

(defun epoxide-decision--no-inputs-ready (inputs)
  "Return t when no INPUTS of the node have data (excluding the clock input)."
  (let ((clock-input (epoxide-decision--get-clock-input)))
    (when clock-input
      (setq inputs (epoxide-setl inputs clock-input nil)))
    (setq inputs (delete "" inputs))
    (setq inputs (delq nil inputs))
    (when (equal inputs nil)
      t)))

(defun epoxide-decision--decide (config inputs)
  "Decide whether all inputs meet their respective criteria.

CONFIG is is the part of the node config list that is concerned
with the decision process and INPUTS is a list that contains the
read inputs."
  ;; When a decision is made, the timeout register needs to be updated.
  (setq epoxide-decision-first-tick epoxide-decision-last-tick)
  (let* ((selection-function (pop config))
	 (original-inputs (copy-sequence inputs))
	 (longest-input 0)
	 (i 0)
	 data)
    (while config
      ;; Collect inputs and decision parameters into a single data
      ;; structure.  When an input needs line-by-line processing,
      ;; determine the maximum number of lines.
      (let* ((by-line-processing
	      (epoxide-string-or-nil (pop config)))
	     (output-same-as-input
	      (epoxide-string-or-nil (pop config)))
	     (decision-function (pop config))
	     (input-as-nth-argument (string-to-number (pop config)))
	     (argument-count (string-to-number (pop config)))
	     (diff (- (length config) argument-count))
	     (arguments (butlast config diff))
	     (input (pop inputs))
	     (input (if input
	     		input
	     	      ""))
	     (input (if by-line-processing
			(split-string input "\n")
		      `(,input)))
	     (input-length (length input))
	     (d `((output-same-as-input . ,output-same-as-input)
		  (decision-function . ,decision-function)
		  (input-as-nth-argument . ,input-as-nth-argument)
		  (input . ,input)
		  (arguments . ,arguments))))
	(setq data (cons d data))
	(setq config (nthcdr argument-count config))
	(when (> input-length longest-input)
	  (setq longest-input input-length))))
    (setq data (nreverse data))
    ;; Execute decision on the inputs by taking into account
    ;; line-by-line processing and whether the output should be taken
    ;; from the input or from the output of the decision function.
    (while (< i longest-input)
      (let (ins results char-count verbose)
	(dolist (d data)
	  (let* ((output-same-as-input (cdr (assoc 'output-same-as-input d)))
		 (input (nth i (cdr (assoc 'input d))))
		 (function (cdr (assoc 'decision-function d)))
		 (arguments (epoxide-decision--insert-other-input
			     (cdr (assoc 'arguments d))
			     original-inputs i))
		 (n (cdr (assoc 'input-as-nth-argument d)))
		 (args (epoxide-add-to-list n input arguments))
		 (r (condition-case nil
			(epoxide-eval-function function args)
		      (error nil))))
	    (setq ins (cons input ins))
	    (when function
	      (setq results
		    (cons (if r
			      (if output-same-as-input
				  input
				r)
			    nil)
			  results)))))
	(setq verbose (mapconcat 'epoxide-decision--thing-to-string ins ""))
	(setq char-count (length verbose))
	(let* ((results (nreverse results))
	       (end-result (epoxide-eval-function selection-function results))
	       (link-status (epoxide-decision--create-status-results
			     results selection-function end-result)))
	  (if end-result
	      (epoxide-decision--write-positive-output end-result link-status)
	    (when (> char-count 0)
	      (epoxide-decision--write-negative-output
	       char-count link-status (when epoxide-decision-verbose
					verbose))))))
      (incf i))))

(defun epoxide-decision--create-status-results
    (results selection-function end-result)
  "Create a list of decision partial results.

Return a list of lists where the inner lists consist of two
items.  The first descibes the input, the second the result that
the decision function paired with the respective input gave.  The
last item in the list contains the result of the decision node's
selection-function.

RESULTS is a list containing the input-by-input results returned
by the respective decision functions.  SELECTION-FUNCTION is the
string name of the node's selection function.  END-RESULT is the
result of the node's selection function."
  (let ((results (copy-sequence results))
	(end-result (copy-sequence end-result))
	(i 0)
	ret)
    (while (< i (length epoxide-node-inputs))
      (push `(,(concat "input-" (number-to-string i) " ("
		       (nth i epoxide-node-inputs) ")")
	      ,(epoxide-decision--evaluate (nth i results))) ret)
      (incf i))
    (push `(,(concat "selection function (" selection-function ")")
	    ,(epoxide-decision--evaluate end-result)) ret)
    (nreverse ret)))

(defun epoxide-decision--evaluate (arg)
  "Return color coded passed string if ARG is non-nil.
Return failed string otherwise."
  (if arg
      (epoxide-decision--passed)
    (epoxide-decision--failed)))

(defun epoxide-decision--evaluate-2 (arg-1 arg-2)
  "Based on ARG-1 color code ARG-2 ."
  (if arg-1
      (epoxide-decision--propertize-passed arg-2)
    (epoxide-decision--propertize-failed arg-2)))

(defun epoxide-decision--passed ()
  "Return color coded passed string."
  (epoxide-decision--propertize-passed "passed"))

(defun epoxide-decision--failed ()
  "Return color coded failed string."
  (epoxide-decision--propertize-failed "failed"))

(defun epoxide-decision--propertize-passed (string)
  "Color code STRING with pass color."
  (propertize string 'face 'success))

(defun epoxide-decision--propertize-failed (string)
  "Color code STRING with fail color."
  (propertize string 'face 'isearch-fail))

(defun epoxide-decision--write-positive-output (result &optional links)
  "Write RESULT to the node's positive output.

Optional argument LINKS is passed to the status output writing function."
  (setq result (epoxide-decision--thing-to-string result))
  (when (> (length result) 0)
    (epoxide-write-node-output
     (if (equal "\n" (substring result (1- (length result))))
	 result
       (concat result "\n"))
     (nth 0 epoxide-node-outputs))
    (epoxide-decision--write-status-output (epoxide-decision--timestamp)
					   t links)))

(defun epoxide-decision--write-negative-output (char-count &optional links
							   verbose)
  "Write timestamp and CHAR-COUNT on the node's negative output.

Optional argument LINKS is passed to the status output writing function.
Optional argument VERBOSE contains the characters that failed on the
decision function."
  (let ((timestamp (epoxide-decision--timestamp))
	(links (if (null links)
		   `(,`("every input"
			,(concat
			  (epoxide-decision--propertize-failed "timeout")
			  " -- no input was available")))
		 links)))
    (epoxide-write-node-output (concat timestamp
				       " (" (number-to-string char-count)
				       " character(s) processed"
				       (if verbose
					   (concat ": " verbose)
					 "")
				       ")\n")
			       (nth 1 epoxide-node-outputs))
    (epoxide-decision--write-status-output timestamp nil links)))

(defun epoxide-decision--write-status-output (timestamp result &optional links)
  "Write TIMESTAMP and RESULT on the node's status output.

When optional argument LINKS is present it is appended to the
current status report to indicate the reason of the
decision.  LINKS is a list containing the input-by-input results."
  (let* ((name (concat epoxide-node-name ": "))
	 (r (concat
	     (epoxide-decision--evaluate-2 result name)
	     (epoxide-decision--evaluate result)))
  	 (timestamp (concat "@" timestamp))
  	 (reason (if links
  	 	     (concat "reason:\n"
  	 		     (mapconcat (lambda (x)
  	 				  (concat (car x) ": "
  	 					  (cadr x)))
  	 				links "\n"))))
  	 (output (delq nil (cons timestamp (cons reason nil))))
  	 (output (mapconcat 'identity output "\n")))
    (epoxide-write-node-output (propertize (concat r "\n") 'face
					   (if result
					       'success
					     'isearch-fail))
  			       (nth 2 epoxide-node-outputs))
    (epoxide-write-node-output (concat output "\n"
				       (make-string 10 ?*) "\n")
  			       (nth 2 epoxide-node-outputs))))

(defun epoxide-decision--timestamp ()
  "Create a timestamp from the current time."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (format "%s:%s"
			(substring x 0 3)
			(substring x 3 5)))
    (format-time-string "%z"))))

(defun epoxide-decision--insert-other-input (arguments inputs n)
  "Add data from an other input to the arguments of the decision function.

Iterate through ARGUMENTS and when the form 'input-x is found
replace that argument with the data read from input-x (contained
in INPUTS).  When line-by-line processing is enabled for the
input, take the Nth line from the data."
  ;; FIXME: not to best way to parse inputs, no error handling.
  (let (new-arguments)
    (dolist (arg arguments)
      (when (and (> (length arg) 7)
		 (equal (substring arg 0 7) "'input-"))
	(let* ((count (substring arg 7))
	       (by-line t))
	  (if (equal (downcase (substring count (1- (length count)))) "l")
	      (setq count (string-to-number
			   (substring count 0 (1- (length count)))))
	    (setq by-line nil)
	    (setq count (string-to-number count)))
	  (if by-line
	      (setq arg (nth n (split-string (nth count inputs) "\n")))
	    (setq arg (nth count inputs)))))
      (setq new-arguments (cons arg new-arguments)))
    (nreverse new-arguments)))

(defun epoxide-decision--get-clock-input ()
  "Look for the first Clock input among the node's inputs."
  (let ((i 0)
	(found nil))
    (while (< i (length epoxide-node-inputs))
      (with-current-buffer (nth i epoxide-node-inputs)
	(with-current-buffer epoxide-src-node
	  (when (and (equal epoxide-node-class "Clock")
		     (null found))
	    (setq found i))))
      (incf i))
    found))

(defun epoxide-decision--timed-out-p (inputs timeout)
  "Return the last tick of the Clock input when a timeout occures.

INPUTS contain the data from all inputs of the node and TIMEOUT
specifies when to signal a timeout in ticks."
  (when timeout
    (let ((last-tick (epoxide-decision--get-last-tick inputs)))
      (when last-tick
	(setq epoxide-decision-last-tick last-tick)
	(when (>= last-tick (+ timeout epoxide-decision-first-tick))
	  (setq epoxide-decision-first-tick last-tick))))))

(defun epoxide-decision--get-last-tick (inputs)
  "Return the last tick of the first clock input from INPUTS."
  (let ((clock-input (epoxide-decision--get-clock-input))
	last-tick)
  (when clock-input
    (setq last-tick
	  (car (last (split-string (nth clock-input inputs) "\n" t))))
    (if last-tick
	(string-to-number last-tick)
      nil))))

(defun epoxide-decision--thing-to-string (thing)
  "Convert THING to string."
  (cond
   ((stringp thing)
    thing)
   ((numberp thing)
    (number-to-string thing))))

(defun epoxide-decision--number-or-nil (string)
  "Return nil when STRING is a string nil, convert it to number otherwise."
  (when string
    (string-to-number string)))

(defun epoxide-decision-stop ()
  "Dummy function."
  nil)

(provide 'decision)

;;; Decision.el ends here
