;;; Clock.el --- EPOXIDE Clock node definition file

;; Copyright (C) 2015      Tamás Lévai
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

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-clock-tick-counter)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs))

(defvar epoxide-clock-timer "Timer for clock functions.")

(defun epoxide-clock-input-info ()
  "Provide documentation, value tips and validation for input fields."
  nil)

(defun epoxide-clock-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "Tick rate in seconds.")
     (value-helper . '("1" "2" "5" "10"))
     (value-validator . epoxide-positive-number-p))))

(defun epoxide-clock-output-info ()
  "Provide documentation, value tips and validation for output fields."
  `(((doc-string . ,(concat "Clock output expands at every tick with "
			   "a new line containing the actual number "
			   "of ticks happened.")))))

(defun epoxide-clock-init ()
  "Initialise Clock node related buffer-local variables and hooks."
  (set (make-local-variable 'epoxide-clock-timer) nil)
  (set (make-local-variable 'epoxide-clock-tick-counter) 0)
  ;; Generate tick when the content of the buffer changes.
  (add-hook 'after-change-functions 'epoxide--clock-manual-tick nil t))

(defun epoxide--clock-manual-tick (b e c)
  "Generate tick and restart timer. Called when the buffer changes."
  (when (timerp epoxide-clock-timer)
    (cancel-timer epoxide-clock-timer))
  (epoxide--clock-callback (current-buffer))
  (setq epoxide-clock-timer
	(run-with-timer 0
			(string-to-number (nth 0 epoxide-node-config-list))
			'epoxide--clock-callback (current-buffer))))

(defun epoxide--clock-callback (buffer)
  "Called when buffer-local epoxide-clock-timer ticks."
  (with-current-buffer buffer
    (setq-local epoxide-clock-tick-counter (+ epoxide-clock-tick-counter 1))
    (let ((tick-counter epoxide-clock-tick-counter)
	  (padding ""))
      (dolist (output epoxide-node-outputs)
	(when output
	  (if (< tick-counter 10)
	      (setq padding "0"))
	  (epoxide-write-node-output
	   (concat padding (number-to-string tick-counter) "\n") output))))))

(defun epoxide-clock-exec ()
  "Start the buffer-local timer of the clock node."
  (when (timerp epoxide-clock-timer)
    (cancel-timer epoxide-clock-timer))
  (setq epoxide-clock-timer
	(run-with-timer 0
			(string-to-number (nth 0 epoxide-node-config-list))
			'epoxide--clock-callback (current-buffer))))

(defun epoxide-clock-stop ()
  "Cancel Clock node timer."
  (when (timerp epoxide-clock-timer)
    (cancel-timer epoxide-clock-timer)))

(provide 'clock)

;;; Clock.el ends here
