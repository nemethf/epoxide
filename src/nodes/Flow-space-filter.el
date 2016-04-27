;;; Flow-space-filter.el --- EPOXIDE Flow space filter node definition file

;; Copyright (C) 2014-2016 István Pelle

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

;; Node provides support to select only that flow space that is wished to be
;; seen.
;; It has one input that should contain the data to be filtered.
;; The node takes one configuration parameter that specifies the flow space.
;; Flow spaces can be specified be using the format:
;; <source parameter name 1>=<source parameter value 1>;<source parameter
;; name 2>=<source parameter value 2>-><destination parameter name 1>=
;; <destination parameter value 1>
;; Parameter names are those that appear in the data to be filtered.
;; Node has one output buffer that contains the data from the input buffer
;; that satisfies the filter condition.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs))

(defvar epoxide-flow-space-filter-node-info
  "Provide support for selecting only a specific flow.")

(defun epoxide-flow-space-filter-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "data to be filtered")
     (value-helper . epoxide-list-output-buffers)
     (value-validator . epoxide-output-buffer-p))))

(defun epoxide-flow-space-filter-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "flow space")
     (value-helper . nil)
     (value-validator . nil))))

(defun epoxide-flow-space-filter-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "filtered data"))))

(defun epoxide-flow-space-filter-init ()
  "Dummy function."
  nil)

(defun epoxide-flow-space-filter-exec ()
  "Return only those flow statistics that belong to the requested flow space."
  (let* ((data-buffer (nth 0 epoxide-node-inputs))
	 (flow-space (nth 0 epoxide-node-config-list))
	 (output (nth 0 epoxide-node-outputs))
	 (flow-stats (epoxide-node-get-inputs-as-string
		      (epoxide-node-read-inputs)))
	 flow-stat-list result
	 src-params dst-params params match)
    (when output
      ;; Parse flow space filter criteria.
      (when (> (length flow-stats) 0)
	;; Apply filter.
	(if (null flow-space)
	    (when (> (length flow-stats) 1)
	      (setq result
		    `(,(substring flow-stats 0 (1- (length flow-stats))))))
	  (setq src-params
		(epoxide-flow-space-filter--get-part flow-space 'src))
	  (setq dst-params
		(epoxide-flow-space-filter--get-part flow-space 'dst))
	  (setq params (append src-params dst-params))
	  (when (and src-params dst-params)
	    (setq flow-stat-list (split-string flow-stats "\n"))
	    (dolist (flow-stat flow-stat-list)
	      (setq match nil)
	      (dolist (param params)
		(if (string-match param flow-stat)
		    (setq match t)
		  (setq match nil)
		  (return)))
	      (when match
		(setq result (cons flow-stat result))))))
	;; Print results.
	(when result
	  (epoxide-write-node-output
	   (concat (mapconcat 'identity (nreverse result) "\n") "\n")
	   output))))))

(defun epoxide-flow-space-filter--get-part (flow-space type)
  "Parse a flow space definition.

FLOW-SPACE is the entire flow space definition.  TYPE is the
requested part, it should be either 'src for the source or 'dst
for destination."
  (let ((parts (split-string flow-space "->"))
	part)
    (cond
     ((eq type 'src)
      (setq part (nth 0 parts)))
     ((eq type 'dst)
      (setq part (nth 1 parts))))
    (when part
      (mapcar 'epoxide-chomp (split-string part ";")))))

(defun epoxide-flow-space-filter-stop ()
  "Dummy function."
  nil)

(provide 'flow-space-filter)

;;; Flow-space-filter.el ends here
