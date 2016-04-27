;;; Escape.el --- EPOXIDE Escape node definition file

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

;; Node is able to query NFFG topology information from Escape.  The
;; node has one input that should be the output of a Clock node which
;; supplies enable signal to the node.  It has three configuration
;; arguments.  The first specifies the host where Escape runs at.  The
;; second specifies the port where the REST API is accessible.  For
;; ease of use not port but the REST API name should be provided here.
;; By customizing the `epoxide-escape-apis' variable assignments can
;; be added or changed in the `epoxide-escape' customization group.
;; The third configuration argument specifies the currently used data
;; format.  This can be either JSON or XML.  Both are supported by
;; Escape and only its configuration decides which format it will use
;; when returning the answer for the REST API call.  The node has two
;; outputs where it transfers the topology information.  The output
;; format is such that it can be processed by a Graph node (see the
;; documentation of the graph node for the format). The first output
;; conveys the basic node and connection informations while the second
;; forwards detailed node information.  Information from Escape is
;; retrieved by way of REST API calls.

;;; Code:

(require 'epoxide)
(require 'json)    ; For JSON processing.
(require 'url)     ; For accessing REST APIs.
(require 'xml)     ; For XML processing.

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defgroup epoxide-escape nil
  "Escape node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-escape-apis '(("ros" "8888" "get-config")
				 ("sas" "8008" "topology"))
  "Assocation list containing REST API name and port pairs."
  :type '(repeat
	  (list
	   (string :tag "API name:")
	   (string :tag "Port:")
	   (string :tag "Function:")))
  :group 'epoxide-escape)

(defun epoxide-escape-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "Clock."))))

(defun epoxide-escape-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "Host where Escape runs at."))
    ((doc-string . "Name of the REST level to be accessed."))
    ((doc-string . "Data format (XML or JSON)."))))

(defun epoxide-escape-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "Graph description."))
    ((doc-string . "Every node attribute."))))

(defun epoxide-escape-init ()
  "Dummy function."
  nil)

(defun epoxide-escape-exec ()
  "Query Escape for topology information and reformat retrieved data."
  (let* ((host (car epoxide-node-config-list))
	 (layer (nth 1 epoxide-node-config-list))
	 (data-format (nth 2 epoxide-node-config-list))
	 (data-format (when data-format
			(upcase data-format)))
	 (port (nth 1 (assoc layer epoxide-escape-apis)))
	 (function (nth 2 (assoc layer epoxide-escape-apis))))
    (when (and port function
	       (epoxide-node-get-inputs-as-string (epoxide-node-read-inputs)))
      (let* ((input (epoxide-escape--get-topo host port function))
	     (topo (when input
		     (pcase data-format
		       ("JSON" (epoxide-escape--parse-topo
				(json-read-from-string input)))
		       ("XML" (epoxide-escape--parse-xml input)))))
	     (displayable (cdr (assoc 'displayable topo)))
	     (details (cdr (assoc 'details topo))))
	(when topo
	  (epoxide-write-node-output displayable (car epoxide-node-outputs))
	  (epoxide-write-node-output details (cadr epoxide-node-outputs)))))))

(defun epoxide-escape--get-topo (host port function)
  "Query topology information from accessing a REST API at HOST on PORT.

FUNCTION specifies which layer to query."
  (setq-local url-show-status nil)
  (let* ((url (concat "http://" host ":" port "/escape/" function))
	 (url-request-method "GET")
	 (url-request-extra-headers)
	 (url-request-data)
	 (result-buffer (url-retrieve-synchronously url))
	 (result (with-current-buffer result-buffer
		   (goto-char (point-min))
		   (when (> (point-max) 1)
		     (condition-case nil
			 (progn
			   (goto-char url-http-end-of-headers)
			   (buffer-substring-no-properties
			    (- (point) 1) (point-max)))
		       (error nil))))))
    (kill-buffer result-buffer)
    result))

(defun epoxide-escape--parse-topo (topo)
  "Reformat elisp representation of TOPO to text based graph description."
  (let* ((saps (mapcar 'identity (cdr (assoc 'node_saps topo))))
	 (infras (mapcar 'identity (cdr (assoc 'node_infras topo))))
	 (nfs (mapcar 'identity (cdr (assoc 'node_nfs topo))))
	 (edges (mapcar 'identity (cdr (assoc 'edge_links topo))))
	 (saps (epoxide-escape--parse-saps saps))
	 (infras (epoxide-escape--parse-infras infras))
	 (nfs (epoxide-escape--parse-nfs nfs))
	 (nodes-to-display
	  (concat
	   (cdr (assoc 'displayable saps)) ", "
	   (cdr (assoc 'displayable infras)) ", "
	   (cdr (assoc 'displayable nfs))))
	 (details
	  (mapconcat 'identity
		     (delete "" (append `(,(cdr (assoc 'details saps)))
					`(,(cdr (assoc 'details infras)))
					`(,(cdr (assoc 'details nfs))))) ", "))
	 (edges (epoxide-escape--parse-edges edges)))
    `(,`(displayable . ,(concat "nodes: " nodes-to-display
				"\nedges: " edges "\n"))
      ,`(details . ,(concat details "\n")))))

(defun epoxide-escape--parse-saps (saps)
  "Parse nodes from among SAPS."
  (epoxide-escape--parse-nodes saps "Service Access Point"
			       '(name domain ports)))

(defun epoxide-escape--parse-infras (infras)
  "Parse nodes from among INFRAS."
  (epoxide-escape--parse-nodes infras "Infra node"
			       '(name ports domain type supported resources)))

(defun epoxide-escape--parse-nfs (nfs)
  "Parse nodes from among NFS."
  (epoxide-escape--parse-nodes nfs "Network function"
			       '(name ports domain type supported resources)))

(defun epoxide-escape--parse-nodes (nodes type attributes)
  "Parse NODES having TYPE and ATTRIBUTES."
  (let (vertices
	all-data)
    (dolist (n nodes)
      (let* ((name (cdr (assoc 'id n)))
	     (name-with-type (concat name "[" type "]"))
	     params all-params)
	(dolist (a attributes)
	  (let* ((details (epoxide-escape--to-string (cdr (assoc a n))))
		 (attribute (if (equal a 'ports)
				(epoxide-escape--ports-to-string
				 (cdr (assoc a n)))
			     details)))
	    (when attribute
	      (push (concat (symbol-name a) "=" attribute)
		    params))
	    (when details
	      (push (concat (symbol-name a) "=" details)
		    all-params))))
	(push (concat name-with-type
		      (epoxide-escape--create-node-params-list params))
	      vertices)
	(when all-params
	  (push (concat name-with-type
			(epoxide-escape--create-node-params-list all-params))
		all-data))))
    `(,`(displayable . ,(mapconcat 'identity (nreverse vertices) ", "))
      ,`(details . ,(mapconcat 'identity (nreverse all-data) ", ")))))

(defun epoxide-escape--create-node-params-list (params)
  "Create a string describing node parameters recived in PARAMS."
  (if params
      (concat "("
	      (mapconcat 'identity
			 (nreverse params) "; ") ")")
    ""))

(defun epoxide-escape--parse-edges (edges)
  "Parse EDGES."
  (let ((attributes '(src_port dst_port delay bandwidth))
	es)
    (dolist (e edges)
      (let ((src-node (cdr (assoc 'src_node e)))
	    (dst-node (cdr (assoc 'dst_node e)))
	    (backward (cdr (assoc 'backward e)))
	    params)
	(unless backward
	  (dolist (a attributes)
	    (let ((attribute (epoxide-escape--to-string (cdr (assoc a e)))))
	      (when attribute
		(push (concat (symbol-name a) "=" attribute)
		      params))))
	  (push (concat src-node " <-> " dst-node
			(if params
			    (concat
			     " ("
			     (mapconcat 'identity
					(nreverse params) "; ")
			     ")")
			  ""))
		es))))
    (mapconcat 'identity (nreverse es) ", ")))

(defun epoxide-escape--ports-to-string (arg)
  "Convert ARG to string if ARG describes ports.

Properties are filtered out."
  (let ((ports (mapcar 'identity arg))
	ret)
    (dolist (p ports)
      (unless (or (equal (caar p) 'property)
		  (equal (caar p) 'flowrules))
	(push (epoxide-escape--to-string p) ret)))
      (mapconcat 'identity (nreverse ret) ",")))

(defun epoxide-escape--to-string (arg)
  "Convert ARG to string."
  (cond
   ((null arg)
    nil)
   ((stringp arg)
    (epoxide-chomp arg))
   ((numberp arg)
    (number-to-string arg))
   ((symbolp arg)
    (symbol-name arg))
   ;; FIXME: find a better way to identify associations
   ((and (listp arg)
   	 (condition-case nil
   	     (when (length arg)
   	       nil)
   	   (error t)))
    (concat (epoxide-escape--to-string (car arg)) "="
	    (epoxide-escape--to-string (cdr arg))))
   ((listp arg)
    (let (ret)
      (mapconcat 'identity
		 (delq nil (dolist (a arg ret)
			     (push (epoxide-escape--to-string a) ret)))
		 ",")))
   ((arrayp arg)
    (epoxide-escape--to-string (mapcar 'identity arg)))))

(defun epoxide-escape--parse-xml (topo)
  "Reformat XML representation of TOPO to text based graph description."
  (let* ((root (with-temp-buffer
	 	 (insert topo)
	 	 (xml-parse-region (point-min) (point-max))))
	 (post (car root))
	 (nodes (xml-get-children (car (xml-get-children post 'nodes)) 'node))
	 (links (xml-get-children (car (xml-get-children post 'links)) 'link))
	 (func-1 (lambda (x y)
		   (nth 2 (car (xml-get-children x y)))))
	 (get-resources (lambda (x)
			  (let ((resources (cddr (car (xml-get-children
						       x 'resources))))
				ret)
			    (dolist (r resources)
			      (when (listp r)
				(push (concat (symbol-name (car r)) "="
					      (nth 2 r)) ret)))
			    (nreverse ret))))
	 (func-2 (lambda (x)
		   (when (string-match "node\\[" x)
		     (substring x (+ (string-match "node\\[" x) 8)
				(string-match "\\]" x)))))
	 node-list link-list)
    (dolist (node nodes)
      ;; TODO: collect additional tags for details.
      (let ((resources (apply get-resources `(,node)))
	    (node-id (apply func-1 `(,node id)))
	    (nf-instances
	     (xml-get-children (car (xml-get-children node 'NF_instances))
			       'node)))
	(push (concat node-id "["
		      (apply func-1 `(,node type)) "](name="
		      (apply func-1 `(,node name)) "; resources="
		      (if (null resources)
			  ""
			(mapconcat 'identity resources ","))
		      ")") node-list)
	(dolist (l (xml-get-children (car (xml-get-children node 'links))
				     'link))
	  (let* ((src (nth 2 (car (xml-get-children l 'src))))
		 (dst (nth 2 (car (xml-get-children l 'dst))))
		 (src (when src
			(apply func-2 `(,src))))
		 (dst (when dst
			(apply func-2 `(,dst)))))
	    (when (and src dst
		       (not (equal src dst)))
	      (push `(,src ,dst) link-list))))
	(dolist (nf nf-instances)
	    (let ((resources (apply get-resources `(,nf)))
		  (id (apply func-1 `(,nf id)))
		  (type (apply func-1 `(,nf type)))
		  (name (apply func-1 `(,nf name))))
	      (push (concat id "["
			    type "](name="
			    name "; resources="
			    (if (null resources)
				""
			      (mapconcat 'identity resources ","))
			    ")") node-list)
	      (push `(,node-id ,id) link-list)))))
    (setq node-list (nreverse node-list))
    (dolist (l links)
      (let* ((src (nth 2 (car (xml-get-children l 'src))))
	     (dst (nth 2 (car (xml-get-children l 'dst))))
	     (src (when src
		    (apply func-2 `(,src))))
	     (dst (when dst
		    (apply func-2 `(,dst)))))
	(when (and src dst
		   (not (equal src dst)))
	  (push `(,src ,dst) link-list))))
    `(,`(displayable . ,(concat "nodes: " (mapconcat 'identity node-list ", ")
				"\nedges: "
				(mapconcat (lambda (x)
					     (concat (car x) " <-> " (cadr x)))
					   link-list ", ") "\n")))))

(defun epoxide-escape-stop ()
  "Dummy function."
  nil)

(provide 'escape)

;;; Escape.el ends here
