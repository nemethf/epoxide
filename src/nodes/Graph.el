;;; Graph.el --- EPOXIDE Graph visualization node definition file

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

;; This node provides graph visualization support.  The node has two
;; inputs.  On the first input it receives the graph description.  This
;; description should contain two lines: the first specifying the
;; nodes, the second the edges of the graph.  Node specification
;; should follow this form: 'nodes:
;; node-1[type-1](attribute-1=value-1;
;; attribute-2=subattribute-2-1=value2-1,subattribute-2-2=value-2-1...),
;; node-2[type-1]...'.  Edge definition should follow this formula:
;; 'edges: start-node-1 <-> end-node-1 (attribute-1=value-1),
;; start-node-2 <-> end-node-2...'.  Attributes can be any other
;; parameter that is needed to be displayed beside the node's name or
;; the edge association, or they can be left out entirely.  When
;; defining an edge the following operators can be used: <->, -> and
;; -.  The optional second input should convey node details: data that
;; might be interesting but is too long to be displayed with the graph
;; visualization.  It should use node descriptions like with the first
;; input.  The graph can be displayed in two ways:

;; * Text mode: nodes and edges and their attributes are listed in
;; text.  Here attributes for nodes and edges can also be displayed by
;; setting the `epoxide-graph-show-attributes-in-text-view' epoxide
;; group custom variable to t.

;; * visualization: using COGRE and Graphviz a graph is drawn.  Here
;; only node attributes can be displayed by setting the
;; `epoxide-graph-show-attributes-in-graph-view' epoxide group
;; custom variable to t.

;; Switching between the two modes can be achieved by using C-c
;; C-c.  The default behavior can be customized via the
;; `epoxide-graph-default-view' epoxide group custom variable.

;; Node names appear as buttons.  Selecting these activate a
;; semantic-popup-menu that is able to display node details received
;; on the second input.

;; The node takes no configuration parameters and has no outputs.

;;; Code:

(require 'epoxide)
(require 'cogre)
(require 'cogre/mode)

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-graph--nodes)
  (defvar epoxide-graph--edges)
  (defvar epoxide-graph-show-attributes-in-graph-view-prev)
  (defvar epoxide-graph--node-details)
  (defvar epoxide-graph--current-node)
  (defvar epoxide-graph--popup-menu))

(defgroup epoxide-graph nil
  "Graph node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-graph-default-view 'graph
  "Define which how data should be displayed by default.
Available values are:
`graph' : show a visualization of the graph
`text'  : list the graph nodes and edges as text"
  :type '(radio
	  (const graph)
	  (const text))
  :group 'epoxide-graph)

(defcustom epoxide-graph-show-attributes-in-text-view t
  "When t show node and link attributes when present."
  :type 'boolean
  :group 'epoxide-graph)

(defcustom epoxide-graph-show-attributes-in-graph-view t
  "When t show node and link attributes when present."
  :type 'boolean
  :group 'epoxide-graph)

(defvar epoxide-graph--text-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybindings of epoxide-mode.
    (set-keymap-parent map epoxide-mode-map)
    ;; Switching between display modes.
    (define-key map (kbd "C-c C-c") 'epoxide-graph--change-view)
    map)
  "Keymap for `epoxide-graph--text-mode'.")

(define-derived-mode epoxide-graph--text-mode epoxide-mode
  "Mode for displaying a graph by listing nodes and edges in text.
Key definitions:
\\{epoxide-graph--mode-map}"
  :group 'epoxide
  (setq mode-name "Epoxide graph text"))

(defvar epoxide-graph--mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybindings of epoxide-mode.
    (set-keymap-parent map epoxide-mode-map)
    ;; Disable RET.
    (define-key map (kbd "RET") 'epoxide-graph--ret)
    ;; Enable dragging nodes.
    (define-key map (kbd "<down-mouse-1>")
      (lambda (event)
	(interactive "e")
	(epoxide-graph--down-mouse-1-click event)))
    ;; Switching between display modes.
    (define-key map (kbd "C-c C-c") 'epoxide-graph--change-view)
    map)
  "Keymap for `epoxide-graph--mode'.")

(define-derived-mode epoxide-graph--mode cogre-mode
  "Mode for displaying a graph by visualizing it with COGRE.
Key definitions:
\\{epoxide-graph--mode-map}"
  :group 'epoxide
  (setq-local kill-buffer-hook (cons 'epoxide-tsg-kill kill-buffer-hook))
  (setq mode-name "Epoxide graph"))

(defun epoxide-graph-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "graph description"))
    ((doc-string . "node details"))))

(defun epoxide-graph-config-info ()
  "Provide documentation, value tips and validation for config fields."
  nil)

(defun epoxide-graph-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "not used")
     (auto-create . nil))))

(defun epoxide-graph-init ()
  "Initialise Graph node related buffer-local variables set default view."
  (let ((vars-nil '(epoxide-graph--nodes epoxide-graph--edges
	            epoxide-graph--popup-menu epoxide-graph--current-node
		    epoxide-graph--node-details epoxide-input-markers)))
    (dolist (var vars-nil)
      (set (make-local-variable var) nil)))
  (set (make-local-variable 'epoxide-graph-show-attributes-in-graph-view-prev)
       epoxide-graph-show-attributes-in-graph-view)
  (epoxide-graph--create-popup-menu)
  (epoxide-graph--change-view))

(defun epoxide-graph--change-view ()
  "Change node display between graph visualization and text display."
  (interactive)
  (pcase major-mode
    (`epoxide-mode
     (pcase epoxide-graph-default-view
       (`graph
	(epoxide-graph--switch-to-graph (current-buffer)))
       (`text
	(epoxide-graph--switch-to-text (current-buffer)))))
    (`epoxide-graph--text-mode
     (epoxide-graph--switch-to-graph (current-buffer)))
    (`epoxide-graph--mode
     (epoxide-graph--switch-to-text (current-buffer))))
  (epoxide-graph-show))

(defun epoxide-graph-exec ()
  "Process input and display it."
  (let ((input (epoxide-node-get-inputs-as-list (epoxide-node-read-inputs))))
    (epoxide-graph--read-graph (car input))
    (epoxide-graph--read-node-details (cadr input))
    (epoxide-graph-show)))

(defun epoxide-graph--read-graph (input)
  "Read graph description from INPUT."
  (let* (nodes nodes-set edges edges-set)
    (when input
      (setq input (nreverse (split-string input "\n")))
      (dolist (i input)
	(when (> (length i) 0)
	  (when (and (null nodes-set)
		     (equal (substring i 0 4) "node"))
	    (setq nodes (epoxide-graph--get-items i))
	    (setq nodes-set t))
	  (when (and (null edges)
		     (equal (substring i 0 4) "edge"))
	    (setq edges (epoxide-graph--get-items i))
	    (setq edges-set t)))
	(when (and nodes-set edges-set)
	  (return)))
      (setq-local epoxide-graph--nodes nodes)
      (setq-local epoxide-graph--edges edges))))

(defun epoxide-graph--read-node-details (input)
  "Read node details from INPUT."
  (let* ((input (if input
		    (car (last (split-string input "\n" t)))))
  	 nodes)
    (when input
      (setq-local epoxide-graph--node-details
		  (mapcar (lambda (x)
			    (let ((name (substring x 0 (string-match "\\[" x)))
				  (attributes-start (1+ (string-match "(" x))))
			      `(,name . ,(substring x attributes-start))))
			  (split-string input ")," t "\s"))))))

(defun epoxide-graph--get-items (data)
  "Create an alist from the received graph description in DATA."
  (let ((items (split-string (substring data 7) ", " t "\s"))
	ret)
    (dolist (i items)
      (cond
       ((string-match "[[:alnum:]-]+\\[.*\\](.*)" i)
	(push `((item . ,(epoxide-chomp (substring i 0 (string-match "\\[" i))))
		(type . ,(epoxide-chomp (substring i (1+ (string-match "\\[" i))
						   (string-match "\\]" i))))
		(attributes . ,(epoxide-chomp
				(substring i (1+ (string-match "(" i))
					   (string-match ")" i))))) ret))
       ((string-match "[[:alnum:]-]+\\[.*\\]" i)
	(push `((item . ,(epoxide-chomp (substring i 0 (string-match "\\[" i))))
		(type . ,(epoxide-chomp (substring i (1+ (string-match "\\[" i))
						   (string-match "\\]" i))))) ret))
       ((string-match "[[:alnum:]-:]+[\s]+<->[\s]+[[:alnum:]-:]+[\s]+(.*)" i)
	(push `((item . ,(epoxide-chomp (substring i 0 (string-match "(" i))))
		(attributes . ,(epoxide-chomp
				(substring i (1+ (string-match "(" i))
					   (string-match ")" i))))) ret))
       (t
	(push `((item . ,i)) ret))))
    (nreverse ret)))

(defun epoxide-graph-show ()
  "Display nodes and edges either in text or with visualization using COGRE."
  (pcase major-mode
    (`epoxide-graph--text-mode
     (epoxide-graph-show-as-text epoxide-graph--nodes epoxide-graph--edges))
    (`epoxide-graph--mode
     (epoxide-graph-show-using-cogre epoxide-graph--nodes
				     epoxide-graph--edges))))

;; TODO: make buttons from node names to invoke popup menu.
(defun epoxide-graph-show-as-text (nodes edges)
  "Display NODES and EDGES as a text based list."
  (let ((line (1- (line-number-at-pos)))
	(pos-in-line (- (point) (line-beginning-position))))
    (erase-buffer)
    (insert (propertize (concat "nodes:\t" (mapconcat
					    'epoxide-graph--display-as-text
					    nodes "\n\t")
				"\n\nedges:\t"
				(mapconcat 'epoxide-graph--display-as-text
					   edges "\n\t")
				"\n\n\n" (make-string 80 ?-) "\n")
			'face 'font-lock-doc-face))
    (insert "\n\n"
	    "To switch to graph visualization press C-c C-c"
	    "\n\n")
    (epoxide-insert-node-basics)
    (insert "\n\n")
    (epoxide-insert-key-bindings "epoxide-graph--text-mode-map")
    (setq-local truncate-lines t)
    (goto-char (point-min))
    (forward-line line)
    (if (> (line-end-position) (+ (line-beginning-position) pos-in-line))
	(forward-char pos-in-line)
      (goto-char (line-end-position)))))

(defun epoxide-graph--display-as-text (element)
  "Display node or edge ELEMENT as text."
  (let ((attributes (assoc 'attributes element))
	(type (assoc 'type element))
	(ret (cdr (assoc 'item element))))
    (when epoxide-graph-show-attributes-in-text-view
      (when type
    	(setq ret (concat ret (concat "[" (cdr type) "]"))))
      (when attributes
    	(setq ret (concat ret "(" (replace-regexp-in-string
				   "," ", " (cdr attributes)) ")"))))
    ret))

(defun epoxide-graph--switch-to-graph (node-buffer)
  "Switch node display to graph vizualization with COGRE.

NODE-BUFFER is the currently used node buffer."
  (let ((cgr-file
	 (find-file-noselect (concat (buffer-name node-buffer) ".cgr"))))
    (unless (get-buffer-window cgr-file)
      (switch-to-buffer cgr-file)
      (auto-save-mode t)
      ;; Switch to the node's own mode from cogre-mode, copy required
      ;; local variables, kill NODE-BUFFER then replace it with this
      ;; new COGRE based buffer.
      (unless (equal major-mode 'epoxide-graph--mode)
      	(epoxide-graph--mode)
	(epoxide-graph--copy-node-attributes node-buffer (current-buffer))
	(let ((old-node-buffer-name (concat (buffer-name node-buffer) "-old")))
	  (with-current-buffer node-buffer
	    (rename-buffer old-node-buffer-name))
	  (set-buffer-modified-p nil)
	  (rename-buffer (substring (buffer-name) 0
				    (- (length (buffer-name)) 4)))
	  (with-current-buffer old-node-buffer-name
	    ;; When closing the old node buffer, it should not ask to
	    ;; kill the whole Epoxide session.
	    (fundamental-mode)
	    (setq-local kill-buffer-hook (delete 'epoxide-tsg-kill
						 kill-buffer-hook))
	    (kill-buffer))))
      ;; Use unicode characters when displaying the graph.
      (when epoxide-cogre-enable-unicode
	(cogre-uml-enable-unicode))
      ;; Set font to one that looks OK.
      (let ((display-font "DejaVu Sans Mono"))
	(when (member display-font (font-family-list))
	  (buffer-face-set `(:family ,display-font)))))))

(defun epoxide-graph--switch-to-text (node-buffer)
  "Switch to text display.

NODE-BUFFER is the buffer currently associated the the Graph node."
  (let* ((new-buffer-name (concat (buffer-name node-buffer) "-new"))
	 (new-buffer (get-buffer-create new-buffer-name)))
    ;; Switch to the node's own mode from fundamental-mode, copy required
    ;; local variables, kill NODE-BUFFER then replace it with this
    ;; new text based buffer.
    (with-current-buffer new-buffer
      (epoxide-graph--text-mode)
      (epoxide-graph--copy-node-attributes node-buffer (current-buffer)))
    (switch-to-buffer new-buffer)
    (with-current-buffer node-buffer
      (fundamental-mode)
      (set-buffer-modified-p nil)
      (setq-local kill-buffer-hook (delete 'epoxide-tsg-kill kill-buffer-hook))
      (kill-buffer))
    (with-current-buffer new-buffer
      (rename-buffer (substring new-buffer-name 0
				(- (length new-buffer-name) 4))))))

(defun epoxide-graph--process-edges (edges)
  "Process the list of EDGES and return an association list."
  (let ((create-edge (lambda (start end direction)
		       `(,`(start . ,start) ,`(end . ,end)
			 ,`(direction . ,direction))))
	(es (mapcar 'epoxide-graph--get-item edges))
	points ret)
    (dolist (e es ret)
      (cond
       ((string-match "[[:alnum:]]+[\s]<->[\s][[:alnum:]]+" e)
	(setq points (split-string e "<->" t "\s"))
	(setq ret (cons (apply create-edge `(,(car points) ,(cadr points)
					     two-way)) ret)))
       ((string-match "[[:alnum:]]+[\s]->[\s][[:alnum:]]+" e)
	(setq points (split-string e "->" t "\s"))
	(setq ret (cons (apply create-edge `(,(car points) ,(cadr points)
					     one-way)) ret)))
       ((string-match "[[:alnum:]]+[\s]-[\s][[:alnum:]]+" e)
	(setq points (split-string e "->" t "\s"))
	(setq ret (cons (apply create-edge `(,(car points) ,(cadr points)
					     undirected)) ret)))))))

(defun epoxide-graph--get-item (data)
  "Return the item part of DATA."
  (cdr (assoc 'item data)))

(defun epoxide-graph--get-attributes (data)
  "Return the attributes part of DATA."
  (let ((attributes (cdr (assoc 'attributes data))))
    (if attributes
	attributes
      "")))

;; TODO: take care of node type changes. When using a POX controller a
;; host gets an IP address it makes it clear that the node is a host.
(defun epoxide-graph-show-using-cogre (nodes edges)
  "Display NODES and EDGES as a graph using COGRE."
  (setq edges (epoxide-graph--process-edges edges))
  (let* ((pos (vector 1 1))
  	 (dot-file-buffer-name "")
	 cogre-nodes new-layout-requested-p refresh-requested-p)
    (unless (equal epoxide-graph-show-attributes-in-graph-view
		   epoxide-graph-show-attributes-in-graph-view-prev)
      (dolist (e (append (epoxide-cogre-get-links)
      			 (epoxide-graph--cogre-get-nodes-and-classes)))
      	(cogre-delete e))
      (setq-local epoxide-graph-show-attributes-in-graph-view-prev
		  epoxide-graph-show-attributes-in-graph-view))
    ;; First create cogre nodes for nodes that are not yet displayed.
    (when cogre-graph
      (dolist (n nodes)
	(let* ((item (epoxide-graph--get-item n))
	       (type (cdr (assoc 'type n)))
	       (attributes (epoxide-graph--get-attributes n))
	       (c-node
		(object-assoc item
			      :object-name
			      (epoxide-graph--cogre-get-nodes-and-classes))))
	  (if (null c-node)
	      ;; Add a new cogre-node.
	      (let ((obj (epoxide-graph--create-cogre-node pos type attributes)))
		;; Set name of node.
		(cogre-set-element-name obj item)
		;; Request new layout when a new node has been added
		;; to the graph.
		(setq new-layout-requested-p t))
	    ;; Replace attributes, when they have changed.
	    (when (and epoxide-graph-show-attributes-in-graph-view
		       (not (epoxide-graph--attributes-equal-p
			     attributes (oref c-node :attributes))))
	      (oset c-node :attributes (epoxide-graph--create-cogre-attributes
					attributes))))))
      (setq cogre-nodes (epoxide-graph--cogre-get-nodes-and-classes))
      ;; Add new edges to the graph.
      (dolist (e edges)
      	;; Query the cogre modell whether it has the currently examined edge.
      	(let* ((start (object-assoc (cdr (assoc 'start e))
      				    :object-name cogre-nodes))
      	       (end (object-assoc (cdr (assoc 'end e))
      				  :object-name cogre-nodes))
	       ;; TODO: there might be a more effective way to do this
	       (link (let (ls)
		       (dolist (l (copy-sequence (epoxide-cogre-get-links)) ls)
			 (when (equal (oref l :start) start)
			   (push l ls)))))
      	       (link (if end (object-assoc end :end link)))
      	       (link-type (cdr (assoc 'direction e)))
      	       (arrow-type (pcase link-type
			     (`two-way nil)
			     (`undirected nil)
			     (`one-way
			      (if epoxide-cogre-enable-unicode
				  cogre-arrow
				cogre-small-arrow)))))
      	  (unless link
	    (when (and start end)
	      ;; Add a link if it has not yet been in the graph.
	      (cogre-new-link start end arrow-type)
	      ;; Request new layout when adding a new edge.
	      (setq new-layout-requested-p t)))))
      ;; Remove unused nodes and edges silently.
      (setq cogre-delete-dont-ask t)
      ;; Iterate through the list of edges and remove those that are
      ;; displayed on the graph but are not in the list of currently
      ;; active edges.
      (let ((cogre-links (epoxide-cogre-get-links))
      	    (links (mapcar (lambda (x)
      			     (concat
      			      (cdr (assoc 'start x))
      			      (cdr (assoc 'end x))))
      			   edges)))
      	(dolist (e cogre-links)
      	  (let ((start (oref (oref e :start) object-name))
      		(end (oref (oref e :end) object-name)))
      	    (unless (member (concat start end) links)
	      (setq refresh-requested-p t)
      	      (cogre-delete e)))))
      ;; Remove unused nodes.
      (setq nodes (mapcar 'epoxide-graph--get-item nodes))
      (dolist (n cogre-nodes)
      	(unless (member (oref n object-name) nodes)
	  (setq refresh-requested-p t)
      	  (cogre-delete n)))
      ;; Redraw graph.
      (let ((msg (current-message)))
	(when (or  new-layout-requested-p
		   refresh-requested-p)
	  ;; Silence some cogre messages.
	  (epoxide-log "Calling cogre ...")
	  (when new-layout-requested-p
	    ;; Use DOT for creating new layout.
	    (setq cogre-dot-node-position-scale '(4 . 10))
	    (cogre-layout)
	    ;; Kill cedet dot buffer.
	    (epoxide-kill-buffer "*CEDET graphviz dot*")
	    ;; Kill compilation log buffer.
	    (epoxide-kill-buffer "*Compile-Log*")
	    ;; Kill dot buffer.
	    (dolist (b (buffer-list))
	      (when (equal (concat (buffer-name) ".dot") (buffer-name b))
		(epoxide-kill-buffer b t))))
	  (when refresh-requested-p
	    (cogre-refresh))
	  (epoxide-log "Calling cogre ... done")
	  (if msg
	      (message "%s" msg)
	    (message nil))
	  (epoxide-graph--place-buttons)))
      (set-buffer-modified-p nil))))

(defun epoxide-graph--cogre-get-nodes-and-classes ()
  "Retreive cogre-nodes and cogre-classes from the current cogre graph."
  (append (epoxide-cogre-get-elements cogre-node)
	  (epoxide-cogre-get-elements cogre-class)))

(defun epoxide-graph--create-cogre-node (position &optional type attributes)
  "Create a new cogre node having POSITION, TYPE and ATTRIBUTES."
  (unless type
    (setq type ""))
  (if epoxide-graph-show-attributes-in-graph-view
      (cogre-new-node 1 'cogre-class :position position
		      :methods `(,`(,type method))
		      :attributes (epoxide-graph--create-cogre-attributes
				   attributes))
    (cogre-new-node 1 'cogre-node :position position)))

(defun epoxide-graph--create-cogre-attributes (attributes)
  "Transform the string ATTRIBUTES to a list satisfying cogre specifications."
  (let (ret)
    (nreverse
     (dolist (a (split-string attributes ";" t "\s") ret)
       (let ((bs (split-string a "," t "\s"))
	     (offset 0))
	 (when bs
	   (let* ((first (pop bs))
		  (off (string-match "=" first)))
	     (push `(,first variable) ret)
	     (when off
	       (setq offset (1+ off)))))
	 (dolist (b bs)
	   (push `(,(concat (make-string offset ?\s) b) variable) ret)))))))

(defun epoxide-graph--attributes-equal-p (attributes cogre-attributes)
  "Return t when the two arguments specify the same node attributes.

ATTRIBUTES is a string containing node attributes while
COGRE-ATTRIBUTES contains attributes retreived from a
cogre class."
  (when (equal (epoxide-graph--create-cogre-attributes attributes)
	       cogre-attributes)
    t))

(defun epoxide-graph--place-buttons ()
  "Make button of the node names on the graph."
  (save-excursion
    ;; Look up every node on the graph a create a button from their
    ;; names.
    (dolist (node (epoxide-graph--cogre-get-nodes-and-classes))
      (let* ((node-position (mapcar 'identity (oref node :position)))
	     (pos-x (1+ (car node-position)))
	     (pos-y (cadr node-position))
	     (name (oref node object-name)))
	(goto-char (point-min))
	(forward-line pos-y)
	(goto-char (+ (line-beginning-position) pos-x))
	(make-button (point) (+ (point) (length name))
		     'action (lambda (x)
			       (setq-local epoxide-graph--current-node
					   (button-label x))
			       (semantic-popup-menu epoxide-graph--popup-menu))
		     'follow-link t))))
  (set-buffer-modified-p nil))

;; TODO: inactivate details menu if details input is not present.
(defun epoxide-graph--create-popup-menu ()
  "Create a semantics popup menu."
  (let* ((details (list
		   (epoxide-graph--create-menu-item
		    "Node details"
		    'epoxide-graph-show-node-details
		    :active t)))
	 (em (cons "Node menu"
		   details)))
    (easy-menu-define epoxide-graph--popup-menu
      nil
      "Node menu"
      em)))

(defun epoxide-graph--create-menu-item (itemname function &rest attributes)
  "Build an easymenu compatible menu item.
Name will be ITEMNAME.  FUNCTION will be called.
ATTRIBUTES are easymenu compatible attributes."
  (apply #'vector itemname function attributes))

;; TODO: there is some unexpected behavior when invoking the menu with
;; mouse.
(defun epoxide-graph-show-node-details ()
  "Show node details in a new buffer."
  (interactive)
  (let* ((buf (get-buffer-create "*Node details*"))
	 (node-name epoxide-graph--current-node)
	 (header (concat "Details of node " node-name ":"))
	 (node-details (cdr (assoc node-name epoxide-graph--node-details))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert header "\n"
		(make-string (length header) ?~) "\n")
	(when node-details
	  (dolist (d (epoxide-graph--create-cogre-attributes node-details))
	    (insert (car d) "\n"))))
      (read-only-mode 1)
      (local-set-key "q" 'kill-buffer-and-window)
      (goto-char (point-min)))
    (unless (get-buffer-window buf)
      (split-window)
      (switch-to-buffer buf))))

(defun epoxide-graph--copy-node-attributes (buffer-1 buffer-2)
  "Copy graph node related local variables from BUFFER-1 to BUFFER-2."
  (let ((vars '(epoxide-node-name epoxide-node-class epoxide-node-inputs
		epoxide-node-config-list epoxide-node-outputs
		epoxide-root-buffer epoxide-init-function
		epoxide-exec-function epoxide-stop-function
		epoxide-input-markers
		epoxide-graph--nodes epoxide-graph--edges
		epoxide-graph-show-attributes-in-graph-view-prev
		epoxide-graph--popup-menu epoxide-graph--current-node
		epoxide-graph--node-details))
	(i 0)
	values)
    (with-current-buffer buffer-1
      (dolist (v (reverse vars))
	(setq values (cons (symbol-value v) values))))
    (with-current-buffer buffer-2
      (dolist (v vars)
	(set (make-local-variable v) (nth i values))
	(incf i)))))

(defun epoxide-graph--ret ()
  "Disable the RET key event in `epoxide-graph--mode'."
  (interactive))

(defun epoxide-graph--down-mouse-1-click (event)
  "Handles the EVENT down mouse 1 in `epoxide-graph--mode'.

It provides the same functionality as the respective function in
`cogre-mode'."
  (let ((posn (elt event 1)))
    (select-window (posn-window posn))
    (with-selected-window (posn-window posn)
      (goto-char (posn-point posn))
      (if (button-at (point))
  	  (button-activate (button-at (point)))
  	(cogre-down-mouse-1 event))
      (epoxide-graph--place-buttons))))

(defun epoxide-graph-stop ()
  "Dummy function."
  nil)

(provide 'graph)

;;; Graph.el ends here
