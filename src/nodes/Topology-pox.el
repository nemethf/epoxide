;;; Topology-pox.el --- EPOXIDE Topology-pox node definition file

;; Copyright (C) 2015      Felician Németh
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

;; Node is used for querying topology information from a POX
;; controller.  On its first and only input it takes an enable
;; signal.  Each time this input receives new data a new query is
;; performed.  The node takes one configuration argument that should be
;; the IP address of the host where the controller is running at.  The
;; node has one output where is displays the description of the graph
;; it read from POX.  The format of the graph description is in
;; accordance with what a Graph node is to receive.

;; Query is preformed in the following way.  A process is started that
;; listens on port 8282 for topology information from POX.  (POX should
;; be started with modules web.webcore, openflow.webservice,
;; openflow.discovery, epoxide_topo, epoxide_host_tracker in order to
;; get proper topology information.  epoxide_host_tracker and
;; epoxide_topo are more stable versions of the original POX
;; modules.  The topo module opens sends topology information to port
;; 8282 as a stream.) When new information is received the nodes inner
;; model is refreshed.  Each time the node receives an enable signal
;; all topology information contained currently in the node's model is
;; relayed to its output.

;;; Code:

(require 'epoxide)
(require 'json)

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar epoxide-topology-topo)
  (defvar epoxide-topology-pox-nodes)
  (defvar epoxide-topology-pox-edges)
  (defvar epoxide-topology-pox-process-buffer-name)
  (defvar epoxide-topology-pox-process))

(defun epoxide-topology-pox-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-topology-pox-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host"))))

(defun epoxide-topology-pox-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "graph description"))))

(defun epoxide-topology-pox-init ()
  "Perform node initialization."
  (set (make-local-variable 'epoxide-topology-pox-process-buffer-name) nil)
  (set (make-local-variable 'epoxide-topology-pox-process) nil))

(defun epoxide-topology-pox-exec ()
  "Query POX topology and write it to the first output."
  (let* ((clock (nth 0 epoxide-node-inputs))
	 (enable (epoxide-node-enable-input-active
	  	  (epoxide-node-read-inputs) clock))
	 (host (car epoxide-node-config-list)))
    (when (and clock enable)
      (epoxide-write-node-output
       (epoxide-topology-pox--get-topo host)
       (car epoxide-node-outputs)))))

(defun epoxide-topology-pox--convert (key val)
  "Convert a simple topo representation to an object format.
Used buffer local variables: `epoxide-topology-pox-nodes',
`epoxide-topology-pox-edges'.  Argument KEY is ignored.  VAL is
an element in simple representation returned by POX."
  (cond
   ((equal (cdr (assoc 'kind val)) "host")
    (let* ((str-mac (cdr (assoc 'label val)))
           (str-ip (cdr (assoc 'ip val)))
	   (host (concat str-mac "[Host]"
			 (if str-ip
			     (concat "(ip=" str-ip ")")
			   ""))))
      (when (member str-mac epoxide-topology-pox-nodes)
	  (setq epoxide-topology-pox-nodes (delete str-mac epoxide-topology-pox-nodes)))
      (push host epoxide-topology-pox-nodes)))
   ((equal (cdr (assoc 'kind val)) "switch")
    (let* ((dpid (cdr (assoc 'label val))))
      (unless (member dpid epoxide-topology-pox-nodes)
	(push (concat dpid "[Switch]") epoxide-topology-pox-nodes))))
   ((assoc 'directed val) ; Link.
    (let* ((src (cdr (assoc 'source val)))
           (dst (cdr (assoc 'target val)))
           (sport (cdr (assoc 'source_port val)))
           (dport (cdr (assoc 'target_port val)))
	   (link (concat src " <-> " dst
			 (cond
			  ((and sport dport)
			   (concat " (sport=" (number-to-string sport)
				   "; dport=" (number-to-string dport) ")"))
			  (sport
			   (concat " (sport=" (number-to-string sport)")"))
			  (dport
			   (concat " (dport=" (number-to-string dport) ")"))
			  (t "")))))
      ;; TODO: using regexp it might be possible to decide whether src
      ;; and dst are hosts or switches.
      (unless (or (member src epoxide-topology-pox-nodes)
		  (member (concat src "[Host]")
			  epoxide-topology-pox-nodes)
		  (member (concat src "[Switch]")
			  epoxide-topology-pox-nodes))
      	(push src epoxide-topology-pox-nodes))
      (unless (or (member dst epoxide-topology-pox-nodes)
		  (member (concat dst "[Host]")
		  	  epoxide-topology-pox-nodes)
		  (member (concat dst "[Switch]")
		  	  epoxide-topology-pox-nodes))
      	(push dst epoxide-topology-pox-nodes))
      (push link epoxide-topology-pox-edges)))
   (t
    (epoxide-log "ignoring object (%s-%s)" key val))))

(defun epoxide-topology-pox--filter (proc string)
  "Filter new topology elements.
Called when POX sends something.  PROC: the process of which
output the filter is applied to.  STRING: received data from
PROC."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (save-excursion
      (goto-char (process-mark proc))
      (while (not (eobp))
        (if (equal (char-after) ?{)
            (if (equal (char-before (- (point-at-eol) 1)) ?})
                (let ((obj (json-read)))
                  (cond
                   ((member (caar obj) '(an ae))
                    (dolist (elem (cdar obj))
                      (puthash (car elem) (cdr elem) epoxide-topology-topo)))
                   ((member (caar obj) '(dn de))
                    (dolist (elem (cdar obj))
                      (if (equal (car elem) 'filter)
                          (clrhash epoxide-topology-topo) ; Delete everything.
                        (remhash (car elem) epoxide-topology-topo)
                        (when (eq (caar obj) 'dn)
                          ;; Delete corresponding edges as well.
                          (let ((id (symbol-name (car elem)))
                                to-delete)
                            (maphash
                             (lambda (key val)
                               (if (member id
                                           (list (cdr (assoc 'source val))
                                                 (cdr (assoc 'target val))))
                                   (setq to-delete (cons key to-delete))))
                             epoxide-topology-topo)
                            (dolist (key to-delete)
                              (remhash key epoxide-topology-topo)))))))
                   (t
                    (epoxide-log "unknown object type (%s)" (caar obj))))
                  ;; JSON object is proccessed, let's proceed.
                  (set-marker (process-mark proc) (point)))
              ;; Incomplete input, wait for more by not moving the process mark.
              (goto-char (point-max)))
          (forward-line))))))

(defun epoxide-topology-pox--get-topo (ip-address)
  "Query POX for topology info.

Open port 8282 at IP-ADDRESS as a stream, then read the
connections between the network elements using a POX module named
'epoxide_topo'.

Return a string describing a graph to draw.  The format of this
string matches that of requested by a Graph node."
  (let* ((process-name (concat
			(epoxide-tsg-create-node-buffer-name
			 epoxide-node-name epoxide-node-class)
			"-proc"))
         (service 8282)
         (process (get-process process-name)))
    (setq-local epoxide-topology-pox-process process)
    (setq-local epoxide-topology-pox-process-buffer-name (concat process-name "-buf"))
    (unless process
      (setq process (make-network-process
		     :name process-name
		     :buffer epoxide-topology-pox-process-buffer-name
		     :host ip-address
		     :service service
		     :noquery t
		     :filter 'epoxide-topology-pox--filter))
      ;; (switch-to-buffer-other-window buffer-name) ; For debugging.
      (with-current-buffer epoxide-topology-pox-process-buffer-name
        (erase-buffer)
        (make-local-variable 'epoxide-topology-topo)
	(make-local-variable 'epoxide-topology-pox-nodes)
	(make-local-variable 'epoxide-topology-pox-edges)
        (setq epoxide-topology-topo (make-hash-table :test 'equal))
      ;; Send initial request.
      (process-send-string process "\n\n")
      (sit-for 0.1)))
    (with-current-buffer epoxide-topology-pox-process-buffer-name
      (let (epoxide-topology-pox-nodes epoxide-topology-pox-edges)
        (maphash 'epoxide-topology-pox--convert epoxide-topology-topo)
	(concat "nodes: "
		(mapconcat 'identity (epoxide-topology-pox--delete-node-dups epoxide-topology-pox-nodes) ", ")
		"\nedges: "
		(mapconcat 'identity epoxide-topology-pox-edges ", ") "\n")))))

(defun epoxide-topology-pox--delete-node-dups (nodes)
  "Remove duplicates from NODES."
  (let ((nodes (delete-dups nodes))
	tmp ret)
    (dolist (n nodes ret)
      (unless (> (length (dolist (node nodes tmp)
			   (let ((name (if (string-match "[[:alnum:]-]+\\[.*\\](.*)" node)
					   (substring node 0 (string-match "\\[" node))
					 node)))
			     (when (equal n name)
			       (push node tmp))))) 1)
	(push n ret))
      (setq tmp))))

(defun epoxide-topology-pox-stop ()
  "Close query process and its buffer."
  (epoxide-stop-process epoxide-topology-pox-process)
  (epoxide-kill-buffer epoxide-topology-pox-process-buffer-name t))

(provide 'topology-pox)

;;; Topology-pox.el ends here
