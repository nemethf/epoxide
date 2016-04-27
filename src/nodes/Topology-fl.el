;;; Topology-fl.el --- EPOXIDE Topology-fl node definition file

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

;; Node is used for querying topology information from a Floodlight
;; controller.  On its first and only input it takes an enable
;; signal.  Each time this input receives new data a new query is
;; performed.  The node takes one configuration argument that should be
;; the IP address of the host where the controller is running at.  The
;; node has one output where is displays the description of the graph
;; it read from Floodlight.  The format of the graph description is in
;; accordance with what a Graph node is to receive.

;; Queries are preformed by accessing the Floodlight controllers REST
;; API on port 8080. Hosts, switches and links are queried each time
;; the node receives an enable signal using the respective Floodlight
;; REST API calls.  These data are then reformatted and written on the
;; node output.

;;; Code:

(require 'epoxide)
(require 'json)    ; For JSON processing.
(require 'url)     ; For accessing REST APIs.

(eval-when-compile
  (defvar epoxide-node-name)
  (defvar epoxide-node-class)
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-inputs)
  (defvar epoxide-node-outputs)
  (defvar url-http-end-of-headers))

(defun epoxide-topology-fl-input-info ()
  "Provide documentation, value tips and validation for input fields."
  '(((doc-string . "clock"))))

(defun epoxide-topology-fl-config-info ()
  "Provide documentation, value tips and validation for config fields."
  '(((doc-string . "host"))))

(defun epoxide-topology-fl-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "graph description"))))

(defun epoxide-topology-fl-init ()
  "Dummy function."
  nil)

(defun epoxide-topology-fl-exec ()
  "Query Floodlight topology and write it to the first output."
  (let* ((clock (nth 0 epoxide-node-inputs))
	 (enable (epoxide-node-enable-input-active
	  	  (epoxide-node-read-inputs) clock))
	 (host (car epoxide-node-config-list)))
    (when (and clock enable)
      (epoxide-write-node-output
       (epoxide-topology-fl--get-topo host)
       (car epoxide-node-outputs)))))

(defun epoxide-topology-fl--get-topo (host)
  "Query topology from from REST API available at HOST."
  (let* ((nodes (epoxide-topology-fl--get-switches host))
	 (edges (epoxide-topology-fl--get-links host))
	 (hosts-and-links (epoxide-topology-fl--get-hosts-and-links host))
	 (nodes (append nodes (cdr (assoc 'nodes hosts-and-links))))
	 (edges (append edges (cdr (assoc 'edges hosts-and-links)))))
    (concat "nodes: "
	    (mapconcat 'identity nodes ", ")
	    "\nedges: "
	    (mapconcat 'identity edges ", ") "\n")))

(defun epoxide-topology-fl--method-call (host request-method request-data
					    json &rest args)
  "Call a Floodlight REST API method and return the results.

HOST: ip address of the host where the Floodlight controller is running.
REQUEST-METHOD HTTP: request method: e.g. GET, POST or DELETE.
REQUEST-DATA HTTP: request data.  If no request data is needed set to nil,
else set it to a JSON formed message.
JSON: t if /json is required at the end of the link
ARGS: parts of the method to be invoked except for 'wm' 'json' and '/'.
If '/wm/core/switch/00:00:00:00:00:00:01/port/json' is to be called ARGS
should be 'core 'switch 00:00:00:00:00:00:01 'port."
  (setq-local url-show-status nil)
  (let* ((url (concat "http://" host ":8080/wm/"
		      (mapconcat (lambda (x)
				   (condition-case nil
				       (symbol-name x)
				     (error nil))) args "/")
		      (if json
			  "/json"
			"/")))
	 (url-request-method request-method)
	 (url-request-extra-headers)
	 (url-request-data request-data)
	 (result-buffer (condition-case nil
			    (url-retrieve-synchronously url)
			  (error nil)))
	 (data (with-current-buffer result-buffer
		 (when (> (point-max) 1)
		   (goto-char url-http-end-of-headers)
		   (json-read-from-string
		    (buffer-substring-no-properties
		     (point) (point-max)))))))
    (kill-buffer result-buffer)
    data))

(defun epoxide-topology-fl--get-links (host)
  "Query links between switch nodes from the REST API available at HOST.
Return links in a list."
  (let* ((all-data (mapcar 'identity (epoxide-topology-fl--method-call
				      host "GET" nil t 'topology 'links)))
	 links)
    (while all-data
      (let* ((current (pop all-data)))
	(push (concat
	       (cdr (assoc 'src-switch  current)) " "
	       (if (equal (cdr (assoc 'direction current))
			  "bidirectional")
		   "<-> "
		 "-> ")
	       (cdr (assoc 'dst-switch  current)) " (sport="
	       (number-to-string (cdr (assoc 'src-port  current))) "; dport="
	       (number-to-string (cdr (assoc 'dst-port current))) ")")
	      links)))
    links))

(defun epoxide-topology-fl--get-hosts-and-links (host)
  "Query links and hosts from the Floodlight REST API available at HOST.
Return links in a list.

Return an association list with two associations: nodes and
edges.  The cdr of each these associations is a list containing
nodes or edges."
  (let* ((all-data (mapcar 'identity (epoxide-topology-fl--method-call
				      host "GET" nil nil 'device)))
	 hosts links ret)
    (dolist (host all-data)
      (let* ((macs (mapcar 'identity (cdr (assoc 'mac host))))
	     (host-mac (pop macs))
	     (host-ips (mapcar 'identity (cdr (assoc 'ipv4 host)))))
	(while (and host-ips
		    (equal (car host-ips) "0.0.0.0"))
	  (pop host-ips))
	(push
	 (concat host-mac "[Host]"
		 (cond
		  ((and host-ips macs)
		   (concat "(ip="
			   (mapconcat 'identity host-ips ",") "; other_macs="
			   (mapconcat 'identity macs ",") ")"))
		  (host-ips
		   (concat "(ip="
			   (mapconcat 'identity host-ips ",") ")"))
		  (macs
		   (concat "(other_macs="
			   (mapconcat 'identity macs ",") ")"))
		  (t
		   ""))) hosts)
	(dolist (link (mapcar 'identity (cdr (assoc 'attachmentPoint host))))
	  (let ((port (number-to-string (cdr (assoc 'port link))))
		(src (cdr (assoc 'switchDPID link))))
	    (push (concat
		   src " <-> " host-mac " (port=" port ")") links)))))
    (setq ret `(,`(nodes . ,hosts)))
    (push `(edges . ,links) ret)
    ret))

(defun epoxide-topology-fl--get-switches (host)
  "Query switch nodes from the Floodlight REST API available at HOST.
Return them as a list."
  (let* ((all-data (mapcar 'identity
			   (epoxide-topology-fl--method-call
			    host "GET" nil t 'core 'controller 'switches)))
	 switches)
    (while all-data
      (let* ((current (pop all-data))
	     (dpid (cdr (assoc 'switchDPID current)))
	     (ip-address (cdr (assoc 'inetAddress current)))
	     (address (if (> (length ip-address) 0)
			  (mapcar 'char-to-string (string-to-list ip-address))))
	     (ip-address (if (member "." address)
			     (progn ; IPv4.
			       (when (> (length (member ":" address)) 0)
				 (substring ip-address 1
					    (- (length ip-address)
					       (length (member ":" address))))))
			   (substring ip-address 1)))) ; IPv6.
	(push (concat dpid "[Switch](ip=" ip-address ")") switches)))
    (nreverse switches)))

(defun epoxide-topology-fl-stop ()
  "Dummy function.")

(provide 'topology-fl)

;;; Topology-fl.el ends here
