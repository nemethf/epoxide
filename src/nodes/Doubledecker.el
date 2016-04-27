;;; Doubledecker.el --- EPOXIDE Doubledecker node definition file

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

;; This node is able to connect to a DoubleDecker broker, receive and
;; send messages to a partner or to a topic.

;; The node has no inputs, when it is instantiated it creates a
;; subprocess that continuously updates its only output buffer.  The
;; node has one mandatory and four optional configuration
;; arguments.  The mandatory first argument specifies the host where
;; the DoubleDecker client should run at.  The second argument
;; specifies the topic where the node should subscribe in the form of
;; <topic>/<scope> (see the reference DoubleDecker client for
;; available options of scope). In this implementation when more than
;; one topic is listed, they should appear as a semi-colon (;)
;; separated list.  The following arguments have default values, that
;; can be seen and set in the `epoxide' customization group's
;; `doubledecker' subgroup.  The third argument is defines the location
;; of the key file, the fourth the tenant and the fifth specifies the
;; name that the node should have in a DoubleDecker connection.  When
;; these arguments have non-nil values, those overrule the default
;; ones.

;; At startup the node calls a special DoubleDecker client that is
;; able to display and receive messages in JSON format.  The location
;; of this python file needs to be given to the node by setting the
;; `epoxide-doubledecker' customization group's
;; `epoxide-doubledecker-jsonclient' variable.

;; The only output of the node relays every message as received from
;; the jsonclient.  These messages are usually JSON formatted but in
;; case of startup they are unformatted strings.

;;; Code:

(require 'epoxide)

(eval-when-compile
  (defvar epoxide-node-config-list)
  (defvar epoxide-node-outputs))

(defgroup epoxide-doubledecker nil
  "Doubledecker node defaults."
  :prefix 'epoxide
  :group 'epoxide)

(defcustom epoxide-doubledecker-jsonclient
  "~/DoubleDecker/python/bin/jsonclient.py"
  "DoubleDecker jsonclient.py file's location."
  :type 'file
  :group 'epoxide-doubledecker)

(defcustom epoxide-doubledecker-key "/etc/doubledecker/a-keys.json"
  "DoubleDecker default location of key file."
  :type 'file
  :group 'epoxide-doubledecker)

(defcustom epoxide-doubledecker-tenant "a"
  "Default DoubleDecker tenant location."
  :type 'file
  :group 'epoxide-doubledecker)

(defvar epoxide-process "Handle to DoubleDecker backgroud process.")

(defun epoxide-doubledecker-input-info ()
  "Provide documentation, value tips and validation for input fields."
  nil)

(defun epoxide-doubledecker-config-info ()
  "Provide documentation, value tips and validation for config fields."
  `(((doc-string . ,(concat "Source address\nEither IP address/hostname "
                            "or a tramp directory like '/ssh:remote.host:/' "
                            "or '/mininet:h1:/' see "
                            "(info \"(Tramp)Remote processes\"). "
                            "Empty means localhost."))
     (value-helper . epoxide-list-ip-addresses)
     (value-validator . epoxide-ip-address-or-name-p))
    ((doc-string . "topics (`;' separated)"))
    ((doc-string . "location of key file"))
    ((doc-string . "tenant"))
    ((doc-string . "name\ndefault: epoxide"))))

(defun epoxide-doubledecker-output-info ()
  "Provide documentation, value tips and validation for output fields."
  '(((doc-string . "DoubleDecker messages in JSON"))))

(defun epoxide-doubledecker-init ()
  "Set up the node's buffer.

Insert basic node information and key combination as well as a
widget-based form that supports sending messages/notifications
via the DoubleDecker broker."
  (set (make-local-variable 'epoxide-process) nil)
  (let* ((inhibit-read-only t)
	 (field-length 50)
	 (separator-length (+ 17 field-length))
	 (name (or (nth 4 epoxide-node-config-list)
		   "epoxide"))
	 (destination "")
	 (topic "")
	 (msg "")
	 values)
    (erase-buffer)
    (remove-overlays)
    (epoxide-insert-node-basics)
    (widget-insert "\n\n" (make-string separator-length ?=) "\n\n"
		   "Connection status: "
		   (propertize "disconnected" 'face 'isearch-fail)
		   "\n\nSend message/notification:\n\n")
    (widget-create 'editable-field
            :size field-length
            :format "Source:\t\t %v" name)
    (widget-insert "\n\n")
    (widget-create 'editable-field
            :size field-length
            :format "Destination:\t %v" destination)
    (widget-insert "\n\t\t Either this or `Topic' should be filled in.\n\t\t "
		   "When both are filled in, destination takes\n\t\t "
		   "precedence and a notification is sent.\n\n")
    (widget-create 'editable-field
            :size field-length
            :format "Topic:\t\t %v" topic)
    (widget-insert "\n\t\t Either this or `Destination' should be filled in."
		   "\n\t\t Fill in only this to publish to a topic.\n\n")
    (widget-create 'editable-field
            :size field-length
            :format "Message:\t %v" msg)
    (widget-insert "\n\n\n")
    (insert-button "Send"
		   'action (lambda (&rest ignore)
			     (save-excursion
			       (let (data)
				 ;; Collect modified widget values.
				 (goto-char (point-min))
				 (when (search-forward "Source:" nil t)
				   (dotimes (i 4)
				     (widget-forward 1)
				     (push (widget-value (widget-at)) data))
				   (setq data (nreverse data))
				   (epoxide-doubledecker-send-message
				    (nth 0 data) (nth 1 data)
				    (nth 2 data) (nth 3 data))))))
		   'follow-link t)
    (widget-insert "\n\n")
    (widget-insert (make-string separator-length ?=) "\n\n\n")
    (epoxide-insert-key-bindings "epoxide-mode-map")
    (goto-char (point-min))
    (widget-setup))
  ;; Add function that watches whether the (dis)connection happened.
  (with-current-buffer (car epoxide-node-outputs)
    (add-hook 'after-change-functions
	      'epoxide-doubledecker-check-connection t t)))

(defun epoxide-doubledecker-check-connection (begin end len)
  "Check incoming messages for indication of (dis)connection.

When a proper JSON message for (dis)connection arrives modify the
connection status descriptor in the node's buffer.

BEGIN the beginning of the changed region.
END the end of the changed region.
LEN the length of the changes."
  (let ((msg (condition-case nil
  		 (json-read-from-string
		  (buffer-substring-no-properties begin end))
  	       (error nil)))
	;; Prevent kill-ring modification when changing status.
	kill-ring)
    (with-current-buffer epoxide-src-node
      (save-excursion
  	(goto-char (point-min))
  	(when (search-forward "Connection status: " nil t)
  	  (pcase (cdr (assoc 'type msg))
  	    ("reg"
	     (widget-kill-line)
  	     (widget-insert (propertize "connected" 'face 'success)))
  	    ("discon"
	     (widget-kill-line)
  	     (widget-insert
	      (propertize "disconnected" 'face 'isearch-fail)))))))))

(defun epoxide-doubledecker-start ()
  "Start the DoubleDecker subprocess."
  (let* ((config (epoxide-node-get-config))
	 (source (pop config))
	 (jsonclient epoxide-doubledecker-jsonclient)
	 (topic `(,(replace-regexp-in-string ";" "," (pop config))))
	 (key (or (epoxide-string-or-nil (pop config))
		  epoxide-doubledecker-key))
	 (tenant (or (epoxide-string-or-nil (pop config))
		     epoxide-doubledecker-tenant))
	 (name (pop config))
	 (name (or name
		   "epoxide"))
         (output-buffer (nth 0 epoxide-node-outputs))
         (command "python3")
         (args `(,output-buffer ,output-buffer ,command ,jsonclient "-k" ,key))
	 (args (if topic
		   (append args '("-t") topic)
		 args))
	 (args (append args `(,name ,tenant))))
    (setq epoxide-process (apply 'epoxide-start-process source args))))

(defun epoxide-doubledecker-exec ()
  "Wrapper for external `doubledecker' command."
  (if (and epoxide-process
           (process-live-p epoxide-process))
      ;; The framework called epoxide-doubledecker-exec, because there's a
      ;; change in one of its input buffers, or because of something
      ;; else, but the doubledecker process is still running, so don't restart
      ;; the process.
      nil
    (epoxide-doubledecker-start)))

(defun epoxide-doubledecker-send-message (src dst topic data)
  "Send a message via the DoubleDecker broker.

SRC specifies the source, DST the destionation, TOPIC the topic
and DATA contains the message.  When DST's length is greater than
0, a DoubleDecker notification is sent, when it is 0 but TOPIC's
length is greater than 0, DATA is published to the given topic."
  (let* ((msg `((src . ,src) (data . ,data)))
	 (msg (if (> (length dst) 0)
		 (append msg `((type . "notify") (dst . ,dst)))
	        (append msg `((type . "pub") (topic . ,topic)))))
	 (msg (concat (json-encode msg) "\n")))
    (process-send-string epoxide-process msg)))

(defun epoxide-doubledecker-stop ()
  "Kill connection buffer."
  (when (boundp 'epoxide-process)
    (epoxide-stop-process epoxide-process)))

(provide 'doubledecker)
;;; Doubledecker.el ends here
