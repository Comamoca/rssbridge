(defpackage #:rssbridge/src/nostr
  (:use #:cl #:rssbridge/src/utils)
  (:export #:make-event #:sign-event #:verify-event #:event-from-json #:event-to-json))

(in-package #:rssbridge/src/nostr)

;; By default, shasht serializes structure keys in uppercase letters, so they are converted to lowercase letters.
;; Also, since a hyphen is used in the key of the structure, we also convert it to an underscore.
(defparameter shasht:*symbol-name-function* (lambda (sym) 
					      (cl-ppcre:regex-replace-all "-" (string-downcase (symbol-name sym)) "_")))

;; Nostr event structure
;; id and sig are specified as nil as default values ​​for later calculation
(defstruct event
  (pubkey nil :type string)              ;; 32-byte lowercase hexadecimal public key (Hex)
  (created-at nil :type integer)         ;; Unix timestamp (seconds)
  (kind nil :type integer)               ;; integer from 0 to 65535
  (tags nil :type vector)                ;; list of tags (nested string list)
  (content nil :type string)             ;; any string
  (id nil)                               ;; 32-byte lowercase hex SHA256 hash
  (sig nil))                             ;; 64-byte lowercase hex signature

(defun make-event-hash (pubkey created-at kind tags content)
  "Calculate event id"
  (let* ((tags (if (= (length tags) 0)
		   :empty-array
		   tags))
	 (tags-txt (let ((*print-pretty* nil))
		     (shasht:write-json tags nil)))
	 (sig-string (format nil "[0,\"~A\",~A,~A,~A,\"~A\"]"
			     pubkey
			     created-at
			     kind
			     tags-txt
			     content)))
    (rssbridge/src/utils:string-to-sha256 sig-string)))

;; Function to sign/verify events with BIP340
(defun sign (seckey message)
  (let* ((message (hex->bytes message))
       (sec-key-bytes (hex->bytes seckey))
       (sign (rssbridge/src/bip340:sign-message sec-key-bytes message)))

  (bytes->hex sign)))

(defun sign-event (event seckey)
  (let* ((hash (make-event-hash (event-pubkey event)
				(event-created-at event)
				(event-kind event)
				(event-tags event)
				(event-content event)))
	 (sig (sign seckey hash)))

    (setf (event-id event) hash)
    (setf (event-sig event) sig)

    event))

(defun verify-event (event)
  (rssbridge/src/bip340:verify-signature (hex->bytes (event-pubkey event))
					 (hex->bytes (event-id event))
					 (hex->bytes (event-sig event))))

;; Function to serialize/deserialize events to JSON
(defun event-to-json (event)
  (shasht:write-json event nil))

(defun event-from-json (json-text)
  (let* ((ev (shasht:read-json json-text))
	 (id (gethash "id" ev))
	 (pubkey (gethash "pubkey" ev))
	 (created-at (gethash "created_at" ev))
	 (kind (gethash "kind" ev))
	 (tags (gethash "tags" ev))
	 (content (gethash "content" ev))
	 (sig (gethash "sig" ev)))

    (make-event :id id
	      :pubkey pubkey
	      :created-at created-at
	      :kind kind
	      :tags tags
	      :content content
	      :sig sig)))
