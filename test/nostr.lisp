(defpackage #:rssbridge/test/nostr
  (:use #:cl #:fiveam #:rssbridge/src/bip340 #:rssbridge/src/nostr))
(in-package #:rssbridge/test/nostr)

(def-suite :rssbridge/nostr)
(in-suite :rssbridge/nostr)

;; This key pair was generated for testing purposes, so please DO NOT USE IT.
(defparameter nostr-seckey-hex "527f163f04a4fc6997fbf885c4a4d57ef817e2add492cfb004db81c4929271c3")
(defparameter nostr-pubkey-hex "1b29c25cb4f95e524a0e031b9899d63566ebcae7c8f1fc58487cfb187b3a3d2b")
(defparameter nostr-event (rssbridge/src/nostr:make-event :kind 1
							  :content "hello, world"
							  :tags #()
							  :pubkey nostr-pubkey-hex
							  :created-at (rssbridge/src/utils:get-now-unix-time)))


;; Test sign-event and verify-event
(test sign-event
      (let ((actual (verify-event (rssbridge/src/nostr:sign-event nostr-event nostr-seckey-hex)))
	    (expected t))
	(is (eq actual expected))))
