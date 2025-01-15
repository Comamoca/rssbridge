;; Thin wrapper for textdb(https://textdb.dev)

(defpackage #:rssbridge/src/textdb
  (:use #:cl #:quri #:rssbridge/src/utils)
  (:export #:db-get #:db-post #:db-clear #:make-url))
(in-package #:rssbridge/src/textdb)

(defun make-url (id)
  (quri:make-uri :scheme "https"
		 :host "textdb.dev"
		 :path (format nil "/api/data/~a" id)))

(defun db-get (url) 
  (dex:get url))

(defun db-post (url content)
  (dex:post url
	    :headers '(("content-type" . "text/plain"))
	    :content content))

(defun db-clear (url)
  (db-post url ""))
