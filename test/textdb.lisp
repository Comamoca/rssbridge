(defpackage #:rssbridge/test/textdb
  (:use #:cl #:fiveam #:rssbridge/src/textdb #:cl-mock))
(in-package #:rssbridge/test/textdb)

(def-suite :rssbridge/textdb)
(in-suite :rssbridge/textdb)

;; Test data imitating textdb
(defparameter textdb-data "")
(defparameter textdb-uuid (frugal-uuid:to-string (frugal-uuid:make-v4)))
(defparameter textdb-url (format nil "https://textdb.dev/api/data/~a" textdb-uuid))


;; Generate a URL that allows you to access the textdb storage location by specifying the id
(test make-url
  (let* ((uuid (frugal-uuid:to-string (frugal-uuid:make-v4)))
	 (actual (make-url uuid)) 
	 (expected (format nil "https://textdb.dev/api/data/~a" uuid)))
	(is (string= (princ-to-string actual) expected))))

;; If you access with get, the saved text data will be returned.
(test textdb-get
  (let ((data "test-get"))
    (with-mocks ()
      (answer (dex:get url) data)
      (is (string= (db-get textdb-url) data)))))

;; If you access with post, the content will be saved and the saved content will be returned.
(test textdb-post
  (let ((data "test-post"))
    (with-mocks ()
      (answer (dex:post url
			:headers '(("content-type" . "text/plain"))
			:content content)
	content)
      (is (string= (db-post textdb-url data) data)))))

;; Saved content can be deleted by posting an empty string.
(test textdb-clear
  (let ((data ""))
    (with-mocks ()
      (answer (dex:post url
			:headers '(("content-type" . "text/plain"))
			:content content)
	content)
      (is (string= (db-clear textdb-url) data)))))


;; TODO: Also test abnormal systems
