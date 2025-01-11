(defpackage #:rssbridge/test/utils
  (:use #:cl #:fiveam #:rssbridge/src/utils))
(in-package #:rssbridge/test/utils)

(def-suite :rssbridge/utils)
(in-suite :rssbridge/utils)

(defparameter
    testdata-array-1
  (make-array 4 :initial-contents '(202 254 1 35)))

(defparameter testdata-string-1 "cafe0123")

(defparameter
    testdata-array-2
  (make-array 32 :initial-contents '(54 104 6 71 118 120 107 128 19 191 230 201 11 235 150 70 23 89 3 103 94 230 129 177 45 200 193 49 125 28 106 200)))

(defparameter testdata-string-2 "3668064776786b8013bfe6c90beb9646175903675ee681b12dc8c1317d1c6ac8")

(test utils-join
  (let ((actual (join '("hello" "world") ","))
	(expected "hello,world"))
    (is (string= actual expected))))

(test utils-to-lower
  (let ((actual (to-lower "HeLlO"))
	(expected "hello"))
    (is (string= actual expected))))

(test utils-to-upper
  (let ((actual (to-upper "HeLlO"))
	(expected "HELLO"))
    (is (string= actual expected))))

(test bytes->hex-1
      (let ((actual (to-lower (bytes->hex testdata-array-1)))
	    (expected testdata-string-1))
	(is (string= actual expected))))

(test bytes->hex-2
      (let ((actual (to-lower (bytes->hex testdata-array-2)))
	    (expected testdata-string-2))
	(is (string= actual expected))))

;; cafe0123
(test hex->bytes-1
      (let ((actual (hex->bytes testdata-string-1))
	    (expected testdata-array-1))
	(is (equalp actual expected))))

(test hex->bytes-2
      (let ((actual (hex->bytes testdata-string-2))
	    (expected testdata-array-2))
	(is (equalp actual expected))))
