(defpackage #:rssbridge/test/rssbridge
  (:use #:cl #:fiveam #:rssbridge))
(in-package #:rssbridge/test/rssbridge)

(def-suite :rssbridge)
(in-suite :rssbridge)

(test fib-test
  (is (= (fib 10) 55)))
