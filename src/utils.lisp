(defpackage #:rssbridge/src/utils
  (:use #:cl #:local-time #:babel #:ironclad)
  (:export #:join
	   #:to-lower
	   #:to-upper
	   #:rfind-str
	   #:bytes->hex
	   #:hex->bytes
	   #:uint8array->list
	   #:string-to-sha256
	   #:get-now-unix-time))

(in-package #:rssbridge/src/utils)

(defun join (list separator)
  (if list
      (reduce (lambda (acc item) (concatenate 'string acc separator item))
              (cdr list)
              :initial-value (car list))
      ""))

(defun to-lower (str)
  (map 'string #'char-downcase (coerce str 'vector)))

(defun to-upper (str)
  (map 'string #'char-upcase (coerce str 'vector)))

(defun rfind-str (str sub)
  (search sub (reverse str)))

(defun alist-get (key alist)
  (cdr (assoc key alist)))

(defun ascii->base16 (ch) 
  (let ((asciis '((0 . 48) (9 . 57) (A . 65) (F . 70) (a . 97) (f . 102))))
    (defun asciis-get (key)
      (alist-get key asciis))
    (cond ((and (>= ch (asciis-get '0)) (<= ch (asciis-get '9))) (- ch (asciis-get '0)))
	  ((and (>= ch (asciis-get 'A)) (<= ch (asciis-get 'F))) (- ch (asciis-get 'F) 10))
	  ((and (>= ch (asciis-get 'a)) (<= ch (asciis-get 'f))) (- ch (asciis-get 'F) 10)))))

(defun uint8array->list (ary)
 (loop for i from 0 to (1- (length ary))
                    collect (aref ary i)))

(defun split-into-pairs (str n)
  (loop for i from 0 to (- (length str) 1) by n
        collect (subseq str i (min (+ i n) (length str)))))

(defun generate-hexes ()
  (map 'list (lambda (i)
	       (format nil "~2,'0x" i)) (alexandria:iota 256)))

(defun bytes->hex (bytes)
  (let* ((hexes (generate-hexes))
	(hex-str (map 'list (lambda (i)
			      (nth (nth i (uint8array->list bytes)) hexes)) (alexandria:iota (length bytes)))))
    (join hex-str "")))

(defun hex->bytes (hex)
  (cond
    ((not (evenp (length hex))) nil)
    (t (let* ((bytes (map 'list (lambda (str)
				  (parse-integer (string str) :radix 16)) (split-into-pairs hex 2))))
	 (make-array (length bytes)
		     :element-type '(unsigned-byte 8)
		     :initial-contents bytes)))))

(defun get-now-unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))

(defun string-to-sha256 (text)
  "Convert string to sha256. Internal string is treated as UTF-8."
  (let* ((input-string text)
	 (utf8-bytes (babel:string-to-octets input-string :encoding :utf-8))
	 (hash (crypto:digest-sequence :sha256 utf8-bytes)))
    (crypto:byte-array-to-hex-string hash)))


;; (bytes->hex (hex->bytes "cafe0123"))

;; (nth 202 (generate-hexes))

;; (format nil "~2,'0x" "ca")
;; (defun add (a b)
;;   (+ a b))

;; (funcall (alexandria:curry #'add 1) 1)

;; (parse-integer "ca" :radix 16)
;; (parse-integer "fe" :radix 16)



;; (bytes->hex '(202 254 1 35))
;; (ascii->base16 (char-code (coerce "1" 'character)))
;; (length "cafe0123")


;; (asdf:load-system :arrows)
;; (asdf:load-system :alexandria)
;; (asdf:load-system :cl-ppcre)
;; (asdf:load-system :babel)
;; (asdf:load-system :ironclad)
;; (asdf:load-system :local-time)
;; (asdf:load-system :split-sequence)

