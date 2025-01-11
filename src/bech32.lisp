(defpackage #:rssbridge/src/bech32
  (:use #:cl #:rssbridge/src/utils)
  (:export #:bech32-encode #:bech32-decode))
(in-package #:rssbridge/src/bech32)

(defparameter CHARSET "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
(defparameter BECH32M-CONST #x2bc830a3)

(defun bech32-hrp-expand (hrp)
  "Expand the HRP into values for checksum computation."
  (let* ((len (length hrp))
         (upper-bits (make-array len :element-type 'fixnum))
         (lower-bits (make-array len :element-type 'fixnum)))
    ;; Calculate upper and lower bits in one loop
    (loop for i from 0 below len
          for c = (char-code (aref hrp i))
          do (setf (aref upper-bits i) (ash c -5))
             (setf (aref lower-bits i) (logand c 31)))
    ;; Convert to list and join
    (append (coerce upper-bits 'list)
            '(0)
            (coerce lower-bits 'list))))

(defun bech32-polymod (values)
  "Internal function to calculate Bech32 checksum."
  (let* ((generator #(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3))
         (chk 1))
    (dolist (value values chk)
      (let ((top (ldb (byte 5 25) chk)))
        (setf chk (logxor (ash (logand chk #x1ffffff) 5) value))
        (dotimes (i 5)
          (when (/= 0 (logand (ash top (- i)) 1))
            (setf chk (logxor chk (aref generator i)))))))))

(defun bech32-create-checksum (hrp data spec)
  "Function to calculate checksum value from HRP and data.
   hrp: Human-Readable Part data: Data part (list)
   spec: Encoding type (BECH32M or BECH32)"
  (let* ((values (append (bech32-hrp-expand hrp) data))
         (const (if (eq spec 'bech32m) BECH32M-CONST 1))
         (polymod (logxor (bech32-polymod (append values '(0 0 0 0 0 0))) const)))
    (loop for i from 0 below 6
          collect (ldb (byte 5 (* 5 (- 5 i))) polymod))))

(defun bech32-verify-checksum (hrp data)
  "Verify a checksum given HRP and converted data characters."
  (let* ((const (bech32-polymod (append (bech32-hrp-expand hrp) data))))
    (cond
      ((= const 1) 'bech32)
      ((= const BECH32M-CONST) 'bech32m))))

(defun bech32-encode (hrp data spec)
  (let* ((combined (append data (bech32-create-checksum hrp data spec)))
	 (head (uiop:strcat hrp "1"))
	 (tail (coerce (loop for d in combined collect (elt CHARSET d)) 'string))) 
    (uiop:strcat head tail)))

(defun bech32-decode (bech)
  "Validate a Bech32/Bech32m string, and determine HRP and data."
  (defun nil-any (sequence)
    (some (lambda (val) (not (null val))) sequence))

  (let* ((bech (to-lower bech))
	 (pos (search "1" bech))
	 (null-values '(nil nil nil)))

    (cond
      ((nil-any (map 'list (lambda (x)
			     (or (< (char-code x) 33) (> (char-code x) 126)))
		     (coerce bech 'list))) null-values)

      ((or (< pos 1) (> (+ pos 7) (length bech)) (> (length bech) 90))
       null-values)

      ((every #'identity (map 'list (lambda (x)
				      (null (search (string x) CHARSET))) (coerce (subseq bech (+ pos 1)) 'list)))
       null-values)

      (t (progn
	   (let* ((hrp (subseq bech 0 pos))
		  (data (map 'list (lambda (x)
				     (search (string x) CHARSET)) (subseq bech (+ pos 1))))
		  (spec (bech32-verify-checksum hrp data)))

	     (let ((data (subseq data 0 (- (length data) 6))))
	       (if (null spec)
		   null-values
		   (list hrp data spec)))))))))
