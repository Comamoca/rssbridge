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
    ;; 上位ビットと下位ビットを1回のループで計算
    (loop for i from 0 below len
          for c = (char-code (aref hrp i))
          do (setf (aref upper-bits i) (ash c -5))
             (setf (aref lower-bits i) (logand c 31)))
    ;; リストに変換して結合
    (append (coerce upper-bits 'list)
            '(0)
            (coerce lower-bits 'list))))

(defun bech32-polymod (values)
  "Bech32のチェックサムを計算する内部関数。"
  (let* ((generator #(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3))
         (chk 1))
    (dolist (value values chk)
      (let ((top (ldb (byte 5 25) chk)))
        (setf chk (logxor (ash (logand chk #x1ffffff) 5) value))
        (dotimes (i 5)
          (when (/= 0 (logand (ash top (- i)) 1))
            (setf chk (logxor chk (aref generator i)))))))))

(defun bech32-create-checksum (hrp data spec)
  "HRPとデータからチェックサム値を計算する関数。
   hrp: Human-Readable Part
   data: データ部分（リスト）
   spec: エンコーディングタイプ（BECH32MまたはBECH32）"
  (let* ((values (append (bech32-hrp-expand hrp) data))
         (const (if (eq spec 'bech32m) BECH32M-CONST 1))
         (polymod (logxor (bech32-polymod (append values '(0 0 0 0 0 0))) const)))
    (loop for i from 0 below 6
          collect (ldb (byte 5 (* 5 (- 5 i))) polymod))))

(defun bech32-verify-checksum (hrp data)
  "Verify a checksum given HRP and converted data characters."
  (let* ((const (bech32-polymod (append (bech32-hrp-expand hrp) data))))
    ;; (when (= const 1)
    ;;   'bech32)
    ;; (when (= const BECH32M-CONST)
    ;;   'bech32m)

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
      ((nil-any (map 'list
		     (lambda (x) (or (< (char-code x) 33) (> (char-code x) 126))) (coerce bech 'list)))
       null-values)

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
		 (list hrp data spec))) 
	     ))))))

;; (bech32-decode "bc1pzry9lf7nxe")
;; (bech32-decode "npub10hqkwugj7p027j250qr9gwcuqpkwxftj0rjjk8y6hlmryu8dwp8s2runf2")


;; (bech32-encode "npub"
;; 	       (coerce (rssbridge/src/utils:hex->bytes  "8e1268c5f64362848bc0531e895d7489921ba2667e33e862433a0123ed5ca5a6") 'list)
;; 	       'bech32)

;; (let ((data '(1 2 3 4 5 31 9 30 19 6 25) ))
;;   (subseq data 0 (- (length data) 6))
;;   )


;; (let* ((bech (to-lower "bc1pzry9lf7nxe"))
;;        (pos (search "1" bech))) 

;;   (let* ((hrp (subseq bech 0 pos))
;; 	 (data (map 'list (lambda (x)
;; 			    (search (string x) CHARSET)) (subseq bech (+ pos 1))))
;; 	 (spec (bech32-verify-checksum hrp data))) 

;;     ;; data
;;     ;; (bech32-verify-checksum hrp '(1 2 3 4 5 31 9 30 19 6 25))
;;     hrp
;;     ))

;; (let ((bech "pzry9lf7nxe"))
;;   (map 'list (lambda (x)
;; 	       (search (string x) CHARSET)) bech))

;; (map 'list (lambda (x)
;; 	     ) "pzry9lf7nxe")

	  ;; (when (or (< pos 1) (> (+ pos 7) (length bech)) (> (length bech) 90))
	  ;;     '(nil nil nil))
	  ;; (when (not (every #'identity (map 'list (lambda (x)
	  ;; 					    (null (search (string x) CHARSET))) (coerce (subseq bech (+ pos 1)) 'list))))
	  ;;   '(nil nil nil))

  ;; (if (nil-any (map 'list
  ;; 		    (lambda (x) (or (< (char-code x) 33) (> (char-code x) 126))) (coerce bech 'list)))
  ;;     '(nil nil nil)
  ;;     (progn
  ;; 	(let* ((bech (to-lower bech))
  ;; 	       (pos (rfind-str "1" bech)))
  ;; 	  (when (or (< pos 1) (> (+ pos 7) (length bech)) (> (length bech) 90))
  ;; 	      '(nil nil nil))
  ;; 	  (when (not (every #'identity (map 'list (lambda (x)
  ;; 						    (null (search (string x) CHARSET))) (coerce (subseq bech (+ pos 1)) 'list))))
  ;; 	    '(nil nil nil))
  ;; 	  ))
  ;;     ))


;; (every #'identity '(nil nil))

;; (bech32-decode "npub1g8tj7zgure2etccavmp7sdj5xn75en4h4tkzezx48zcmf23dqens6exr02")

;; ---------------------- ;;

;; (map 'string #'char-upcase "hello")
;; (map 'list (lambda (char) (char-upcase char)) (coerce "hello" 'list))

;;   (funcall (lambda (x)
;;     (or (< (char-code x) 33) (> (char-code x) 126))) (char "a" 0))

;; (some (lambda (val) (not (null val))) '(nil nil nil))

;; (not (null 1))

;; (append (bech32-hrp-expand "bc") '(1 2 3 4 5))

;; (bech32-polymod (append (bech32-hrp-expand "bc") '(1 2 3 4 5)))

;; (bech32-verify-checksum "bc" '(1 2 3 4 5))

;; (bech32-encode "npub" '(8 7 11 18 30 2 8 28 3 25 10 25 11 24 24 29 12 27 1 30 16 13 18 20 6 19 30 20 25 19 21 23 21 11 22 2 25 2 6 21 7 2 24 27 9 10 17 13 0 25 19 16) 'bech32)

;; (bech32-create-checksum "bc" '(1 2 3 4 5) 'bech32m)
;; (bech32-encode "bc" '(1 2 3 4 5) 'bech32) ; => "bc1pzry9lf7nxe"
;; (coerce (loop for d in '(1 2 3) collect (elt CHARSET d)) 'string)


;; (defun bech32-create-checksum (hrp data spec)
;;   (let* ((values (append (bech32-hrp-expand hrp) data))
;; 	 (const (if (eq spec 'bech32m) BECH32M-CONST 1))
;; 	 (polymod (logxor (bech32-polymod (append values '(0 0 0 0 0 0))) const)))

;;     (loop for i from 0 below 6
;; 	  collect (logand (ash polymod (- (* 5 (- 5 i)))) 31))))
