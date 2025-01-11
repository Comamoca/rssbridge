(defpackage #:rssbridge/src/nostr
  (:use #:cl #:rssbridge/src/utils)
  (:export #:make-event #:sign-event #:verify-event #:event-from-json #:event-to-json))

(in-package #:rssbridge/src/nostr)

;; shashtはデフォルトで構造体のkeyを大文字でシリアライズするため、小文字に変換している。
;; また、構造体のkeyにハイフンが使われているため、それをアンダーバーに変換する処理も行っている。
(defparameter shasht:*symbol-name-function* (lambda (sym) 
					      (cl-ppcre:regex-replace-all "-" (string-downcase (symbol-name sym)) "_")))

;; Nostrイベントの構造体
;; idとsigは後で計算する都合上デフォルト値としてnilが指定されている
(defstruct event
  (pubkey nil :type string)              ; 32バイトの小文字16進数公開鍵(Hex)
  (created-at nil :type integer)         ; Unixタイムスタンプ（秒）
  (kind nil :type integer)               ; 0から65535の整数
  (tags nil :type vector)                ; タグのリスト（ネストされた文字列リスト）
  (content nil :type string)             ; 任意の文字列
  (id nil)                               ; 32バイトの小文字16進数SHA256ハッシュ
  (sig nil))                             ; 64バイトの小文字16進数署名

(defun make-event-hash (pubkey created-at kind tags content)
  "イベントのidを算出する"
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

;; イベントをBIP340で署名/検証する関数
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

;; イベントをJSONにシリアライズ/デシリアライズする関数
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

;; ----------------------------------------  SCRATCH ZONE ---------------------------------------- 

;; (let* ((event (make-event :kind 1
;; 			  :content "hello"
;; 			  :tags #()
;; 			  :pubkey "8e1268c5f64362848bc0531e895d7489921ba2667e33e862433a0123ed5ca5a6"
;; 			  :created-at (get-now-unix-time))) 
;;        (seckey "4e8dd4a103bf238ff0f7adb7f911ff8c9d65e869982ad80594858f230dc2908b")) 
;;   ;; (verify-event event)

;;   (shasht:write-json (sign-event event seckey) nil))




;; ;; (unintern 'event (package-name *package*))

;; (defun get-event ()
;;   ""
;;   "{\"content\": \"ぽわ～\",
;;   \"created_at\": 1734731158,
;;   \"id\": \"eb53bf9c97610a51d81b0a0a4ca2b101de33fd95638b6505c6cf8069adf098bd\",
;;   \"kind\": 1,
;;   \"pubkey\": \"4bcc022810a7eff571f1a02024cf3a835c795af109b4ecab11e45062299141d7\",
;;   \"sig\": \"7f083be748cc69f1562819698cfbdd2fdcad7d71faac0245e7f9e5324e6337a8b95e0ea82a8de3e5918597a0996428319385fbfb2812dfc2a439aa0b4be6535a\",
;;   \"tags\": []}")

;; (let* ((message (rssbridge/src/utils:hex->bytes "01f9640fa49905074e5bdbb1e98d94ab521b00f4b2bd7558295f9f436dd26ad5"))
;;        (pub-key-bytes (rssbridge/src/utils:hex->bytes "8e1268c5f64362848bc0531e895d7489921ba2667e33e862433a0123ed5ca5a6"))
;;        (sec-key-bytes (rssbridge/src/utils:hex->bytes "4e8dd4a103bf238ff0f7adb7f911ff8c9d65e869982ad80594858f230dc2908b"))
;;        (pub-key (make-instance 'ironclad:secp256k1-public-key :y pub-key-bytes))
;;        (sec-key (make-instance 'ironclad:secp256k1-private-key :x sec-key-bytes))
;;        ;; (sign (ironclad:sign-message sec-key message))
;;        (sign (sign-message sec-key-bytes message))
;;        )
;;   (rssbridge/src/utils:bytes->hex sign)
;;   )

;; (let* ((event (make-event :kind 1
;; 			  :content "hello"
;; 			  :tags #()
;; 			  :pubkey "8e1268c5f64362848bc0531e895d7489921ba2667e33e862433a0123ed5ca5a6"
;; 			  :created-at (get-now-unix-time)))
;;        (hash (make-event-hash (event-pubkey event)
;; 			      (event-created-at event)
;; 			      (event-kind event)
;; 			      (event-tags event)
;; 			      (event-content event)))
;;        (sig (sign "4e8dd4a103bf238ff0f7adb7f911ff8c9d65e869982ad80594858f230dc2908b" hash)))

;;     (setf (event-id event) hash)
;;     (setf (event-sig event) sig)

;;     )

;; ;; (asdf:load-system :alexandria)
;; ;; (asdf:load-system :ironclad)

;; ;; pubkey: 8e1268c5f64362848bc0531e895d7489921ba2667e33e862433a0123ed5ca5a6
;; ;; seckey: 4e8dd4a103bf238ff0f7adb7f911ff8c9d65e869982ad80594858f230dc2908b
;; ;; id:     e467a852300352c1d8f42c7b678ce1106e18c5fa8fe5f6c3d62976950b83a288
;; ;; sig:    a20edcc50f6f40e6ef7e2a01ea328e1a18a5d6076e57962370154915ec6c4c2c1cf84de0f2e3f0c5c8fcd4a46ec7bd0dc955519da9ee71580a59e4053d77b0b8


;; (progn 
;;   (asdf:load-system :rssbridge/src/bip340)
;;   (asdf:load-system :rssbridge/src/utils)
;;   (asdf:load-system :shasht)
;;   (asdf:load-system :local-time)
;;   (asdf:load-system :babel)
;;   (asdf:load-system :ironclad)
;;   (asdf:load-system :cl-ppcre))

