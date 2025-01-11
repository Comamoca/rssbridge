(defsystem #:rssbridge
  :class :package-inferred-system
  :version "0.1.0"
  :build-operation program-op
  :entry-point "rssbridge:main"
  :depends-on (#:rssbridge/src/rssbridge
	       #:alexandria
	       #:arrows
	       #:local-time
	       #:shasht
	       #:cl-ppcre
	       #:websocket-driver-client
	       #:ironclad)
  :in-order-to ((test-op (test-op #:rssbridge/test))))

(defsystem #:rssbridge/test
  :depends-on (#:fiveam
	       #:rssbridge/test/rssbridge
	       #:rssbridge/test/bech32
	       #:rssbridge/test/utils
	       #:rssbridge/test/nostr
	       ;; #:rssbridge/src/rssbridge
	       )
  :perform (test-op (o c)
		    (symbol-call :fiveam :run! :rssbridge)
		    (symbol-call :fiveam :run! :rssbridge/utils)
		    (symbol-call :fiveam :run! :rssbridge/nostr)
		    ;; (symbol-call :fiveam :run! :rssbridge/bech32)
		    ))
