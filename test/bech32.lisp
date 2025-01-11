(defpackage #:rssbridge/test/bech32
  (:use #:cl #:fiveam #:rssbridge/src/bech32))
(in-package #:rssbridge/test/bech32)

(def-suite :rssbridge/bech32)
(in-suite :rssbridge/bech32)

;; Datas for testing
(let ((VALID_BECH32 '("A12UEL5L"
		      "a12uel5l"
		      "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
		      "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
		      "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
		      "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
		      "?1ezyfcl"))
      (VALID_BECH32M '("A1LQFN3A"
		       "a1lqfn3a"
		       "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6"
		       "abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx"
		       "11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8"
		       "split1checkupstagehandshakeupstreamerranterredcaperredlc445v"
		       "?1v759aa"))
      (INVALID_BECH32 '(" 1nwldj5" ;; HRP character out of range
			"\x7F" + "1axkwrx" ;; HRP character out of range
			"\x80" + "1eym55h" ;; HRP character out of range
			;; overall max length exceeded
			"an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx"
			"pzry9x0s0muk"	;; No separator character
			"1pzry9x0s0muk"	;; Empty HRP
			"x1b4n0q5v"	;; Invalid data character
			"li1dgmt3"	;; Too short checksum
			"de1lg7wt" + "\xFF" ;; Invalid character in checksum
			"A1G7SGD8" ;; checksum calculated with uppercase form of HRP
			"10a06t8"  ;; empty HRP
			"1qzzfhee" ;; empty HRP
			))
      (INVALID_BECH32M '(" 1xj0phk" ;; HRP character out of range
			 "\x7F" + "1g6xzxy" ;; HRP character out of range
			 "\x80" + "1vctc34" ;; HRP character out of range
			 ;; overall max length exceeded
			 "an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4"
			 "qyrz8wqd2c9m"	 ;; No separator character
			 "1qyrz8wqd2c9m" ;; Empty HRP
			 "y1b0jsk6g"	 ;; Invalid data character
			 "lt1igcx5c0"	 ;; Invalid data character
			 "in1muywd"	 ;; Too short checksum
			 "mm1crxm3i" ;; Invalid character in checksum
			 "au1s5cgom" ;; Invalid character in checksum
			 "M1VUXWEZ" ;; Checksum calculated with uppercase form of HRP
			 "16plkw9"  ;; Empty HRP
			 "1p2gdwpf" ;; Empty HRP
			 ))
      (VALID_ADDRESS '('("BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4" "0014751e76e8199196d454941c45d1b3a323f1433bd6")
		       '("tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
			 "00201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262")
		       '("bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y"
			 "5128751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6")
		       '("BC1SW50QGDZ25J" "6002751e")
		       '("bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs" "5210751e76e8199196d454941c45d1b3a323")
		       '("tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy"
			 "0020000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433")
		       '("tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c"
			 "5120000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433")
		       '("bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
			 "512079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")))
      (INVALID_ADDRESS '(;; Invalid HRP "tc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq5zuyut"
			 ;; Invalid checksum algorithm (bech32 instead of bech32m)
			 "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqh2y7hd"
			 ;; Invalid checksum algorithm (bech32 instead of bech32m)
			 "tb1z0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqglt7rf"
			 ;; Invalid checksum algorithm (bech32 instead of bech32m)
			 "BC1S0XLXVLHEMJA6C4DQV22UAPCTQUPFHLXM9H8Z3K2E72Q4K9HCZ7VQ54WELL"
			 ;; Invalid checksum algorithm (bech32m instead of bech32)
			 "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kemeawh"
			 ;; Invalid checksum algorithm (bech32m instead of bech32)
			 "tb1q0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq24jc47"
			 ;; Invalid character in checksum
			 "bc1p38j9r5y49hruaue7wxjce0updqjuyyx0kh56v8s25huc6995vvpql3jow4"
			 ;; Invalid witness version
			 "BC130XLXVLHEMJA6C4DQV22UAPCTQUPFHLXM9H8Z3K2E72Q4K9HCZ7VQ7ZWS8R"
			 ;; Invalid program length (1 byte)
			 "bc1pw5dgrnzv"
			 ;; Invalid program length (41 bytes)
			 "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7v8n0nx0muaewav253zgeav"
			 ;; Invalid program length for witness version 0 (per BIP141)
			 "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P"
			 ;; Mixed case
			 "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq47Zagq"
			 ;; More than 4 padding bits
			 "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7v07qwwzcrf"
			 ;; Non-zero padding in 8-to-5 conversion
			 "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vpggkg4j"
			 ;; Empty data section
			 "bc1gmk9yu"))

      (INVALID_ADDRESS_ENC '('("BC" 0 20)
			     '("bc" 0 21)
			     '("bc" 17 32)
			     '("bc" 1 1)
			     '("bc" 16 41)))))


;; TODO: Write tests for bech32

;; NOTE: bech32-decode function
;; - bech must consist of strings contained in CHARSET
;; - Ifvalidation is violated, return '(nil nil nil)
