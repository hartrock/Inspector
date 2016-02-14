;; WebSockets
;;

(MAIN:load-module "WS_WS.em_bits.lsp")

(context 'WS_WS)

;; util

(define (_reverse-bytes_4 val, res) ; slower than next variant
  (|
   (<< (& val 0x000000FF) 24)
   (<< (& val 0x0000FF00)  8)
   (>> (& val 0x00FF0000)  8)
   (>> (& val 0xFF000000) 24)))
(define (reverse-bytes_4 val) ; reverse by ..
  ((unpack ">lu" (pack "<lu" val)) 0)) ; .. in/out with different byte order

(set 'littleEndianArchitecture (= ((pack "d" 1) 0) "\001")
     'littleEndian_32 (if littleEndianArchitecture
                       (lambda (val) val)
                       reverse-bytes_4))

;; conversion
(define (bstring->bytes bstring , (len (length bstring)))
  (unpack (dup "b" len) bstring))
(define (bytes->bstring bytes , (len (length bytes)))
  (pack (dup "b" len) bytes))
;; printing
(define (bytes->hexrep-string bytes)
  (join (map (curry format "0x%02X") bytes) " "))
(define (bstring->hexrep-string bstr)
  (bytes->hexrep-string (bstring->bytes bstr)))

;;
(define (bytes_bigEndian->host bytes)
  (if littleEndianArchitecture
      (reverse bytes)
      bytes))
(define (bytes_littleEndian->host bytes)
  (if littleEndianArchitecture
      bytes
      (reverse bytes)))

(if littleEndianArchitecture
    (set 'bytes_bigEndian->host (fn (bytes) (reverse bytes))
         'bytes_littleEndian->host (fn (bytes) bytes))
    (set 'bytes_bigEndian->host (fn (bytes) bytes)
         'bytes_littleEndian->host (fn (bytes) (reverse bytes))))

;; bytes list/arr -> int32
(define (bytes_littleEndian->int32 bytes)
  (| (<< (bytes 3) 24)
     (<< (bytes 2) 16)
     (<< (bytes 1)  8)
     (bytes 0)))
(define (bytes_bigEndian->int32 bytes)
  (| (<< (bytes 0) 24)
     (<< (bytes 1) 16)
     (<< (bytes 2)  8)
     (bytes 3)))
(set 'bytes_net->int32 bytes_bigEndian->int32
     ;; for getting host mask corresponding to net bytes mask, if written
     'bytes_host->int32 (if littleEndianArchitecture
                            bytes_littleEndian->int32
                            bytes_bigEndian->int32))


;;
;; bits'n'masks after table http://tools.ietf.org/html/rfc6455#section-5.2

(constant 'b_all 0xFF)
;; bits in first byte after numbering in table
(constant 'b_0 128 ; most significant
          'b_1 64
          'b_2 32
          'b_3 16
          'b_4  8
          'b_5  4
          'b_6  2
          'b_7  1)
;; 1. byte
(constant 'mask_FIN b_0
          'mask_RSV (| b_1 (| b_2 b_3))
          'mask_RSV1 b_1
          'mask_RSV2 b_2
          'mask_RSV3 b_3
          'mask_opcode (| b_4 (| b_5 (| b_6 b_7))))
;; 2. byte
(constant 'mask_MASK b_0 ; b_8 in table
          'mask_len (^ b_all b_0)) ; unset b_0; Payload len (7)

;; opt:
;; 3. + 4. byte -> Extended payload length 16 bits
;; 3. - 10. byte ->  Extended payload length 64 bits
;; thereafter (3., 5. or 11. byte) -> masking key (32), if MASK bit set
;; thereafter payload data

;; 1. byte
(define (FIN? b)
  ;(dbg:expr MAIN:bset? WS_WS:bset?)
  ;(dbg:expr (symbols MAIN:WS_WS))
  (bset? mask_FIN b))
(define (opcode b)
  (& mask_opcode b))
;; opcodes
(constant 'b_FIN b_0
          'oc_text 0x1
          'oc_binary 0x2
          'oc_ping 0x9
          'oc_pong 0xA
          'oc_binary_str "\002")
(define (oc_continuation? oc)
  (= oc 0x0))
(define (oc_text? oc)
  (= oc oc_text))
(define (oc_binary? oc)
  (= oc 0x2))
(define (oc_reservedNonControl? oc)
  (and (<= 0x3 oc) (<= oc 0x7)))
(define (oc_connectionClose? oc)
  (= oc 0x8))
(define (oc_ping? oc)
  (= oc 0x9))
(define (oc_pong? oc)
  (= oc 0xA))
(define (oc_reservedControl? oc)
  (and (<= 0xB oc) (<= oc 0xF)))
(define (oc_control? oc)
  (and (<= 0x8 oc) (<= oc 0xF)))

;; http://tools.ietf.org/html/rfc6455#section-5.2
[text]
*  %x0 denotes a continuation frame
*  %x1 denotes a text frame
*  %x2 denotes a binary frame
*  %x3-7 are reserved for further non-control frames
*  %x8 denotes a connection close
*  %x9 denotes a ping
*  %xA denotes a pong
*  %xB-F are reserved for further control frames
[/text]

;; 2. byte
(constant 'b_MASK b_0)
(define (MASK? b)
  (bset? mask_MASK b))
(define (len b) ; 0 <= len <= 127
  (& mask_len b))
(define (extended? len) ; 126, 127
  (>= len 126))
(define (extended_16? len)
  (= len 126))
(define (extended_64? len)
  (= len 127))

(define (byte_1 b_FIN bits_oc)
  (| b_FIN bits_oc))
(define (lenForPayloadLen len)
  (let (lenWithoutMask
        (cond
         ((= len 0)
          2)
         ((<= len 125)
          (+ 2 len))
         ((== len 126)
          (+ 4 len)) ; 2 bytes + 16 bits
         ("else"
          (+ 10 len)))) ; 2 bytes + 64 bits
    (if maskFlag
        (+ lenWithoutMask 4)
        lenWithoutMask)))

(define (uint16->net-bstring u16)
  (pack ">u" u16))
(define (uint32->net-bstring u32)
  (pack ">lu" u32))
(define (uint64->net-bstring u64)
  (pack ">Lu" u64))

;; assumes that mask_32 corresponds to host byte order (read/written in it):
;; bytes_host->int32 is suited for getting host mask corresponding to given net
;; (big endian) bytes (pack/unpack result of bstring)
(define (mask dataStr
              mask_32 ; may already be (net) big endian read-in little endian
              , (mask_littleEndian (littleEndian_32 mask_32))
                (len (length dataStr))
                (num_uint32 (/ len 4))
                (m (% len 4))
                ix uint restStr restBytes
                (res ""))
  ;;(println "dataStr: " dataStr ", len: " len ", (/ len 4): " (/ len 4))
  ;; it doesn't matter for xor, if *both* mask and bytes will be
  ;;   unpacked -> xor'ed -> packed
  ;; little or big endian
  (set 'ix 0)
  (while (< ix num_uint32) ; while better than for (far from C semantics) here
    ;;(dbg:expr ix (- (/ len 4) 1))
    ;; slicing works, indexing not (e.g. for \000 inside str)
    (set 'uint32 ((unpack "lu" ((* ix 4) 4 dataStr)) 0))
    (extend res (pack "lu" (^ uint32 mask_32)))
    (++ ix))
  (when (> m 0)
    ;; big endian treatment of restBytes by taking net mask bytes little endian
    ;; one by one from the right
    (set 'restStr ((- len m) m dataStr)
         'restBytes (unpack (dup "b" m) restStr))
    (dolist (b restBytes)
            (extend res (pack "b" (^ b (& mask_littleEndian 0xFF))))
            (set 'mask_littleEndian (>> mask_littleEndian 8))))
  res)

(define (firstByte-payload-mask->msg firstByte payload mask_32
                           , len bytes res)
  (set 'len (length payload)
       'bytes "")
  (extend bytes (pack "b" firstByte))
  (cond
   ((<= len 125)
    (extend bytes (pack "b" (| (if mask_32 b_MASK 0) len))))
   ((<= len 65535)
    (extend bytes (pack "b" (| (if mask_32 b_MASK 0) 126)))
    (extend bytes (pack "b" (>> (& 0xFF00 len) 8))) ; higher byte
    (extend bytes (pack "b" (& 0xFF len)))) ; lower byte (big endian)
   ("64 bit"
    (extend bytes (pack "b" (| (if mask_32 b_MASK 0) 127)))
    (extend bytes (uint64->net-bstring len))))
  (if mask_32
      (extend bytes (pack "lu" mask_32))) ; written in host byte order!
  (if (string? payload)
      (if mask_32
          (append bytes (mask payload mask_32))
          (append bytes payload))
      (throw-error "not implemented")))
(define (binary->msg payload mask_32)
  (firstByte-payload-mask->msg (| b_FIN oc_binary) payload mask_32))
(define (text->msg payload mask_32)
  (firstByte-payload-mask->msg (| b_FIN oc_text) payload mask_32))
(define (ping->msg payload mask_32)
  (firstByte-payload-mask->msg (| b_FIN oc_ping) payload mask_32))
(define (pong->msg payload mask_32)
  (firstByte-payload-mask->msg (| b_FIN oc_pong) payload mask_32))
;; fragmented
(define (first-fragment-opcode->msg opcode payload mask_32)
  (firstByte-payload-mask->msg opcode payload mask_32))
(define (fragment->msg payload mask_32)
  (firstByte-payload-mask->msg 0x0 payload mask_32))
(define (last-fragment->msg payload mask_32)
  (firstByte-payload-mask->msg b_FIN payload mask_32))

(define (fragmented-opcode->msg opcode payloads mask_32
                                , (numPayloads (length payloads))
                                  (byteStr ""))
  (when (< numPayloads 2)
    (throw-error (string "At least two payloads - and not " numPayloads
                         " - needed for fragmented message.")))
  (extend byteStr (first-fragment-opcode->msg opcode (first payloads) mask_32))
  (dolist (pl (1 -1 payloads))
          (extend byteStr (fragment->msg pl mask_32)))
  (extend byteStr (last-fragment->msg (last payloads) mask_32))
  byteStr)

;; examples from http://tools.ietf.org/html/rfc6455#section-5.7
[text]
(bstring->hexrep-string (text->msg "Hello"))
(bstring->hexrep-string (text->msg "Hello" (bytes_host->int32 '(0x37 0xfa 0x21 0x3d))))
(bstring->hexrep-string (ping->msg "Hello"))
(bstring->hexrep-string (pong->msg "Hello" (bytes_host->int32 '(0x37 0xfa 0x21 0x3d))))
(bstring->hexrep-string (binary->msg (dup "H" 256)))
(0 100 (bstring->hexrep-string (binary->msg (dup "H" 65536))))
[/text]

(define (getPayload) ; uses var env from status-buffer->msg
  ;; rstring begins with mask or payload
  (if hasMask
      (set 'm ((unpack "lu" rstring) 0)
           'payloadStr (4 len rstring)  ; payloadStr after mask
           'payload (mask payloadStr m)
           'buffer ((+ 4 len) rstring)) ; rest after mask + payloadStr
      (set 'payload (0 len rstring)  ; payloadStr
           'buffer (len rstring))) ; rest after payloadStr
  (if (not isFIN) ; msg unfinished
      (if (nil? status) ; first fragment?
          (set 'status (list payload opcode rsv)) ; yes: store payload and ..
          (extend (status 0) payload)) ; no: keep .. opcode, rsv
      (set 'res
           (if isControl
               ;; may be an intermediate ctrl msg
               (list (list payload opcode rsv) ; ctrl msg ..
                     status ; nil or unfinished fragmented msg
                     buffer) ; "" or rest not processed so far
               ;; finished non-ctrl msg
               (list (if (nil? status)
                         (list payload opcode rsv)
                         (list (append (status 0) payload)
                               (status 1)
                               (status 2)))
                     nil ; status
                     buffer))))) ; "" or rest not processed so far

;; in : (status buffer)
;;      (nil buffer) ; first
;; out: ((payload opcode rsv)) status buffer) -> msg
;;      (nil                   status buffer) -> no msg
(define (status-buffer->msg status buffer
                            , res bytes rstring rlen
                              isFIN rsv opcode isContinuation isControl
                              hasMask numMaskBytes len
                              neededBytes m)
  (while (nil? res)
    (cond
     ((< (length buffer) 2)
      (set 'res (list nil status buffer))) ; missing bytes: give it back
     ("else"
      (set 'bytes (unpack "bb" buffer)
           'rstring (2 buffer) ; eat first bytes from buffer
           'rlen (length rstring))
      (set 'isFIN (FIN? (bytes 0))
           'rsv (& mask_RSV (bytes 0))
           'opcode (& mask_opcode (bytes 0))
           'isContinuation (oc_continuation? opcode)
           'isControl (oc_control? opcode)
           'hasMask (MASK? (bytes 1))
           'numMaskBytes (if hasMask 4 0)
           'len (& mask_len (bytes 1)))
      (when status ; fragmented message after first fragment
          (when (not (or isContinuation isControl))
            (throw-error
             (string "Protocol error while parsing fragmented "
                     "message: frame has to be continuation or control msg."))))
      (when isControl
        (when (not isFIN)
          (throw-error "Protocol error: control code needs FIN bit."))
        (when (> len 125)
          (throw-error (string "Protocol error: control code's payload len "
                               len " > 125."))))
      (cond
       ((<= len 125)
        (set 'neededBytes (+ numMaskBytes len)))
       ((= len 126)
        (if (>= rlen 2)
            (set 'len ((unpack ">u" rstring) 0)
                 'rstring (2 rstring)
                 'neededBytes (+ numMaskBytes len)))) ; else nil neededBytes
       ("else" ; (= len 127)
        (if (>= rlen 8)
            (set 'len ((unpack ">Lu" rstring) 0)
                 'rstring (8 rstring)
                 'neededBytes (+ numMaskBytes len))))) ; else nil neededBytes
      (if (nil? neededBytes)
          (set 'res (list nil status buffer)) ; bytes missing
          (if (< (length rstring) neededBytes)
              (set 'res (list nil status buffer)) ; bytes missing
              (getPayload)
              ))))
    ) ; while
  ;;(dbg:expr res)
  res)

;;EOF
