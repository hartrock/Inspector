;;todo:
;;  - move to string utils (however named (U_S?));
;;  - postto newlisp forum regarding 'faster byte access', etc.

(load-module "Util.lsp")

(context 'UTF8)

;; slice for (valid) UTF-8 strings
(define (UTF8:UTF8-slice str pos num)
  (case num
    (nil (join (pos (explode str))))
    (true (join (pos num (explode str))))))
;; check position in (valid) UTF-8 string
;; return val:
;; - pos in range: char at pos inside UTF-8 string,
;; - pos out of range: eval of opt fun or nil
(define (at-ifAbsent str pos funOrNil)
  (Util:at-ifAbsent str pos fun utf8len))


;; Bytes
(macro (em_std-seq-byte B)
  (= (& B 0xC0) 0x80))
(macro (em_byte-at Str Pos) ; MAIN:slice, because ..
  (first (unpack "b" (slice Str Pos 1)))) ; .. we want bytes here

;; UTF-8 string check
;; - expansion macro for fast looping (but be aware: Pos will be changed here)
;; - returns
;;   - next pos in str or str len: for valid,
;;   - nil                       : for invalid
;;    UTF-8 char at given start Pos;
;; - if there is a *valid* UTF-8-char at given Pos, then Pos contains ret val
;;   with return (expansion macro)
;; Note:
;;   In case of an invalid UTF-8 sequence, at return Pos (modified arg) may be
;;   after, inside or before a valid one (following it) at return. So search
;;   for next valid UTF-8 sequence should start immediately after given start
;;   Pos (to be stored as copy before calling).
(macro (em_valid-UTF-8-char? Str LenStr Pos)
  (local (lenStr b_1 b_2 b_3 b_4)
    (if (< Pos LenStr)
        (begin
          (set 'b_1 (em_byte-at Str Pos))
          (if (< b_1 128)
              (++ Pos)
              (cond
               ((= (& b_1 0xE0) 0xC0) ; 2
                (if (>= b_1 0xC2)
                    (begin
                      (if (< (++ Pos) LenStr)
                          (begin
                            (set 'b_2 (em_byte-at Str Pos))
                            (if (em_std-seq-byte b_2)
                                (++ Pos)))))))
               ((= (& b_1 0xF0) 0xE0) ; 3
                (if (< (+ Pos 2) LenStr)
                    (begin
                      (set 'b_2 (em_byte-at Str (++ Pos))
                           'b_3 (em_byte-at Str (++ Pos)))
                      (if (or
                           (and (=  b_1 0xE0) ; E0
                                (= (& b_2 0xE0) 0xA0) (em_std-seq-byte b_3))
                           (and (<= b_1 0xEC) ; E1..EC
                                (em_std-seq-byte b_2) (em_std-seq-byte b_3))
                           (and (=  b_1 0xED) ; ED
                                (= (& b_2 0xE0) 0x80) (em_std-seq-byte b_3))
                           (and               ; EE..EFvalid-UTF-8-string?
                            (em_std-seq-byte b_2) (em_std-seq-byte b_3)))
                          (++ Pos)))))
               ((= (& b_1 0xF8) 0xF0) ; 4
                (if (< (+ Pos 3) LenStr)
                    (begin
                      (set 'b_2 (em_byte-at Str (++ Pos))
                           'b_3 (em_byte-at Str (++ Pos))
                           'b_4 (em_byte-at Str (++ Pos)))
                      (if (or
                           (and (= b_1 0xF0)  ; F0
                                (em_std-seq-byte b_2) (!= (& b_2 0xF0 0x80))
                                (em_std-seq-byte b_3) (em_std-seq-byte b_4))
                           (and (<= b_1 0xF3) ; F1..F3
                                (em_std-seq-byte b_2)
                                (em_std-seq-byte b_3) (em_std-seq-byte b_4))
                           (and (= b_1 0xF4)  ; F4
                                (= (& b_2 0xF0) 0x80) ; implicates std seq char
                                (em_std-seq-byte b_3) (em_std-seq-byte b_4)))
                          (++ Pos)))))))))))
;; returns:
;; - startPos: if/of invalid UTF-8 byte sequence found, starting from pos
;; - nil     : for valid UTF-8 string
;; Note: if there is an invalid UTF-8 sequence, searching for next valid one
;;   should continue just after invalid ones startPos.
;;
;; Does not change arguments; emacro to avoid string copying in loop of
;; invalid-UTF-8-string-chunks? below.
(macro (em_invalid-UTF-8-string? Str Len StartPos)
  (local (pos checkPos)
    (set 'pos StartPos) ; copy for not changing arg below
    (while
        (and
         (< pos Len)
         (begin
           (set 'checkPos pos)
           ;;*changes* pos to next after valid or first after invalid UTF-8 char
           (em_valid-UTF-8-char? Str Len pos))));rets nil, if invalid UTF-8 char
    (if (= pos Len) ; first pos after valid UTF-8 char?
        nil         ; valid
        checkPos))) ;errorneous seq is starting here (and may end here or later)
(define (invalid-UTF-8-string? str
                               (startPos 0) ; opt offset for invalid seq search
                               , (len (length str)))
  (em_invalid-UTF-8-string? str len startPos))
;;
;; rets true for empty or nil string
(define (valid-UTF-8-string? str
                             , (len (length str)))
  (not (em_invalid-UTF-8-string? str len 0))) ; does not change args, so 0 is OK
;; only true for str containing exactly one UTF-8 char
(define (one-valid-UTF-8-char? str
                               , (len (length str))
                                 (pos 0)) ;em_valid-UTF-8-char? needs fresh var!
  (= (em_valid-UTF-8-char? str len pos) ; rets nil for empty or nil str
     len))
;;
(define (invalid-UTF-8-string-chunks? str (startPos 0)
                                      , (len (length str))
                                        chunkBegin chunkEnd pos chunkSlices)
  (set 'chunkBegin (em_invalid-UTF-8-string? str len startPos))
  (while chunkBegin ; nil for valid str -> loop not entered
    (set 'chunkEnd (+ chunkBegin 1))
    (while (and (set 'pos (em_invalid-UTF-8-string? str len chunkEnd))
                (= pos chunkEnd))
      (++ chunkEnd))
    (push (cons chunkBegin (- chunkEnd chunkBegin)) chunkSlices -1)
    (set 'chunkBegin pos)) ; begin of next invalid chunk or nil
  chunkSlices)


(context MAIN)
;;EOF
