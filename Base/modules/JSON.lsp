(load-module "UTF8.lsp")


(context 'JSON)

;; Purpose:
;; Create JSON from newLISP data consisting of 'basic' types having an
;; equivalent structure.
;; Limitation:
;; Only for newLISP types with corresponding JSON types: for others
;; Introspection is the way to go.
;; Restrictions:
;; - property keys have to be valid UTF-8 (unchecked)
;; Note:
;; Arrays could be built as lists and then converted by using
;; list->array (but be careful with em_ variant).

;; For JSON see specifications:
;;   https://tools.ietf.org/html/rfc7159
;;   http://www.json.org/
;;   http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
;; .
;; Unicode chars are understood by both newLISP and JSON parsers; to be quoted
;; in strings are:
;; '\\'
;; '\"'
;; '\n'
;; '\t'
;; '\r'
;; '\b'
;; '\f'
;; and other control chars in the ASCII range; denoted in JSON as
;;   \u<four-hex-digit>, in newLISP often as \ddd (decimal bytes 0..255).
;; .
;; '\/' is special: it has to be interpreted as '/' in case of parsing, as well
;; as an unescaped '/', which is allowed, too.
;; Same holds for newLISP parsing:
;; - "foo\/bar" is the same as "foo/bar", and:
;; (= " \/ " " / ") -> true
;;
;; Notes:
;; There is no ASCII shortcut '\v' (vertical tab) in both JSON spec and newLISP.
;; For newLISP
;; - '\v'   in  "foo\vbar"   silently becomes letter 'v'; but
;; - '\011' in  "foo\011bar" (decimal '\011' corresponds to '\v') has its
;;   effect.
;; '\000' will sometimes be treated as end of string char:
;;   (append "\000foo" "\000bar")          -> "\000foo\000bar"
;; but
;;   (string (append "\000foo" "\000bar")) -> ""

;; ASCIIfying invalid UTF-8 and string quoting (for newLISP strings or symbols).
;; Note:
;; For binary-only *string* representations all bytes could be quoted somehow
;; (e.g. as \ddd or \xHH (hex)).
;; Problem here: there is no criterium for interpreting a string as 'binary';
;; e.g. a flag or an additional string type for *explicitely* stating, if a
;; string should be treated as an UTF-8 or as a binary string (ideally trying to
;; create an invalid UTF-8 string gives an error).
(define (newLISP-ASCIIfy-n-string-quote-ASCII str
                                              , (len (length str))
                                                (lastPos (- len 1))
                                                (res "")
                                                b)
  (for (ix 0 lastPos)
       (set 'b (UTF8:em_byte-at str ix))
       (extend res (if (< b 128)
                       (newLISP-string-quote-ASCII (char b))
                       (string "\\" b))))
  res)
;; For binary strings: quote all bytes.
(define (newLISP-byte-quote str
                            , (len (length str))
                              (lastPos (- len 1))
                              (res "")
                              b)
  (for (ix 0 lastPos)
       (set 'b (UTF8:em_byte-at str ix))
       (extend res (format "\\%03d" b)))
  res)
;; ASCIIfying invalid UTF-8 and quoting of invisible ASCII (for output from
;; remote stdout/stderr).
(define (newLISP-ASCIIfy-n-quote-invisible-ASCII str
                                                 , (len (length str))
                                                   (lastPos (- len 1))
                                                   (res "")
                                                   b)
  (for (ix 0 lastPos)
       (set 'b (UTF8:em_byte-at str ix))
       (extend res (if (< b 128)
                       (newLISP-quote-invisible-ASCII (char b))
                       (string "\\" b))))
  res)

;; Regex for all ASCII shortcuts, control chars and \127 (DEL): it depends on
;; quoting method, if they actually will be changed by replacement.
;; ASCII chars to be quoted to get valid and visible - newLISP \127 (DEL) -
;; string representation for newLISP or JSON strings: sufficient only for valid
;; UTF-8.
(set 'rc_ASCII_toBeQuotedInString
     (regex-comp
      "(\\\\|\"|\\000|\001|\002|\003|\004|\005|\006|\007|\b|\t|\n|\011|\f|\r|\014|\015|\016|\017|\018|\019|\020|\021|\022|\023|\024|\025|\026|\027|\028|\029|\030|\031|\127)"))
      ;;"(\\\\|\"|\\000|\001|\002|\003|\004|\005|\006|\007|\b|\t|\n|\011|\f|\r|\014|\015|\016|\017|\018|\019|\020|\021|\022|\023|\024|\025|\026|\027|\028|\029|\030|\031)"))
(set 'rc_ASCII_unprintedAsChar
     (regex-comp
      "(\\000|\001|\002|\003|\004|\005|\006|\007|\b|\t|\n|\011|\f|\r|\014|\015|\016|\017|\018|\019|\020|\021|\022|\023|\024|\025|\026|\027|\028|\029|\030|\031|\032|\127)")) ; compared to the former: without '\' and '"', but with ' '
(define (contains-unprinted-ASCII-char? str)
  (find rc_ASCII_unprintedAsChar str 0x10000))
;; Create JSON strings from UTF-8 newLISP strings.
(define (string-quote str)
  (replace
   rc_ASCII_toBeQuotedInString
   str (case $1
         ("\\"   "\\\\")
         ("\""   "\\\"")
         ;; control codes/shortcuts
         ("\000" "\\u0000")
         ("\001" "\\u0001")
         ("\002" "\\u0002")
         ("\003" "\\u0003")
         ("\004" "\\u0004")
         ("\005" "\\u0005")
         ("\006" "\\u0006")
         ("\007" "\\u0007")
         ("\b"   "\\b")
         ("\t"   "\\t")
         ("\n"   "\\n")
         ("\011" "\\u000B") ; \v
         ("\f"   "\\f")
         ("\r"   "\\r")
         ("\014" "\\u000E")
         ("\015" "\\u000F")
         ("\016" "\\u0010")
         ("\017" "\\u0011")
         ("\018" "\\u0012")
         ("\019" "\\u0013")
         ("\020" "\\u0014")
         ("\021" "\\u0015")
         ("\022" "\\u0016")
         ("\023" "\\u0017")
         ("\024" "\\u0018")
         ("\025" "\\u0019")
         ("\026" "\\u001A")
         ("\027" "\\u001B")
         ("\028" "\\u001C")
         ("\029" "\\u001D")
         ("\030" "\\u001E")
         ("\031" "\\u001F")
         ;;
         ("\127" "\u007F") ; unchanged (quoting not needed to get valid JSON)
         (true "<!implementation error!>"))
   0x10000))

;; Quoting in ASCII range to get valid newLISP string representation: validity
;; of UTF-8 outside ASCII range not checked/handled here.
;; Notes:
;; - quotes around have to be added;
;; - for transferring it as JSON string, result has to be quoted again for
;; getting valid JSON;
;; - expansion macro has not helped for increasing speed here.
(define (newLISP-string-quote-ASCII str)
  (replace
   rc_ASCII_toBeQuotedInString
   str (case $1
         ("\\"   "\\\\")
         ("\""   "\\\"")
         ;; control codes/shortcuts
         ("\000" "\\000")
         ("\001" "\\001")
         ("\002" "\\002")
         ("\003" "\\003")
         ("\004" "\\004")
         ("\005" "\\005")
         ("\006" "\\006")
         ("\007" "\\007")
         ("\b"   "\\b")
         ("\t"   "\\t")
         ("\n"   "\\n")
         ("\011" "\\011") ; \v
         ("\f"   "\\f")
         ("\r"   "\\r")
         ("\014" "\\014")
         ("\015" "\\015")
         ("\016" "\\016")
         ("\017" "\\017")
         ("\018" "\\018")
         ("\019" "\\019")
         ("\020" "\\020")
         ("\021" "\\021")
         ("\022" "\\022")
         ("\023" "\\023")
         ("\024" "\\024")
         ("\025" "\\025")
         ("\026" "\\026")
         ("\027" "\\027")
         ("\028" "\\028")
         ("\029" "\\029")
         ("\030" "\\030")
         ("\031" "\\031")
         ;; invisible without quoting (shown as '' or ' ')
         ("\127" "\\127")
         (true "<!implementation error!>"))
   0x10000))
;; Show most ASCII ctrl chars as \ddd (decimal): validity of UTF-8 outside ASCII
;; range not checked/handled here.
;; Note:
;; This is for giving more precise info about transferred ASCII bytes in output
;; error cases (bytes leading to invalid UTF-8).
(define (newLISP-quote-invisible-ASCII str)
  (replace
   rc_ASCII_toBeQuotedInString
   str (case $1
         ("\\"   "\\")    ; let it be
         ("\""   "\"")    ; let it be
         ("\000" "\\000")
         ("\001" "\\001")
         ("\002" "\\002")
         ("\003" "\\003")
         ("\004" "\\004")
         ("\005" "\\005")
         ("\006" "\\006")
         ("\007" "\\007")
         ("\b"   "\b")    ; let it be
         ("\t"   "\t")    ; let it be
         ("\n"   "\n")    ; let it be (important for detecting prompt)
         ("\011" "\011")  ; let it be ; \v
         ("\f"   "\f")    ; let it be (like \v)
         ("\r"   "\r")    ; let it be
         ("\014" "\\014")
         ("\015" "\\015")
         ("\016" "\\016")
         ("\017" "\\017")
         ("\018" "\\018")
         ("\019" "\\019")
         ("\020" "\\020")
         ("\021" "\\021")
         ("\022" "\\022")
         ("\023" "\\023")
         ("\024" "\\024")
         ("\025" "\\025")
         ("\026" "\\026")
         ("\027" "\\027") ; ESC
         ("\028" "\\028")
         ("\029" "\\029")
         ("\030" "\\030")
         ("\031" "\\031")
         ;; invisible without quoting (shown as '' or ' ')
         ("\127" "\\127") ; DEL
         (true "<!implementation error!>"))
   0x10000))
(define (newLISP-string-quote str)
  ((if (UTF8:valid-UTF-8-string? str)
      JSON:newLISP-string-quote-ASCII
      JSON:newLISP-ASCIIfy-n-string-quote-ASCII)
   str))
(define (newLISP-string-quote str
                              , (strOrChunks
                                 (newLISP-chunk-quote-non-UTF-8 str)))
  (if (string? strOrChunks)
      (JSON:newLISP-string-quote-ASCII strOrChunks)
      (let (valid_UTF-8_flag true res "")
        (dolist (chunk strOrChunks)
                (extend res (if valid_UTF-8_flag
                                (JSON:newLISP-string-quote-ASCII chunk)
                                chunk)) ; already byte-quoted
                (set 'valid_UTF-8_flag (not valid_UTF-8_flag)))
        res)))
(set 'non-UTF-8-in-markerStr   "bytes"
     'non-UTF-8-markerStr      (append "["   non-UTF-8-in-markerStr "]")
     'non-UTF-8-beginMarkerStr (append "["   non-UTF-8-in-markerStr "..]")
     'non-UTF-8-endMarkerStr   (append "[.." non-UTF-8-in-markerStr "]")
     'non-UTF-8-showBeginFlag  true
     'non-UTF-8-showEndFlag    true)
(define (quote-non-UTF-8 str)
  (let (quotedStr (newLISP-ASCIIfy-n-quote-invisible-ASCII str))
    (if (not (or non-UTF-8-showBeginFlag non-UTF-8-showEndFlag))
        quotedStr ; shortcut
        (append (if non-UTF-8-showBeginFlag
                    (if non-UTF-8-showEndFlag
                        non-UTF-8-beginMarkerStr
                        non-UTF-8-markerStr)
                    "")
                quotedStr
                (if non-UTF-8-showEndFlag
                    (if non-UTF-8-showBeginFlag
                        non-UTF-8-endMarkerStr
                        non-UTF-8-markerStr)
                    "")))))
(define (newLISP-quote str)
  (if (UTF8:valid-UTF-8-string? str)
      str ; (newLISP-quote-invisible-ASCII str) ;this alt may or may not be good
      (quote-non-UTF-8 str)))

(macro (em_newLISP-chunk-quote Str)
  (let (chunks (UTF8:invalid-UTF-8-string-chunks? Str))
    (if chunks
        (let (off 0 res '())
          (dolist (chunkLoc chunks)
                  (let (chunkOff (first chunkLoc) chunkLen (last chunkLoc))
                    (push (slice Str off (- chunkOff off))
                          res -1)
                    (push (newLISP-byte-quote (slice Str chunkOff chunkLen))
                          res -1)
                    (set 'off (+ chunkOff chunkLen))))
          (push (slice Str off (- (length Str) off))
                res -1)
          res)
        Str)))
(define (newLISP-chunk-quote-non-UTF-8 str)
  (em_newLISP-chunk-quote str))
;; For using as prop val ensuring valid UTF-8.
(define (newLISP-chunk-quote-non-UTF-8-val str)
  (let (quoted (em_newLISP-chunk-quote str))
    (if (list? quoted)
        (list->array quoted)
        quoted)))


;; Note: hasn't led to speedup
;; (define (string-quote_? str)
;;   (replace
;;    rc_ASCII_toBeQuotedInString
;;    str (JSON_HM $1)
;;    0x10000)
;;   str)

;; Check key for UTF8:valid-UTF-8-string?
;; -> not needed: only valid UTF-8 keys allowed.
;; here (see restrictions at top).
(define (key-valJSON keyStr valJSONStr)
  (append "\"" (string-quote keyStr) "\":" objColonWS valJSONStr))
(define (h_val->JSON val)
  ;;(dbg:expr val)
  (cond
   ((string? val)
    (append
     "\"" (string-quote ; JSON quote alone only sufficient for *valid* UTF-8
           ;;todo: rm after switching to UTF-8 clean prop vals
           (if (UTF8:valid-UTF-8-string? val) ;in case of invalid UTF-8: just ..
               val ; no quote here: stdout/stderr should remain unchanged
               (quote-non-UTF-8 val))) ; .. ASCII-fy it
     "\""))
   ((number? val)
    (string val))
   ((= true val)
    "true")
   ((and (symbol? val) (= (term val) "false"))
    "false")
   ((nil? val)
    "null")
   ((JSON? val)
    (last val))
   ((obj? val)
    (obj->JSON val))
   ((array? val)
    (arr->JSON val))
   ("default"
    (throw-error (string "no conversion of val " val " to JSON")))))
(define (JSON? val)
  (and (list? val) (= (length val) 2) (symbol? (first val))
       (= (first val) 'JSON:mark)))
(define (obj? val) ; only correct, if checked for JSON? before
  (and (list? val) (not (lambda? val)) (not (macro? val))))
(define (prop? prop)
  (and (list? prop) (not (lambda? prop)) (not (macro? prop))
       (= (length prop) 2) (string? (prop 0))))
(define (prop->JSON prop)
  (when (not (prop? prop))
    (throw-error
     (string
      "property expected: " prop "; but: "
      (cond
       ((not (list? prop))
        "not a list")
       ((lambda? prop)
        "lambda")
       ((macro? prop)
        "lambda-macro")
       ((!= (length prop) 2)
        (string "length " (length prop)
                " (should be 2)"))
       ((not (string? (prop 0)))
        (string "key " (prop 0) " should be string"))
       ("??? (implementation error)")))))
  (let (key (prop 0) val (prop 1))
    (key-valJSON key (h_val->JSON val))))

(macro (em_propStr-obj-or-arr? PropStr)
  (let (c (last PropStr))
    (or (= c "}") (= c "]"))))
(macro (concat-JSON-elements EStrs NonStructBorderWS StructBorderWS)
  (apply
   (fn (e en) ; prop strings to be concatenated (en is nil for last)
     (if en
         (append e ; concat e ..
                 "," ; .. with separator, WS: ..
                 (if (or (em_propStr-obj-or-arr? (last e))
                         (em_propStr-obj-or-arr? (last en)))
                     StructBorderWS ; a) before/after obj/arr
                     NonStructBorderWS) ; b) (less) between non-objs/arrs
                 en) ; .. and en
         e)) ; last element without successor en
   EStrs
   2))
(define (obj->JSON l
                   , propStrings res)
  (letn ((oiStr (if (nil? oiStr) ; outer oiStr stems from val->JSON
                    ""
                    (append oiStr indentIncrementStr)))
         (iiStr (append oiStr indentIncrementStr)))
    (set 'propStrings (map (fn (e) (prop->JSON e))
                           l))
    (set 'res (concat-JSON-elements
               propStrings
               obj_nonStructBorderWS (obj_structBorderWSFun oiStr iiStr)))
    (append (objOpenFun oiStr iiStr)
            res
            (objCloseFun oiStr iiStr))))
(define (arr->JSON a
                   , elementStrings res)
  (letn ((oiStr (if (nil? oiStr) ; outer oiStr stems from val->JSON
                    ""
                    (append oiStr indentIncrementStr)))
         (iiStr (append oiStr indentIncrementStr)))
    (set 'elementStrings (map (fn (e) (h_val->JSON e))
                              a))
    (set 'res (concat-JSON-elements elementStrings
                                    arr_nonStructBorderWS (arr_structBorderWSFun
                                                           oiStr iiStr)))
    (append (arrOpenFun oiStr iiStr)
            res
            (arrCloseFun oiStr iiStr))))

(define (h_mlist arguments)
  (local (res)
    (dolist (e arguments)
            (push e res -1))
    res))
;;
;; DNW: (list E1 E2) : (list... converts array args to lists
(macro (em_mlist_2 E1 E2)
  (local (res) ; pushing keeps array args as arrays
    (push E1 res -1)
    (push E2 res -1)
    res))


;;
;; iface

;;
(macro (JSON:valJSON ValJSON)
  (list
   'JSON:mark ; sym for marking val as JSON string (not to be quoted anymore)
   ValJSON))
;;
(macro (em_list->array L)
  (if (null? (length L))
      (JSON:valJSON "[]") ; empty NL array not allowed
      (array (length L) L)))
(define (list->array l)
  (em_list->array l))

;; leave array elems as they are (do not convert them to lists)
(define (mlist) ; (mlist ... ; like (list ...
  (h_mlist (args)))
;; create array ..
(define (marray) ; (marray ... ; .. like (list ...
  (list->array (h_mlist (args)))) ; no em_: avoid multiple evaluations
;;
(macro (add-prop Key Val L) ; (add-prop "foo" "bar" l)
  (push (em_mlist_2 Key Val) L -1))
(macro (add-prop-valJSON Key ValJSON L) ; (add-prop-valJSON "foo" "\"bar\"" l)
  (push (em_mlist_2 Key (JSON:valJSON ValJSON)) L -1))
;;
;;think: may be too fat for generating many tiny structures, instead of less big
;;  ones (which has been the original usecase)
;; parametrizable: defaults are for pretty-print with whitespace
(define (toJSON val
                ;; optional from here
                (oiStr "") ; outer indent str
                (indentIncrementStr "  ")
                (objColonWS " ")          ; _ for WS: "key":_val
                (obj_nonStructBorderWS objColonWS) ; _ for WS: prop,_prop,_...
                (arr_nonStructBorderWS objColonWS) ; _ for WS: elem,_ elem,_...
                (obj_structBorderWSFun ; before/after a prop with obj/arr val
                 (fn (oiStr iiStr) (append "\n" iiStr)))
                (arr_structBorderWSFun ; before/after an obj/arr elem
                 (fn (oiStr iiStr) (append "\n" iiStr)))
                (objOpenFun
                 (fn (oiStr iiStr) (append "{\n" iiStr)))
                (objCloseFun
                 (fn (oiStr iiStr) (append "\n" oiStr "}")))
                (arrOpenFun
                 (fn (oiStr iiStr) (append "[\n" iiStr)))
                (arrCloseFun
                 (fn (oiStr iiStr) (append "\n" oiStr "]"))))
  (h_val->JSON val))
;; omitting any unneeded whitespace
(define (toJSON-minimal val)
  (toJSON val "" "" "" "" ""
          (fn () "") (fn () "")
          (fn () "{") (fn () "}")
          (fn () "[") (fn () "]")))


;; Overview
;; ========
;; newLISP val      -> JSON string ; comment
;; --------------------------------------
;; nil              -> null
;; true             -> true
;; 'false sym       -> false      ; works for ..
;; '<ctx>:false sym -> false      ; .. all contexts
;; <obj>            -> {...}      ; nL list containing (<string> val) prop lists
;; <array>          -> [...]      ; nL array containing vals
;; <number>         -> number
;; <string>         -> "string"   ; JSON quoted
;;
;; <sym>            -> invalid, if not 'false sym
;; lambda | macro   -> invalid
;; list             -> invalid, if not <obj> list

;; EOF
