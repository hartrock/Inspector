(load-module "JSON.lsp")
(load-module "Util.lsp")

(context 'Introspection)

;; Notes:
;; - Expensive operations are:
;;   - checking for invalid UTF-8: needed not only for strings, but also e.g.
;;     in lambdas;
;;   - quoting in two steps:
;;     - first for getting valid UTF-8,
;;     - second for quoting the result to get a valid JSON string.
;; But:
;; - make it work, then make it fast;
;; - robustness and good introspection (e.g. the details of string contents) is
;;   more important than speed.

(define (j-NL)  "\n")
(define (j-sNL) ",\n")
(define (j-sS)  ", ")
(define (j-S)   " ")

;; only wraps key'n'val strings with quotes; without indent
(define (j-key-valStr-noquote keyStr valStr)
  (append "\"" keyStr "\":" (j-S) "\"" valStr "\""))

;; Quoting valStr in one or two steps:
;; - 1 step for suited string (has to be valid UTF-8!): JSON string-quote alone
;;   here;
;; - 2 steps for unsuited string (e.g. invalid UTF-8):
;;   a) some kind of newLISP quoting *before* (ensuring valid UTF-8), and then
;;   b) JSON:string-quote here.
;; JSON quoting of valStr; expects valid UTF-8.
(define (j-key-valStr keyStr valStr)
  (j-key-valStr-noquote ; only wraps key'n'val strings with '"' chars
   keyStr ; default is not to quote keys
   (JSON:string-quote valStr)))

;; Symbols evaluating to strings have their own representation (see below),
;; which will be embedded as valJSONStr here (which contains JSON strings
;; containing quoted newLISP strings).
;; For finished JSON vals not to be JSON quoted (default is to treat vals as to
;; be quoted strings).
(define (j-key-valJSON keyStr valJSONStr)
  (append "\"" keyStr "\":" (j-S) valJSONStr))
;; Variant for keys to be JSON-quoted (default is not to quote them).
(define (j-keyQuote-valJSON keyStr valJSONStr)
  (j-key-valJSON
   (JSON:string-quote keyStr)
   valJSONStr))

;; with indent
(define (ji-key-valStr keyStr valStr)
  (append iis (j-key-valStr keyStr valStr)))
(define (ji-key-valStr-noquote keyStr valStr)
  (append iis (j-key-valStr-noquote keyStr valStr)))
(define (ji-key-valJSON keyStr valJSONStr) ; valJSONStr with its own indent
  (append iis (j-key-valJSON keyStr valJSONStr)))

(set 'minimalOutput nil)
(if minimalOutput
    ;; minimal versions omitting JSON whitespace
    (set 'indentPlusStr ""
         'ois ""
         'iis ""
         'j-NL (lambda () "")
         'j-sNL (lambda () ",")
         'j-sS (lambda () ",")
         'j-S (lambda () "")
         ;; switch indent off
         'ji-key-valStr j-key-valStr
         'ji-key-valStr-noquote j-key-valStr-noquote
         'ji-key-valJSON j-key-valJSON)
    (set 'indentPlusStr "  " ; mandatory
         'ois "" ; needed by ji-* funcs (if not switched off), if not called ..
         'iis "")) ; .. via json-str/json-expr (which sets them)


;; shortcuts without indent
(define (j-type typeStr)
  (j-key-valStr-noquote "type" typeStr))
(define (j-val-valStr-noquote valStr)
  (j-key-valStr-noquote "val" valStr))
(define (j-val-valStr valStr)
  (j-key-valStr "val" valStr))
;; shortcuts with indent
(define (ji-type typeStr)
  (ji-key-valStr-noquote "type" typeStr))
(define (ji-val-valStr valStr)
  (ji-key-valStr "val" valStr))
(define (ji-val-valJSON valJSONStr)
  (ji-key-valJSON "val" valJSONStr))

(define (toJSON-simple-noquote typeStr arg)
  (append "{" (j-S)
          (j-type typeStr) (j-sS) (j-val-valStr-noquote
                                   (string arg))
          (j-S) "}"))
;; Without -noquote for string representation of lambdas...
(define (toJSON-simple typeStr arg)
  (append "{" (j-S)
          (j-type typeStr) (j-sS) (j-val-valStr
                                   (JSON:newLISP-quote (string arg)))
          (j-S) "}"))
(define (toJSON-simple-multiline typeStr arg)
  (append "{" (j-NL)
          (ji-type typeStr) (j-sNL) (ji-val-valStr
                                     (JSON:newLISP-quote (string arg)))
          (j-NL)
          ois "}"))
(define (j-obj-content contentStr)
  (append "{"
          (j-NL)
          contentStr
          (j-NL)
          ois "}"))

(define (j-typeObj-content typeStr contentStr
                           , (iis (append ois indentPlusStr)))
  (j-obj-content
   (append
    iis (j-type typeStr)
    (j-sNL)
    contentStr)))

(define (json-for-sym s fromOrNil
                      , (iis (append ois indentPlusStr))
                        (prefix_term (Util:prefix-term-strings s))
                        (prefixStr (prefix_term 0))
                        (termStr (prefix_term 1))
                        (prefixStrOrChunks
                         (JSON:newLISP-chunk-quote-non-UTF-8-val prefixStr))
                        (termStrOrChunks
                         (JSON:newLISP-chunk-quote-non-UTF-8-val termStr)))
  ;;(dbg:expr (string (prefix s)) (prefix_term 0))
  (j-typeObj-content
   "sym"
   (append
    ;; iis indent
    (ji-key-valStr  "prefix"            (JSON:newLISP-string-quote prefixStr))
    (if (array? prefixStrOrChunks)
        (append
         (j-sS) ; ', ' only
         (j-key-valJSON "prefix_chunks" (JSON:toJSON prefixStrOrChunks ois)))
        "")
    (j-sS) ; ', ' only
    ;; no indent
    (j-key-valStr   "term"              (JSON:newLISP-string-quote termStr))
    (if (array? termStrOrChunks)
        (append
         (j-sS) ; ', ' only
         (j-key-valJSON "term_chunks"   (JSON:toJSON termStrOrChunks ois)))
        "")
    (j-sNL) ; with NL
    ;; iis indent
    (ji-key-valJSON "global?"           (if (global?    s) "true" "false"))
    (j-sS) ; ', ' only
    ;; no indent
    (j-key-valJSON  "protected?"        (if (protected? s) "true" "false"))
    (j-sS) ; ', ' only
    ;; no indent
    (j-key-valJSON  "termString_legal?" (if (legal? termStr) "true" "false"))
    ;; we are interested in val of sym, but we want to avoid infinite recursion:
    ;; - sym_1 -> sym_2 -> sym_1
    (if (not (symbol? fromOrNil))
        (append
         (j-sNL) ; NL
         ;; iis indent
         (ji-val-valJSON (json-str (eval s) s iis)))
        ""))))

(define (json-for-dynsym s fromOrNil
                         , (iis (append ois indentPlusStr))
                           (dptt (Util:dynamic-prefix-term-target-strings s))
                           (varPrefixStr (dptt 0))
                           (varTermStr (dptt 1))
                           (targetTermStr (dptt 2))
                           (varPrefixStrOrChunks
                            (JSON:newLISP-chunk-quote-non-UTF-8-val
                             varPrefixStr))
                           (varTermStrOrChunks
                            (JSON:newLISP-chunk-quote-non-UTF-8-val
                             varTermStr))
                           (targetTermStrOrChunks
                            (JSON:newLISP-chunk-quote-non-UTF-8-val
                             targetTermStr)))
  ;;(dbg:expr (string s))
  (j-typeObj-content
   "dynsym"
   (append
    ;; iis indent
    (ji-key-valStr  "varPrefix"               (JSON:newLISP-string-quote
                                               varPrefixStr))
    (if (array? varPrefixStrOrChunks)
        (append
         (j-sS) ; ', ' only
         (j-key-valJSON "varPrefix_chunks"    (JSON:toJSON varPrefixStrOrChunks
                                                           ois)))
        "")
    (j-sS) ; ', ' only
    ;; no indent
    (j-key-valStr   "varTerm"                 (JSON:newLISP-string-quote
                                               varTermStr))
    (if (array? varTermStrOrChunks)
        (append
         (j-sS) ; ', ' only
         (j-key-valJSON "varTerm_chunks"      (JSON:toJSON varTermStrOrChunks
                                                           ois)))
        "")
    (j-sNL) ; with NL
    ;; iis indent
    (ji-key-valStr  "targetTerm"              (JSON:newLISP-string-quote
                                               targetTermStr))
    (if (array? targetTermStrOrChunks)
        (append
         (j-sS) ; ', ' only
         (j-key-valJSON "targetTerm_chunks"   (JSON:toJSON targetTermStrOrChunks
                                                           ois)))
        "")
    (j-sS) ; ', ' only
    ;; no indent
    (j-key-valJSON  "targetTermString_legal?" (if (legal? targetTermStr)
                                                  "true"
                                                  "false"))
    ;; no val here: depends from eval of dynsym var, which is another symbol
  )))

(define (json-for-context c
                          , (iis (append ois indentPlusStr))
                            (termStr (term c)))
  (j-typeObj-content
   "context"
   (append
    (ji-key-valStr  "term"       (JSON:newLISP-string-quote termStr))
    (j-sNL)
    ;; no creation of default (functor) sym
    (ji-key-valJSON "default" (if (Util:default? c)
                                  "true"
                                  "null")))))

(define (json-for-lambda l
                         , (iis (append ois indentPlusStr)))
  (toJSON-simple-multiline "lambda" l))

(define (json-for-macro m
                        , (iis (append ois indentPlusStr)))
  (toJSON-simple-multiline "macro" m))

(define (json-for-listOrArray typeStr loa
                              , (iis (append ois indentPlusStr)))
  (j-typeObj-content
   typeStr
   (append
    (ji-key-valJSON "length" (string (length loa))) (j-sNL)
    (ji-key-valStr "rep" (JSON:newLISP-quote (string loa))))))

(macro (json-for-list L)
  (json-for-listOrArray "list" L))
(macro (json-for-array A)
  (json-for-listOrArray "array" A))

;; Creates string chunks separated by "\n" (at end, if it exists): so in a GUI
;; each can be rendered at its own line (similar to original text).
(define (quoted-string-chunks str
                              , (len (length str))
                                (startPos 0) (pos 0)
                                chunks)
  (if (null? len)
      (set 'chunks '(""))
      (begin
        (while (set 'pos (find "\n" str nil pos))
          (push (slice str startPos (++ (- pos startPos))) chunks -1)
          (set 'startPos (++ pos)))
        (if (< startPos len)
            (push (slice str startPos (- len startPos)) chunks -1))))
  (if nil
      (map
       (fn (chunk)
         (let (invalid (UTF8:invalid-UTF-8-string? chunk))
           ;;(dbg:expr chunk)
           (append
            "\""
            ((if invalid
                 JSON:newLISP-byte-quote chunk
                 JSON:newLISP-string-quote-ASCII)
             chunk)
            "\"")))
       chunks)
      (map (fn (chunk)
             (append
              "\""
              (JSON:newLISP-string-quote chunk)
              "\""))
           chunks)))
(define (quoted-string-rep str
                           , (chunks (quoted-string-chunks str)))
  (append "["
          (join (map (fn (chunk)
                       (append "\""
                               (JSON:string-quote chunk)
                               "\""))
                     chunks)
                ",")
          "]"))

;; Problems related to getting *precise* info about string contents:
;; - standard newLISP string quoting inside to be quoted string
;;   - "":
;;     1. does not show unprintable chars (e.g. \127)
;;     2. does not show binary data leading to invalid unicode
;;     3. using source representation; example source:
;;        "(set 'str \"bar\\000buz\")\n\n"
;;        - for larger contents string will be stored with a fixed chunk size
;;          not honoring "\n"
;;        - if there are multiple "" chunks: how to parse source rep for
;;          separating chunks therein?
;;   - [text] same as 1., 2. for "", and it does not show string contents
;;     precisely (\t\n) enough
;; So this approach has been cancelled in favour of other solutions:

;; TODO: replacement of "unprintable chars" property by e.g. "binary",
;; "non-UTF-8 bytes"
(define (json-for-string str
                         , (iis (append ois indentPlusStr))
                           strLen)
  (set 'strLen (length str))
  ;;(dbg:expr strLen)
  (j-typeObj-content
   "string"
   (append
    (ji-key-valJSON "length" (string strLen)) (j-sNL)
    ;;TODO: UTF-8 length, if valid UTF-8
    (ji-key-valJSON "rep"
                    (quoted-string-rep str)))))

(macro (em-json Def-or-Defmac Fun)
  (Def-or-Defmac (Fun a fromOrNil
                      (ois "") ; outer indent string
                      (iis (append ois ; iis in shared use
                                   indentPlusStr)))
    (append
     (cond
      ((integer? a)
       (toJSON-simple-noquote "integer" a))
      ((bigint? a)
       (toJSON-simple-noquote "bigint" a))
      ((float? a)
       (toJSON-simple-noquote "float" a))
      ((string? a)
       (json-for-string a iis))
      ((quote? a)
       (toJSON-simple-noquote "quote" a))
      ((lambda? a)
       (json-for-lambda a))
      ((array? a)
       (json-for-array a))
      ((primitive? a)
       (toJSON-simple-noquote "primitive" a))
      ((context? a)
       (json-for-context a))
      ;; seq counts here
      ((Util:dynamic-symbol? a)
       (json-for-dynsym a))
      ((symbol? a)
       (json-for-sym a fromOrNil))
      ((macro? a) ;&& seq counts: macro? for em-macro sym is true
       (json-for-macro a))
      ((list? a) ; a macro is a list, too
       (json-for-list a)) ; may call back with increased indent
      ((nil? a) ; (nil? 'nil) -> true
       (toJSON-simple-noquote "nil" a))
      ((= true a) ; (true? a) is true for many things...
       (toJSON-simple-noquote "true" a))
      ("default"
       (throw-error (string "should not happen, unknown argument: " a))
       (toJSON-simple-noquote "unknown" a))))))


(em-json define json-str)
(em-json define-macro json-expr)

;;(define (sym-str sy
(define (standard-sym-str sy
                          , (prefixTerm (Util:prefix-term-strings sy)))
  (JSON:newLISP-string-quote
   (append (prefixTerm 0)
           ":"
           (prefixTerm 1))))
(define (sym-str sy)
  (if (Util:dynamic-symbol? sy)
      (JSON:valJSON (json-str sy))
      (standard-sym-str sy)))
(define (sym-actions sy)
  ;; (dbg:expr sy) -> fails due to some recursion
  ;;(dbg:expr-loc 'sym-actions (string sy))
  (extend res ; key JSON-quoted ..
          (j-keyQuote-valJSON ; .. for formerly quoted irregular syms
           (sym-str sy)
           (json-str sy))))
(define (symbols-to-JSON syms) ; listOrNil
  ;;(dbg:expr syms)
  (if (nil? syms)
      "null"
      (begin
        ;;TODO: Introspection type 'omitted'
        (set 'syms (difference syms '(Introspection:res))) ; quiet a lot
        (set 'res "")
        (if syms (sym-actions (first syms)))
        (dolist (sy (rest syms))
                (extend res ",\n")
                (sym-actions sy))
        (j-obj-content res))))
(define (multi-symbols-to-JSON symsList) ; list of lists
  (when (not (list? symsList))
    (throw-error "list expected"))
  (when (and symsList ; not empty
             (not (or (list? (first symsList))
                      (nil? (first symsList)))))
    (throw-error "list of list or nil elements expected"))
  ;; maps list of syms (list) -> JSON array of JSON objects
  (append
   "[\n"
   (join (map symbols-to-JSON symsList) ", ")
   "\n]"))

(define (symbols-in-list-rec l)
  ;; catch for val_nil:term or eval2ctx_var:nonExistingIn_ctx constructs
  ;; (leading to nil for failed catch/eval):
  ;; "ERR: context expected in function symbol? : val_nil"
  (if (list? l)
      (unique
       (ref-all nil
                l
                (fn (ignore sy)
                  ;; (or
                  ;;  (and (catch (symbol? sy) 'r)
                  ;;       r)
                  ;;  (global? sy))) ; for eval2ctx:non-existing-term
                  (Util:symbol-like? sy))
                true))
      nil))
;; for getting syms in "(expand '(..." part of expansion macros
(define (symbols-in-list-rec-with-quoted-sublists l)
  (unique
   (flat
    (append
     (symbols-in-list-rec l)
     (map symbols-in-list-rec
          (map eval
               (ref-all
                nil
                l
                (fn (ignore e)
                  (and (quote? e)
                       (list? (eval e))))
                true)))))))
(define (symbols-in-list l)
  (symbols-in-list-rec-with-quoted-sublists l))

(when true ; used by Inspector:symbols-JSON; may become obsolete later
  (define (symbols-in-symStr symStr
                             , (symVal (and
                                        (Util:symbol-string? symStr)
                                        (eval-string symStr))))
    (if (list? symVal)
        (symbols-in-list symVal)))
  (define (symbols-in-symStrs symStrs)
    (map symbols-in-symStr symStrs)))

(define (symbols-in-list-as-string-array l)
  (set 'sil (symbols-in-list l))
  ;;(dbg:expr-loc 'symbols-in-list-as-string-array sil)
  (JSON:list->array
   (map (fn (sy)
          (sym-str sy))
        sil)))
(define (symbols-in-listSym-as-string-array listSym)
  (symbols-in-list-as-string-array (eval listSym)))
(define (symbols-in-listSyms-to-JSON listSyms
                                     , obj)
  (dolist (sy listSyms)
          (JSON:add-prop (sym-str sy)
                         (symbols-in-listSym-as-string-array sy)
                         obj))
  (JSON:toJSON obj))
(define (symbols-in-listSym-to-JSON_listSym listSym)
  (JSON:toJSON (symbols-in-listSym-as-string-array listSym)))

;; Quoting  | For
;; ---------+--------------------------------------------------------
;;   '      | dealing with expansion macros (to avoid expanding them)
;;   string | the former *and* quoting of non-UTF-8 symbols
;; ---------+--------------------------------------------------------
(define (unquote-listSym qsy)
  (if (quote? qsy)
      (eval qsy)
      (string? qsy)
      (eval-string qsy))) ; string quote good for expansion macros, too
(define (symbols-in-listSymsQuoted-to-JSON listSymsQuoted)
  (symbols-in-listSyms-to-JSON (map unquote-listSym listSymsQuoted)))
(define (symbols-in-listSymQuoted-to-JSON_listSym listSymQuoted)
  (symbols-in-listSym-to-JSON_listSym (unquote-listSym listSymQuoted)))


(set 'loadedFlag true)
;; EOF
