(dbg:begin "util.lsp")

(load-libs 'assert)

(context 'util)

(define (detect-ifAbsent k l
                         (fun (fn (k e) (dbg:expr k e) (= k e)))
                         (ifAbsentFunOrNil nil))
  (dbg:expr k l)
  (if (list? l)
      (let (r (ref k l fun))
        (if (nil? r)
            (if ifAbsentFunOrNil (ifAbsentFunOrNil)) ; eval, if not nil
            (l r)))))

(define (compose f_o f_i)
  ;(dbg:expr f_o f_i)
  (expand (fn (x) (f_o (f_i x))) 'f_o 'f_i))

(define (any-satisfy l pred)
  (dbg:expr l pred)
  (dbg:expr (null? l))
  (not (not (find nil l (fn (k_ignored e) (pred e)))))) ; enforce true or nil

(define (all-satisfy l pred )
  (or (null? l)
      (not (any-satisfy l (compose not pred)))))
 

;;;; pos in vec

(define (in-range vec pos (lenFunc length))
  (let (len (lenFunc vec))
  (and (< pos len)
       (<= (- pos) len))))

;; elem at pos inside vec with lenFunc, or eval of fun, if out of range
(define (at-ifAbsent vec pos (lenFunc length) (ifAbsentFuncOrNil nil))
  (if (in-range vec pos lenFunc)
      (vec pos)
      (if ifAbsentFuncOrNil (ifAbsentFuncOrNil))))


;;;; code creation

(define (create-dolist symbol listExpr breakExprOrNil body)
  ;(dbg:expr symbol listExpr breakExprOrNil body)
  (list dolist
        (list symbol
              listExpr
              breakExprOrNil)
        body))


;;;; patches

(define (patch-by-source aSymbol oldStr newStr verboseFlag , src)
  (and verboseFlag (println "patch-by-source sym arg: " aSymbol))
  (setq src (string (source aSymbol)))
  (and verboseFlag (print "before replace: " src))
  (replace oldStr src newStr)
  (and verboseFlag (print "after replace: " src))
  (eval-string src))

(define (test_patch-by-source)
  (patch-by-source 'getopts:usage "Usage: " "UUUsage: " true))


;;;; introspection
(define (in-context? ctx aSym)
  (= (prefix aSym) ctx))
(define (calls-into-context funcSym (ctx (prefix funcSym))) ;'ref-parent
  (assert:pre (Util:lambda-or-macro-symbol? funcSym))
  (letn ((func (eval funcSym))
         (symInContextPred (curry in-context? ctx))
         (symPred (fn (s)
                    ;;(dbg:expr s)
                    (and (symInContextPred s) (Util:lambda-or-macro-symbol? s))))
         (refPred (fn (r) (= (last r) 0))) ; func call pos?
         )
    (unique
     (map (fn (r) (nth r func))
          (filter
           refPred
           (ref-all nil func (fn (ignore s) (and (symbol? s) (symPred s)))))))))


;; from manual (unique added)
(define (all-contexts)
  (unique (filter context? (map eval (symbols MAIN)))))

(define (prefix-term-strings s
                             , symStr
                               parsedStr prefixCtx
                               prefixStr termStr)
  (set 'symStr (string s))
  ;;(dbg:expr s symStr)
  (if (find ":" symStr)
      (set 'parsedStr (parse symStr ":")
           'prefixStr (first parsedStr)
           'termStr (last parsedStr))
      (set 'prefixCtx (prefix s)
           'prefixStr (string prefixCtx) ; always in MAIN
           'termStr symStr))
  (cons prefixStr termStr))
(define (term-string)
  ((prefix-term-string s) 1))
(define (sym-all strOrSym)
  (map (curry sym strOrSym)
       (filter (curry Util:sym? strOrSym) (all-contexts))))
(define (symbols-all)
  (flat (map symbols (all-contexts))))
(define (irregular-symbol s)
  (and (or (find ":" (string (prefix s)))
           (find ":" (term s)))
       (!= (term s) ":")))
;;
(define (sym-all-dbg-expr strOrSym)
  (dolist (s (sym-all strOrSym))
          (dbg:expr s (eval s))))
;;

(define (append-postfixed-syms postfixStr symbolList)
  (append symbolList
          (add-postfix-to-syms postfixStr symbolList)))

(define (curry-at aList pos)
  (let (res (fn ($x)))
    (push (push '$x aList pos) res -1)))
(define (curry-between listBefore (listAfter '()))
  (let (res (fn ($x)))
    (push (extend listBefore (push '$x listAfter)) res -1)))
;; ((Util:curry-between '(: logg:expr) '("foo" "bar")) logg:cerr)
;; (apply (Util:curry-between '(: logg:expr) '("foo" "bar")) (list logg:cerr))
;; (map (Util:curry-between '(: logg:expr) '("foo" "bar")) (list logg:cerr))

(define (callable? symbol)
  (let (val (eval symbol))
    (or (macro? val)
        (lambda? val)
        (primitive? val))))
(define (callable-sym? symbol ctx)
  (and (Util:sym? symbol ctx)
       (callable? symbol)))

;; to be refactored: comparePred should cover '='
(define (sorted-keep-seq l accessFunc comparePred)
  (sort (copy l) ; unsorted l used below
        (fn (x y)
          (let (xVal (accessFunc x) yVal (accessFunc y))
            (if (= xVal yVal)
                (< (find x l) (find y l))
                (comparePred xVal yVal))))))


(context MAIN)

(dbg:end "util.lsp")
