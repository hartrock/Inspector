(dbg:begin "tweak.lsp")

(load-libs 'assert)


(context 'tweak)

(setq localExprSyms
      (list (sym "args" MAIN)
            (sym "$args" MAIN)
            (sym "symbols" MAIN)
            (sym "sym" MAIN)
            (sym "context" MAIN)
            ))
(define (localExpr-cmp ignored e)
  (find e localExprSyms))
;;
;; inplace mod of a ref-all'ed list referenced by rSym
;;   http://www.newlisp.org/downloads/newlisp_manual.html#ref-all
(define (sym-mod-ref-all rSym sym-mod-ref expKeyOrNil func-compare reverseFlag)
  (let (numChanges 0 refAllCount 0)
    (let (refAll (if func-compare
                     ;; nil arg is different from no arg here ..
                     (ref-all expKeyOrNil (eval rSym) func-compare)
                     (ref-all expKeyOrNil (eval rSym)))) ; .. no nil arg!
      (if reverseFlag (reverse refAll))
      (let (refAllCount (length refAll) ix 0)
        (dolist
         (r refAll)
         (if (sym-mod-ref rSym r expKeyOrNil ix refAllCount) (++ numChanges))
         (++ ix))))
    numChanges))

;; ;; ;; ;;

;; 
(define (create-path-string p)
  (join (map string p) "_"))
;;
;; Replace context sensitive expressions by variables inited inside correct
;; context. Unique var names by utilizing refs to expressions they stand for.
(define-macro (local-expr-into-var)
  (let ((argsRefs (ref-all nil exprList localExpr-cmp))
        (changeCount 0))
    (if (not (null? argsRefs))
        (local (l_args symbol symStartsWith_$ argsName argsExprStore)
          (dolist
           (e argsRefs)
           (setq l_args (if (and (> (length e) 1)
                                 (= (last e) 0))
                            (exprList (0 -1 e)))
                 symbol (exprList e)
                 symStartsWith_$ (= (first (string symbol)) "$"))
           (if (or (list? l_args) symStartsWith_$)
               (begin ; l_args: (sym ...), ($sym ...) or nil
                 ;;(println "l_args: " l_args ", symbol: " symbol)
                 (setq argsName
                       (sym (append "local_" (string symbol)
                                    "_" (create-path-string e))))
                 ;;(println "argsName: " argsName)
                 (setq (exprList
                        (if symStartsWith_$
                            e ; replace sym
                            (0 -1 e))) ; replace func call list with sym
                       argsName)
                 (push (list argsName (if symStartsWith_$ symbol l_args))
                       argsExprStore -1)
                 (++ changeCount)))
           ) ; dolist
          (when (list? argsExprStore)
            ;;(println "1. exprList: " exprList)
            (setq exprList
                  (push exprList ; back: call assert with local vars after ..
                        (push 'let ; .. front: initing them before.
                              (list (flat argsExprStore 1)))
                        -1))
            ;;(println "2. exprList: " exprList)
            ;;(println "3. MAIN:h: " MAIN:h)
            ;;(debug (MAIN:d_foo))
              )))
    (> changeCount 0)))
;;
;; Add suffix to exprSym and put (#call callerSym) at begin of exprList.
(define-macro (add-location-info)
  (letn ((exprSym (exprList 0))
         ;;(newExprSym (sym (append (term exprSym) "_tweaked") (prefix exprSym))))
         (prefixTerm (Util:prefix-term-strings exprSym))
         (newExprSym (read-expr (append (prefixTerm 0) ":"
                                        (prefixTerm 1) "_tweaked"))))
    ;;(println "exprSym: " exprSym ", newExprSym: " newExprSym)
    (setq (nth 0 exprList) newExprSym)
    ;; don't put callerSym at func call pos
    (push
     (cons (- refAllCount ix) callerSym) ; don't put callerSym at func call pos
     exprList 1)
    ;; changedCalleeSyms taken from :context-symsFromCtx below
    (push exprSym changedCalleeSyms -1))
  true) ; has changed
;;
(define (call-mod-ref macroSym callerSym symRef expKeyOrNil ix refAllCount)
  (if (= (last symRef) 0) ; func call pos?
      (letn ((lRef (0 -1 symRef)) ; one level up of expr sym
             (exprList (nth lRef (eval callerSym))) ; nth avoids func call
             (hasChanged ((eval macroSym))))
        (if hasChanged
            (setq (nth lRef (eval callerSym)) exprList))
        hasChanged)))
(define (call-mod-ref-loc callerSym symRef expKeyOrNil ix refAllCount)
  (call-mod-ref 'add-location-info callerSym symRef nil ix refAllCount))
(define (call-mod-ref-expr callerSym symRef)
  (call-mod-ref 'local-expr-into-var callerSym symRef))
;;
(define (inform-about-changes locStr numChanges calleeNames callerSym)
  (if (not (zero? numChanges))
      (dbg:msg-loc locStr
                   (format
                    "%2d %s call%s modified in: %s" 
                    numChanges
                    (string calleeNames)
                    (if (!= numChanges 1) "s" " ")
                    (string callerSym)))))
;;
(define (arguments rSym expKeyOrNil expKeyCompareOrNil)
  (let (numChangesLoc 0 numChangesExpr 0 locIndex 0)
    (if (or (lambda? (eval rSym)) (macro? (eval rSym)))
        (begin
          (setq numChangesExpr
                (sym-mod-ref-all rSym call-mod-ref-expr
                                 expKeyOrNil expKeyCompareOrNil true))
          (setq locIndex numChangesExpr) ; for counting backwards above
          (setq numChangesLoc
                (sym-mod-ref-all rSym call-mod-ref-loc
                                 expKeyOrNil expKeyCompareOrNil true))))
    numChangesLoc))

;; ;; ;;

(define (count-sym-symsFromCtx rSym sfc)
  (arguments rSym nil       (eval (sym "syms-cmp" sfc))))
;;(arguments rSym 'dbg:expr nil)) ; variant for one symbol

(define (context-symsFromCtx ctx symsFromCtx)
  (dbg:begin (format "(context %s, symsFromCtx %s)"
                     (string ctx) (string symsFromCtx)))
  (local (numChanges numAllChanges_assert numAllChanges changedCalleeSyms)
    (set 'changedCalleeSyms '())
    (dolist (s (symbols ctx))
            (setq numChanges (count-sym-symsFromCtx s symsFromCtx))
            (++ numAllChanges numChanges))
    (inform-about-changes
     'context numAllChanges (unique changedCalleeSyms) ctx))
  (dbg:end (format "(context %s, symsFromCtx %s)"
                   (string ctx) (string symsFromCtx))))
(define (context-symsFromContexts ctx symsFromContexts)
  (dolist (sfc symsFromContexts)
          (context-symsFromCtx ctx sfc)))


;;
;; iface: tweak assert's and dbg:expr's in contexts
;;

(define (tweak:context ctx (symsFromContexts '(assert dbg)))
  (context-symsFromContexts ctx symsFromContexts)
  (push ctx tweaked -1))

(define (tweak:context? ctx)
  (and tweaked (find ctx tweaked)))


;; init

(define (initialize)
  (dbg:begin 'initialize)
  (dbg:msg-loc 'initialize
                "nothing to tweak") ; (tweak:context MAIN:tweak)
  (dbg:end 'initialize))


(MAIN:context MAIN)

(dbg:end "tweak.lsp")
