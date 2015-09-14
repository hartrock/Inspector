(context 'Util)

(define (sym? strOrSym ctx)
  (sym strOrSym ctx nil))
(define (default? ctx)
  (sym? (term ctx) ctx))

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

;; from manual (unique added)
(define (all-contexts)
  (unique (filter context? (map eval (symbols MAIN)))))
(define (symbols-all)
  (flat (map symbols (all-contexts))))


(define (lambda-or-macro-symbol? aSym)
  (and (symbol? aSym)
       (or (lambda? (eval aSym))
           (macro? (eval aSym)))))

(define (sym-string s , symStr)
  (set 'symStr (string s))
  (if (find ":" symStr)
      (letn ((prefixCtx (prefix s))
             (parsedStr (parse symStr ":"))
             (prefixStr (first parsedStr))
             (termStr (last parsedStr))
             (prefixCtxStr (string prefixCtx)))
        (if (!= prefixStr prefixCtxStr)
            (append symStr " [" prefixStr ":] " prefixCtxStr ":" termStr)
            symStr))
      (string (prefix s) ":" (term s))))

(define (add-prefix-to-sym prefixStr symbol)
  (sym (append prefixStr (term symbol))
       (prefix symbol))) ; get correct ctx prefix
(define (add-postfix-to-sym postfixStr symbol)
  (sym (append (term symbol) postfixStr)
       (prefix symbol))) ; get correct ctx prefix
(define (add-prefix-to-syms prefixStr symList)
  (map (curry add-prefix-to-sym prefixStr) symList))
(define (add-postfix-to-syms postfixStr symList)
  (map (curry add-postfix-to-sym postfixStr) symList))

(define (swap-symbols symList_1 symList_2)
  (map (fn (s_1 s_2) (swap (eval s_1) (eval s_2)))
       symList_1 symList_2))

;; These functions are an intermediate between
;; - (new srcCtx dstCtx) : not overwriting the vals of existing syms in dstCtx;
;; and
;; - (new srcCtx dstCtx true) : overwriting the val of existing syms in dstCtx.
;; They overwrite the vals of existing syms in dstCtx, but only then, if they
;; are:
;; 1. Variant: *not* nil in srcCtx (overwrite in case of non-nil conflicts).
;; 2. Variant: nil in dstCtx (*no* overwrite in case of non-nil conflicts).
;; Motivation:
;; - There may be nil syms in srcCtx just by referencing syms expected to be in
;;   dstCtx, which *should* *not* be overwritten in dstCtx.
;; - There may be nil syms in dstCtx by referencing syms expected to be in
;;   srcCtx, which *should* be overwritten.
;; Notes:
;; - *non*-existent syms in dstCtx will be created even with nil vals from
;;   srcCtx.
;; - in case of a conflict between not-nil values of a sym in both contexts,
;;   srcCtx losses (1.) or wins (2.).
;;
;; 1. this variant does not overwrite non-nils by non-nils.
;; Note: to be preferred against 2. variant below, if overwritng not needed (for
;; the reason see note there).
(define (mixin-no-overwrite-of-non-nil srcCtx dstCtx)
  (dolist
   (s (symbols srcCtx))
   (if (or (not (sym? s dstCtx))
           (nil? (eval (sym s dstCtx))))
       (def-new s (sym s dstCtx))
       "skip (no overwrite of non-nil vals)")))
;; 2. this variant overwrites non-nils by non-nils.
;; Note: this may overwrite *** just created *** non-nils - by recursively
;; created deps during creation of former symbols.
(define (mixin-no-overwrite-with-nil srcCtx dstCtx)
  (dolist
   (s (symbols srcCtx))
   (if (or (not (sym? s dstCtx))
           (eval s)) ; not nil
       (def-new s (sym s dstCtx))
       "skip (no overwrite with nil vals)")))


(context MAIN)
