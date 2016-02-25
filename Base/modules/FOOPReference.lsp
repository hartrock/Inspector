(context MAIN)

(global 'FOOPReference?) ; this predicate should always be available
(define (FOOPReference? ctx)
  (and (context? ctx)
       (Util:sym? "FOOPReference_marker" ctx)))


(context 'FOOPReference)

;;
;; functor
;;

;;
;; indices of elems in FOOP list

(constant 's_class 0 's_ref 1)

;;
;; helpers

(define (ref-context? ix
                      , (sim (Util:sym? (string (context) "_" ix) MAIN))
                        val)
  (and sim
       (FOOPReference? (set 'val (eval sim)))
       val)) ; ctx here
(define (ref-context-sym ix)
  (sym (string (context) "_" ix) MAIN))
(define (new-ref-context-sym)
  (ref-context-sym (++ foopCount))) ; foopCount for identifying FR instances
(define (new-ref-context)
  (let (ref_contextSym (new-ref-context-sym))
    (prefix (sym ref_contextSym ref_contextSym)))) ;without switching to new ctx
;; uses foopCount, to be incremented before by (new-ref-context)
(define (tree-sym nameStr)
    (sym (string (context) "_" foopCount "_" nameStr) MAIN))
(define (new-tree nameStr)
  (new Tree (tree-sym nameStr)))

;; for reuse from mixin FOOPs
(define (new-FOOPReference)
  (letn ((ref_context (new-ref-context)) ; increments foopCount
         (foop (cons (context) (cons ref_context (args)))))
    (sym "FOOPReference_marker" ref_context) ; mark
    (set (sym (string ref_context) ref_context) foop) ; set ref context default
    ref_context))

;;
;; standard functor:
;; - each call increases foopCount
(set 'FOOPReference:FOOPReference new-FOOPReference)

;;
;; accessors
;;

(define (class)     ; FOOP Class
  (self s_class))
(define (reference) ; FOOP reference context
  (self s_ref))
;; var access from FOOP self to FOOP reference context vars
(define (get-ref-var termStr)
  (eval (sym termStr (reference))))
(define (set-ref-var termStr arg)
  (set (sym termStr (reference)) arg))

;;
;; cleaners
;;

;;
;; helpers

(define (context-sym ctx)
  (sym (string ctx) MAIN))
(define (delete-ref-sym ctxSym)
  (delete ctxSym)  ; syms in context including foop default
  (delete ctxSym)) ; context in MAIN
(define (delete-context ctx)
  (delete-ref-sym (context-sym ctx)))

;;
;; FOOP self instance delete

(define (delete-FOOPReference)
  (delete-context (reference)))
;; standard delete:
;; - to be overwritten by mixin FOOPs creating additional contexts
(define (delete-ref)
  (delete-FOOPReference))

;;
;; FOOP decent instance delete (of indexed one or all instances)
;; - robust against trying to delete already deleted instances
(define (delete-ref-ix ix
                       , (rc (ref-context? ix)))
  (when rc
    (:delete-ref rc)))
(define (delete-all-refs) ; robust against missing refs/foops already deleted
  (while (> foopCount 0)
    (delete-ref-ix foopCount)
    (-- foopCount)))
;;EOF
