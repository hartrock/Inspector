(context 'FOOPReference)

;; indices of elems in FOOP list
(constant 's_class 0 's_ref 1)
;; helpers
(define (ref-context-sym ix)
  (sym (string (context) "_" ix) MAIN))
(define (new-ref-context-sym)
  (ref-context-sym (++ foopCount))) ; foopCount for identifying FR instances
(define (new-ref-context)
  (let (ref_contextSym (new-ref-context-sym))
    (prefix (sym ref_contextSym ref_contextSym)))); without switching to new ctx
;; standard functor: each call increases foopCount
(define (FOOPReference:FOOPReference)
  (letn ((ref_context (new-ref-context)) ; increments foopCount
         (foop (cons (context) (cons ref_context (args)))))
    (set (sym (string ref_context) ref_context) foop) ; set ref context default
    ref_context))
;; accessors
(define (class)     ; FOOP Class
  (self s_class))
(define (reference) ; FOOP reference context
  (self s_ref))
;; cleaners
(define (delete-ref ctxSym)
  (delete ctxSym)  ; syms in context including foop default
  (delete ctxSym)) ; context in MAIN
(define (delete-ref-ix ix)
  (delete-ref (ref-context-sym ix)))
(define (delete-all-refs) ; robust against missing refs/foops already deleted
  (while (> foopCount 0)
    (delete-ref-ix foopCount)
    (-- foopCount)))

(context MAIN)
