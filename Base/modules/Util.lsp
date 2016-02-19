(context 'Util)

;;;; pos in vec
;;todo: think about using expansion macro
(define (in-range vec pos (lenFunc length))
  (let (len (lenFunc vec))
  (and (< pos len)         ; check positive offset (negative always true)
       (<= (- pos) len)))) ; check negative offset (positive always true)

;; elem at pos inside vec with lenFunc, or eval of fun, if out of range
(define (at-ifAbsent vec pos ifAbsentFuncOrNil (lenFunc length))
  (if (in-range vec pos lenFunc)
      (vec pos)
      (if ifAbsentFuncOrNil (ifAbsentFuncOrNil))))


;; Pause by loop (consuming CPU time) without using signals like sleep: shift
;; param 30 gives about 10s for some host. For debugging purposes.
(define (pause-by-loop shiftOne)
  (for (ix 1 (<< 1 shiftOne))))


(define (sym? strOrSym ctx)
  (sym strOrSym ctx nil))
(define (default? ctx)
  (sym? (term ctx) ctx))

;; newlisp.h
;;
;; typedef struct tagSYMBOL
;;         {
;;         int flags;
;;         int color;
;;         char * name;
;;         UINT contents; 
;;         struct tagSYMBOL * context;
;;         struct tagSYMBOL * parent;
;;         struct tagSYMBOL * left;
;;         struct tagSYMBOL * right;   
;;         } SYMBOL;

;; typedef struct 
;;         {
;;         UINT type;
;;         void * next;
;;         UINT aux;
;;         UINT contents;
;;         } CELL;

;; CELL indices
(constant 'cix_type 0 'cix_next 1 'cix_aux 2 'cix_contents 3)
(struct 's_CELL_pp
        "unsigned int"  ; 0
        "void*"         ; 1
        "void*"         ; 2 ; "unsigned int" -> p
        "void*")        ; 3 ; "unsigned int" -> p
(struct 's_CELL_up
        "unsigned int"  ; 0
        "void*"         ; 1
        "unsigned int"  ; 2
        "void*")        ; 3 ; "unsigned int" -> p
(struct 's_CELL_uu
        "unsigned int"  ; 0
        "void*"         ; 1
        "unsigned int"  ; 2
        "unsigned int") ; 3

;; SYMBOL indices
(constant 'six_flags 0 'six_color 1 'six_name 2 'six_contents 3 'six_context 4 'six_parent 5 'six_left 6 'six_right 7)
(struct 's_SYMBOL_p
        "int" "int" "char*"
        "void*" ; "unsigned int" -> p
        "void*" "void*" "void*" "void*")

;; dump indices
(constant 'dix_address 0 'dix_type 1 'dix_next 2 'dix_aux 3 'dix_contents 4)

(constant 'EVAL_SELF_TYPE_MASK 0x0100
          'SYMBOL_TYPE_MASK    0x0040
          'CELL_CONTEXT        6
          'CELL_NIL            (|  0 EVAL_SELF_TYPE_MASK)
          'CELL_SYMBOL         (|  5 SYMBOL_TYPE_MASK)
          'CELL_DYN_SYMBOL     (| 15 SYMBOL_TYPE_MASK)
          'nilSymbol           (address nil))

#define isNil(A) ((A)->type == CELL_NIL || ((A)->type == CELL_SYMBOL && (A)->contents == (UINT)nilSymbol))
(define (nilCell? u_cell) ; u_cell: unpacked cell
  (or
   (= (u_cell cix_type) CELL_NIL)
   (and
    (= (u_cell cix_type) CELL_SYMBOL)
    (= (u_cell cix_contents) nilSymbol))))
(define (contextCell? u_cell)
  (= (u_cell cix_type) CELL_CONTEXT))
(define (dynamic-symbol? sy)
  (= ((dump sy) dix_type)
     CELL_DYN_SYMBOL))
(define (symbol-like?_debug sy , res)
  (set 'res (or (dynamic-symbol? sy)
                (symbol? sy)))
  ;; (when res
  ;;   (dbg:expr sy))
  res)
(define (symbol-like? sy)
  (or (dynamic-symbol? sy)
      (symbol? sy)))
(define (non-dynamic-symbol? sy)
  (and (not (dynamic-symbol? sy))
       (symbol? sy)))

;; Minimal invasive.
(define (dynamic-prefix-term-target-strings sy)
  (when (dynamic-symbol? sy)
    (letn ((sy_dump (dump sy))
           ;; cell -> c_contents
           (target_term
            (get-string (sy_dump dix_contents)))
           ;; cell
           (sy_cell
            (unpack s_CELL_pp (first sy_dump)))
           ;;(target_term alt
           ;; (get-string (sy_cell cix_contents)))
           ;; cell -> aux
           (var_sym
            (and (not (nilCell? sy_cell))
                 (unpack s_SYMBOL_p (sy_cell cix_aux))))
           ;; cell -> aux -> name
           (var_term
            (and var_sym
                 (var_sym six_name)))
           ;; cell -> aux -> context
           (var_context
            (and var_sym
                 (unpack s_SYMBOL_p (var_sym six_context))))
           ;; cell -> aux -> context -> name
           (var_prefixStr
            (and var_context
                 (var_context six_name))))
           ;; minimal invasive: create context info by using previous results
           ;; ;; cell -> aux -> s_contents
           ;; (target_context_cell
           ;;  (unpack s_CELL_pp (var_sym six_contents)))
           ;; ;; cell -> aux -> s_contents -> c_contents
           ;; (target_context_sym
           ;;  ;;(and (not (nilCell? target_context_cell))
           ;;  (and (contextCell? target_context_cell)
           ;;       (unpack s_SYMBOL_p (target_context_cell cix_contents))))
           ;; ;; cell -> aux -> s_contents -> c_contents -> name
           ;; (target_prefixStr
           ;;  (and target_context_sym
           ;;       (target_context_sym six_name))))
      ;;(dbg:expr-sep sy_cell (unpack s_CELL_uu (first sy_dump)) var_sym)
      (list var_prefixStr var_term target_term))))
(define (sym-from-prefix-term-strings prefixStr termStr)
  (sym termStr ; term of ..
       (sym prefixStr ; .. some ctx ..
            MAIN)))
(define (dynamic-var-target-symbols sy
                                    , dptt)
  (set 'dptt (dynamic-prefix-term-target-strings sy)
       'varSym (sym-from-prefix-term-strings (dptt 0) (dptt 1))
       'varCtx (eval varSym)
       'targetSym (if (context? varCtx)
                      (sym (dptt 2) ; target term
                           varCtx)))
  (cons varSym targetSym))
(define (dynamic-prefix-term-strings sy
                                     , (dptt
                                        (dynamic-prefix-term-target-strings sy))
                                       (var_prefixStr (dptt 0))
                                       (var_term (dptt 1))
                                       (target (dptt 2))
                                       varSym targetCtxOrNot)
  (set 'varSym (sym var_term ; term of ..
                    (sym var_prefixStr ; .. some ctx ..
                         MAIN)) ; .. in MAIN
       'targetCtxOrNot (eval varSym)) ; target ctx holder
  ;;(dbg:expr dptt varSym)
  (list (cons var_prefixStr var_term)
        (cons (if (context? targetCtxOrNot)
                  (string targetCtxOrNot))
              target)))

(define (dynamic-var-symbol dsy)
  (first (dynamic-var-target-symbols sy)))
(define (dynamic-target-symbol dsy)
  (last (dynamic-var-target-symbols dsy)))
(define (dynamic-var-context dsy
                             , (varSym (dynamic-var-symbol dsy)))
  (if varSym
      (prefix varSym)))
(define (dynamic-target-context dsy
                                , (targetSym (dynamic-target-symbol dsy)))
  (if targetSym
      (prefix targetSym)))


;; Prefix string and term (string) for direct symbols; not suited for indirect
;; (prefix var) symbols.
;; Takes first ":" as ctx:term separator; also handles special case 'MAIN:: .
(define (prefix-term-strings sy
                             , (symStr (string sy))
                               prefixStr termStr)
  ;;(dbg:expr-loc 'prefix-term-strings (string sy))
  (let (sepPos (find ":" symStr))
    (if (> sepPos 0) ; honors ":" in MAIN
        (set 'prefixStr (0 sepPos symStr)
             'termStr  ((++ sepPos) symStr))
        (if (= (prefix sy) MAIN) ; sy should be a global in MAIN
            (set 'prefixStr "MAIN"
                 'termStr symStr)
            (= (prefix sy) (context))
            (set 'prefixStr (string (context)) ; (context) -> Util
                 'termStr symStr)
            (throw-error "unexpected")))) ; mental model wrong
  (cons prefixStr termStr))
;;
(define (term-string s)
  ((prefix-term-strings s) 1))

;; *indirect* var 'foo evaluating to some ctx without sym 'bar:
;; a)
;;   1. (symbol? 'foo:bar) -> false, but
;;      (global? 'foo:bar) -> true ('foo evaluating to ctx)
;;   2. (prefix 'foo:bar) -> ctx -> and *creates* sym 'bar in ctx
;;   => 3. (symbol? 'foo:bar) -> true
;; b) (term 'foo:bar) -> always fails with ERR
;;

;; Usage of dynamic symbols introspection gives accurate info about var sym and
;; its context.
(define (direct-or-dynamic-var-symbol sy)
  (if (dynamic-symbol? sy)
      (dynamic-var-symbol sy)
      sy))
  
;; - without ':': MAIN context assumed
;; - does not create symbol, if not already there
(define (symbol-string? str
                        , (sepPos (find ":" str))
                          prefixStr termStr)
  (if (> sepPos 0) ; honors ":" in MAIN
      (set 'prefixStr (0 sepPos str)
           'termStr  ((++ sepPos) str))
      (set 'prefixStr "MAIN"
           'termStr str))
  (if (sym? prefixStr MAIN)
      (let (ctx (eval (sym prefixStr MAIN)))
        (sym? termStr ctx))))


;; from manual (unique added)
(define (all-contexts)
  (unique (filter context? (map eval (symbols MAIN)))))
(define (symbols-all)
  (flat (map symbols (all-contexts))))


(define (lambda-or-macro-symbol? aSym)
  (and (symbol? aSym)
       (or (lambda? (eval aSym))
           (macro? (eval aSym)))))

;; For standard symbols: has limitations for dynamic syms.
(define (standard-sym-string sy , symStr)
  (set 'symStr (string sy))
  (if (find ":" symStr)
      (letn ((prefixCtx (prefix sy))
             ;;(parsedStr (parse (dbg:expr-loc 'sym-string-old symStr) ":"))
             (parsedStr (parse symStr) ":")
             (prefixStr (first parsedStr))
             (termStr (last parsedStr))
             (prefixCtxStr (string prefixCtx)))
        (if (!= prefixStr prefixCtxStr)
            (begin
              (append
               symStr " [" prefixStr ":] " prefixCtxStr ":" termStr
               " (unexpected code reach: should be handled by sym-string)"))
            symStr))
      (string (prefix sy) ":" (term sy))))

;; General method for
;; - dynamic symbols: usage of dynamic symbols introspection by looking into
;;   interpreter internals gives accurate info about context var symbol;
;; - standard symbols: use less advanced method above.
(define (sym-string sy)
  (if (dynamic-symbol? sy)
      (letn ((dpts (dynamic-prefix-term-strings sy))
             (var (dpts 0))
             (target (dpts 1))
             (prefix_prefixStr (first var))
             (prefix_term (last var))
             (target_prefixStr (first target))
             (target_term (last target)))
        (append "(" prefix_prefixStr ":)" prefix_term
                ":"
                "(" (string target_prefixStr) ":)" target_term))
      (standard-sym-string sy)))

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
(define (mixin-no-overwrite-of-non-nil srcCtx dstCtx
                                       , defCount skipCount)
  (dolist
   (s (symbols srcCtx))
   (if (or (not (sym? s dstCtx))
           (nil? (eval (sym s dstCtx))))
       (begin
         (++ defCount)
         (def-new s (sym s dstCtx)))
       (-- skipCount)))
  (cons defCount skipCount))
;; manual debugging, because this will be used for *creating* dbg: ctx
(define (dbg_mixin-no-overwrite-of-non-nil srcCtx dstCtx
                                           , defCount skipCount)
  (write-line 2 (string "src: " srcCtx ", dst: " dstCtx))
  (dolist
   (s (symbols srcCtx))
   (if (or (not (sym? s dstCtx))
           (nil? (eval (sym s dstCtx))))
       (begin
         (write-line 2 (string "def-new (no overwrite of non-nil vals (of "
                               (sym s dstCtx)
                               "))"))
         (++ defCount)
	 (def-new s (sym s dstCtx)))
       (begin
         (write-line 2 (string "skip (no overwrite of non-nil vals (like "
                               (sym s dstCtx)
                               "))"))
	 (-- skipCount))))
  (cons defCount skipCount))
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

(define (h_string-to-sym ss ctx)
  (letn ((v (parse ss ":"))
         (p (when (= (length v) 2) (v 0)))
         (t (v -1)))
    (sym t (if p (context (sym p MAIN)) ctx))))
(when (not loadedFlag)
  ;; emacro needed to get caller (context)
  (macro (string-to-sym Str)
    (h_string-to-sym Str (context))))

(set 'loadedFlag true) ; avoid loading emacro twice
;;EOF
