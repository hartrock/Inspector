;; Standard init for scripts using libs.
;;
(when (not Init_allowLoadOnce)
  (let (msg "[Init.lsp][FATAL] Init_allowLoadOnce not set: accidentally tried to load twice?")
    (write-line 2 msg)
    (throw-error msg)))
(delete 'Init_allowLoadOnce ; avoid accidentally loading twice
        nil) ; unsafe delete should be safe here

;;(println "lib/Init.lsp ...")  (sleep 3000)
(map context '(Util Init MAIN))

;; only here this way: later in lib files (dbg:begin "something.lsp")
(or Init:nodebug
    (write-line 2 "[Init.lsp..."))


;;
;; Loggers
;;

(load (append Init:moduledir "/FOOPReference.lsp"))
(load (append Init:moduledir "/Util.lsp")) ; minimal version for module deps
(load (append Init:moduledir "/Logger.lsp"))
(load (append Init:moduledir "/LoggerTweakable.lsp"))

;;
;; default logger

(set 'logg (Logger 2 (fn () "[log]")))
;; OLD: (Util:mixin-no-overwrite-of-non-nil LoggerExpr logg)
(logg:mixin-expr)
(global 'logg)

;;
;; dbg: context

;;(map context '(dbg MAIN)) ; mark dbg sym as context (and global)
(set 'dbg (Logger 2 (fn () "[dbg]") Logger:level_all))
(dbg:mixin-expr-debug)

;; has to overwrite non-nil syms from LoggerDebug, if put thereafter
(Util:mixin-no-overwrite-with-nil LoggerTweakable dbg)
(global 'dbg) ; avoid creation of context or local var in context during load

;; Override 'on' default, but only, if explicitely said so.
;(if Init:nodebug (dbg:off))


;; for tweak.lsp
(context dbg)
(set 'prefixedToBeTweakedStrs (map (curry append "dbg:") toBeTweakedStrs))
     ;;'prefixExprRegex (append "^(?:" (join prefixExprStrs "|") ")$")
     ;;'prefixExprCompiledRegex (regex-comp prefixExprRegex))
(define (syms-cmp ignored e)
  (when nil
    (dbg:expr prefixExprRegex prefixExprCompiledRegex)
    ;;for not existing contexts in exprs like NonCtx:aSym
    (catch (dbg:expr e) 'err))
  (find (string e) prefixedToBeTweakedStrs)) ; not slower as regex...
  ;; *one* last regex will be cached ..
  ;;(regex prefixExprCompiledRegex (string e) 0x10000)) ; .. so just in case
  ;;(regex prefixExprRegex (string e))) ; alt to the former
  ;;(find e exprSyms)) ; OLD: DNW with syms in Logger_* context, which dbg refs
(context MAIN)



(context Init)

;; want to know what to be inited here quite early, but it's good to have a
;; scriptpath...
(or Init:nodebug
    (write-line 2 (append "  -> script path: " (Logger:scriptpath))))

(set 'empty-lambda (fn ())
     'empty-macro (lambda-macro ())) ; does not eval args

;; Very basics.
(setq testContextPrefix "T_")

;; pretty-print honoring terminal column width
(setq tput_cols (exec "tput cols"))
(if tput_cols (pretty-print (- (int (tput_cols 0)) 25)))

(context MAIN)

;;
;; Libs...
;;
(load (append Init:moduledir "/Libs.lsp"))

(context MAIN)

;; difficult to use, before scriptname issues are not solved
;; (define (load-rel path) (load (append Init:scriptDir "/" path)))

;; str args
(define (load~ rel_path_str)
  (load (append (env "HOME") "/" rel_path_str)))
(define (load-module name_str overwriteFlag)
  (Libs:module-opt-overwrite (append Init:moduledir "/" name_str)
                             overwriteFlag))

;; sym args
(define (load-libs)
  (Libs:load-libs (args)))
(set 'load-lib load-libs) ; alias

;; only here this way: later in lib files (dbg:end "something.lsp")
(or Init:nodebug
    (write-line 2 "...Init.lsp]"))
