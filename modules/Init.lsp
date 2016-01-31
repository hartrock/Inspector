;; Standard init for scripts using libs.
;;

;; only here this way: later in lib files (dbg:begin "something.lsp")
(or Init:nodebug
    (write-line 2 "[Init.lsp..."))


(context 'Init)

(when extendedDone
  (let (msg "[Init.lsp][ERROR] Init:extendedDone: accidentally tried to load twice.")
    (write-line 2 msg)
    (throw-error msg)))


(context MAIN)

(map context '(Util MAIN))

(when (not Init:minimalDone)
  (load (append Init:basedir "/modules/Init.minimal.lsp")))

;;
;; Loggers
;;

(load-module "FOOPReference.lsp")
(load-module "Util.lsp") ; minimal version for module deps
(load-module "Logger.lsp")
(load-module "LoggerTweakable.lsp")

;;
;; default logger

(set 'logg (Logger 2 (fn () "[log]")))
;; OLD: (Util:mixin-no-overwrite-of-non-nil LoggerExpr logg)
(logg:mixin-expr)
(global 'logg)

;;
;; dbg: context

(when (context? dbg) ; avoid deleting dbg sym, but ..
  (delete 'dbg)) ; .. delete dbg context from Init.minimal.lsp

(set 'dbg (Logger 2 (fn () "[dbg]") Logger:level_all))
(dbg:mixin-expr-debug)

;; has to overwrite non-nil syms from LoggerDebug, if put thereafter
(Util:mixin-no-overwrite-with-nil LoggerTweakable dbg)
(global 'dbg) ; avoid creation of context or local var in contexts during load

;; Override 'on' default, but only then, if explicitely said so.
(if Init:nodebug (dbg:off))


;; for Tweak.lsp
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

(load-module "Libs.lsp")


(context Init)

;; Used by MAIN:load-module from Init.minimal.lsp.
(define (load-module-extended name_str overwriteFlag
                              , filePathLoaded)
  (when (set 'filePath_loaded
             (Libs:load-opt-overwrite
              (append Init:moduledir "/" name_str)
              overwriteFlag))
    (push filePath_loaded Init:loadedModules)))


(context MAIN)

;; difficult to use, before scriptname issues are not solved:
;;   (define (load-rel path) (load (append Init:scriptDir "/" path)))

;; load funcs with str args
(define (load~ rel_path_str)
  (load (append (env "HOME") "/" rel_path_str)))
(define (load-once filePath)
  (Libs:load-opt-overwrite filePath))
;; load-module defined by Init.minimal.lsp

;; load lib funcs with sym args
(define (load-libs)
  (Libs:load-libs (args)))
(set 'load-lib load-libs) ; alias

;; mem kind of init
(set 'Init:extendedDone true)

;; only here this way: later in lib files (dbg:end "something.lsp")
(or Init:nodebug
    (write-line 2 "...Init.lsp]"))

;;EOF
