(when (not (context? FOOPReference))
  (write-line 2 "[FATAL] Logger needs module FOOPReference.lsp.")
  (exit 1))
(when (not (context? Util))
  (write-line 2 "[FATAL] Logger needs module Util.lsp.")
  (exit 1))

(when (context? Logger)
  (write-line 2 "[Warning] Context Logger already defined."))


(new FOOPReference 'Logger)

(context Logger)

;;
;; script properties
;; - should probably become part of getopts or an own module
;;

;; *** old basename (now scriptname) too limited ***
;;
;;;; works for both newLisp and #!/.../newlisp
;;(define (basename)
;;  (setq execPath (or (main-args 1) (main-args 0)))
;;  (last (parse execPath "/")))

;;
;; A (scriptpath), (scriptname), (scriptargs) solution for skipping newlisp opts
;; and their args: could be a helper for getopts.
;;
;; Should be correct for typical shebang (#!/...) cases, but of interest here
;; are newlisp calls like:
;;   newlisp -s 4096 -m 10 someScript.lsp
;; .
;;
;; But it has limitations: it is only correkt, if *first* non-option arg of
;; newlisp is the script of interest.
;; E.g. calling
;;   newlisp -m 10 nonExistentFile
;; results into
;;   > (Logger:scriptname)
;;   "nonExistentFile"
;; .
;; Therefrom it should be allowed and documented how to override; this can be
;; done by setting scriptpath_ix explicitely, in case of used heuristics fails.
;;
;; See file:///usr/share/doc/newlisp/newlisp_manual.html#options:
;;
;;  -h this help                   -> OK (enters interpreter)
;;  -n no init.lsp (must be first) -> OK
;;  -x <source> <target> link      -> error: should not been reached by script
;;  -v version                     -> OK (enters interpreter)
;;  -s <stacksize>                 -> OK
;;  -m <max-mem-MB> cell memory    -> OK
;;  -e <quoted lisp expression>    -> OK (enters interpreter)
;;  -l <path-file> log connections -> OK
;;  -L <path-file> log all         -> OK
;;  -w <working dir>               -> OK
;;  -c no prompts, HTTP            -> OK
;;  -C force prompts               -> OK
;;  -t <usec-server-timeout>       -> OK
;;  -p <port-no>                   -> OK
;;  -d <port-no> demon mode        -> OK
;;  -http only                     -> OK
;;  -6 IPv6 mode                   -> OK
;;
(set'opt_without_arg
 '("-h" ; enters interpreter
   "-n" ; -> skipped
   "-v" ; enters interpreter
   "-c" ; -> skipped
   "-C" ; -> skipped
   "-http" ; -> skipped
   "-6" ; -> skipped
   )
 'opt_with_arg
 '("-s" ; -> skipped
   "-e" ; enters interpreter
   "-m" ; -> skipped
   "-l" ; -> skipped
   "-L" ; -> skipped
   "-w" ; -> skipped
   "-t" ; -> skipped
   "-p" ; -> skipped
   "-d" ; -> skipped
   )
 'opt_with_2_args
 '("-x" ; should not been reached by script
   ;;"-y" ; for testing errorcase...
   ))
(local (breakFlag skip_next ix execPath)
  (set 'ix 0) ; without any args ix 0 refers to newlisp bin
  (dolist
   (o (1 (main-args)) breakFlag) ; without args, there is no loop here
   (cond
    (skip_next
     (++ ix)
     (set 'skip_next nil)) ; skip once
    ((find o opt_without_arg)
     (++ ix))
    ((find o opt_with_arg)
     (++ ix)
     (set 'skip_next true))
    ((find "/(modules|lib)/.+\\.lsp$" o 0) ; skip modules/ and lib/ *.lsp ..
     (++ ix))                              ; .. they cannot be a scriptname
    ((find o opt_with_2_args)
     (throw-error "should not been reached"))
    ("default" ; end loop: first newlisp noopt should be script
     (++ ix) ; needed: loop started with ix of previous element
     (set 'breakFlag true))))
  ;; scriptpath_ix becomes ix of first extra arg not being:
  ;; - a newlisp option with its args, or
  ;; - module/ or lib/ *.lsp;
  ;; if there is no such extra arg, take ix 0 of newlisp bin name.
  (set 'scriptpath_ix (if breakFlag ; extra arg?
                          ix ; ix of first extra arg
                          0) ; ix of newlisp bin name
       'scriptargs_ (if breakFlag
                        ((+ 1 scriptpath_ix) (main-args))
                        '())
       'scriptpath_ (main-args scriptpath_ix)
       'scriptname_ (last (parse scriptpath_ "/"))))
;; iface
(define (scriptpath-ix)
  scriptpath_ix)
(define (scriptargs) ; good as getopts arg
  scriptargs_)
(define (scriptpath)
  scriptpath_)
(define (scriptname) ; Linux (to be extended for other OSes)
  scriptname_)
(define (shebang?) ; works for Linux; to be extended for other OSes
  (and (= (main-args 0) "/usr/local/bin/newlisp")
       (!= (scriptname) "newlisp")))

;;
;; .. script properties
;;


;; helper
;;
(define (write-string str)
  (write-line (fd) str))
(define (prefix-loc-string (locStrOrSym "") (extraPrefix ""))
  (format
   (if (null? locStrOrSym)
       "%s[%s%s]%s "
       "%s[%s %s]%s ")
   (preprefix-string) (scriptname) (string locStrOrSym) extraPrefix))
(define (prefix-string (extraPrefix ""))
  (prefix-loc-string "" extraPrefix))
(define (to-string arguments)
  ;;(println "arguments: " arguments)
  (if (null? arguments)
      "? (no msg)"
      (apply string arguments)))
(define (msg-format arguments)
  (write-string (to-string arguments)))
(constant 'c_fatalStr   "[FATAL]"
          'c_errorStr   "[ERROR]"
          'c_warningStr "[Warning]"
          'c_infoStr    "[Info]")

;; iface
;;
;; (msg arg [arg [...]]) : like args for println
(define (msg)
  (when (<= (level) level_debug)
    (write-string (append (prefix-string)
                          (to-string (args))))))
(define (msg-loc locStrOrSym)
  (when (<= (level) level_debug)
    (write-string (append (prefix-loc-string locStrOrSym)
                          (to-string (args))))))
(define (info)
  (when (<= (level) level_info)
    (write-string (append (prefix-string c_infoStr)
                          (to-string (args))))))
(define (info-loc locStrOrSym)
  (when (<= (level) level_info)
    (write-string (append (prefix-loc-string locStrOrSym c_infoStr)
                          (to-string (args))))))
(define (warn)
  (when (<= (level) level_warn)
    (write-string (append (prefix-string c_warningStr)
                          (to-string (args))))))
(define (h_warn-loc-string locStrOrSym arguments)
  (append (prefix-loc-string locStrOrSym c_warningStr)
          (to-string arguments)))
(define (warn-loc locStrOrSym)
  (when (<= (level) level_warn)
    (write-string (h_warn-loc-string locStrOrSym (args)))))
(define (error)
  (when (<= (level) level_error)
    (write-string (append (prefix-string c_errorStr)
                          (to-string (args))))))
(define (error-loc locStrOrSym)
  (when (<= (level) level_error)
    (write-string (append (prefix-loc-string locStrOrSym c_errorStr)
                          (to-string (args))))))
(define (fatal)
  (when (<= (level) level_fatal)
    (write-string (append (prefix-string c_fatalStr)
                          (to-string (args))))))
(define (fatal-loc locStrOrSym)
  (when (<= (level) level_fatal)
    (write-string (append (prefix-loc-string locStrOrSym c_fatalStr)
                          (to-string (args))))))


;; helper
;;
(constant 'indentIncrement 2)
(define (indent-string)
  (let (str "")
    (dotimes (n (indent)) ; uses foop indent
             (extend str " "))
    str))


;;
;; iface
;;

;; overload global begin: this is *dangerous* for all contexts ..
(define (Logger:begin (what "")) ; .. getting syms from here!
  (when (<= (level) level_debug)
    (write-string (append
                   (prefix-string)
                   (indent-string)
                   "(" (string what) "..."))
    (++ (self s_indent) indentIncrement)))

(define (end (what ""))
  (when (<= (level) level_debug)
    (-- (self s_indent) indentIncrement)
    (write-string (append
                   (prefix-string)
                   (indent-string)
                   "..." (string what) ")"))))

;; macro to show expr unevaluated; returns evaluation
(define-macro (wrap expr (what (string expr)))
  (begin what) ; Logger:begin
  (let (res (eval expr))
    (end what)
    res))

(define (convenience-forward FunSym ctx)
  (let (Ctx (sym (string ctx) MAIN)) ; alt (slower): use (context) ..
    (set (sym FunSym ctx)
         (expand '(lambda ()
                    (:FunSym Ctx ; .. instead of Ctx here. avoid ..
                             (apply string (args)))))))) ; .. forwarding as list
(define (convenience-forward-first-rest FunSym ctx)
  (let (Ctx (sym (string ctx) MAIN))
    (set (sym FunSym ctx)
         (expand '(lambda ()
                    (:FunSym Ctx
                             (args 0) ; unchanged first (loc) and stringified ..
                             (apply string (rest (args))))))))) ; .. rest args

(when (not standard_constructor) ; be robust against module reload
  (set 'standard_constructor Logger)) ; store ctor from FOOPReference for reuse

;; FOOP Logger ctor with check for int fd
(define (Logger (fd 2) ; stderr
                (preprefix-string (fn () "")) ; optional
                (log-level level_default)) ; optional
  (if (not (integer? fd))
      (MAIN:begin ; avoid Logger:begin
       ;; difficult to use a logger here...
       (write-line 2 "[FATAL] fd arg has to be an int.")
       (exit 1)))
  (let (ref_ctx (standard_constructor fd
                                      preprefix-string
                                      log-level
                                      0)) ; indent starting with 0

    ;; convenience func forwards from ref context to foop
    [text] ;(convenience-forward 'info ref_ctx) results into (for first logger):
    (lambda () (: Logger:info Logger_1 (apply string (args))))
    [/text]
    (convenience-forward            'msg       ref_ctx)
    (convenience-forward-first-rest 'msg-loc   ref_ctx)
    (convenience-forward            'info      ref_ctx)
    (convenience-forward-first-rest 'info-loc  ref_ctx)
    (convenience-forward            'warn      ref_ctx)
    (convenience-forward-first-rest 'warn-loc  ref_ctx)
    (convenience-forward            'error     ref_ctx)
    (convenience-forward-first-rest 'error-loc ref_ctx)
    (convenience-forward            'fatal     ref_ctx)
    (convenience-forward-first-rest 'fatal-loc ref_ctx)

    ;; log level forwards
    ;;
    (set (sym 'level-debug ref_ctx) (lambda () (:level-debug (context))))
    (set (sym 'level-all   ref_ctx) (lambda () (:level-all   (context))))
    (set (sym 'level-info  ref_ctx) (lambda () (:level-info  (context))))
    (set (sym 'level-warn  ref_ctx) (lambda () (:level-warn  (context))))
    (set (sym 'level-error ref_ctx) (lambda () (:level-error (context))))
    (set (sym 'level-fatal ref_ctx) (lambda () (:level-fatal (context))))
    (set (sym 'level       ref_ctx) (lambda () (:level       (context))))
    ;; Logger:begin
    (set (sym 'begin       ref_ctx) (lambda ((what ""))
                                               (:begin       (context) what)))
    (set (sym 'end         ref_ctx) (lambda ((what ""))
                                               (:end         (context) what)))

    ;; mixins
    (setq ref_ctx:mixin-expr
          (lambda ()
            (Util:mixin-no-overwrite-of-non-nil MAIN:LoggerExpr (context))))
    (setq ref_ctx:mixin-expr-debug
          (lambda ()
            ((eval (sym "mixin-expr" (context)))) ; not elegant, but it works
            (Util:mixin-no-overwrite-of-non-nil MAIN:LoggerDebug (context))))

    ;; default logger for being used by other modules
    (if (not Logger:default) ; overload MAIN:default
        (set 'Logger:default ref_ctx))
    ref_ctx))

;; foop accessor indices (starting with 2)
(constant 's_fd 2 's_preprefixStr_func 3 's_logLevel 4 's_indent 5)

;; loglevels
(constant 'level_all   0
          'level_debug 0
          'level_info  1
          'level_warn  2
          'level_error 3
          'level_fatal 4
          'level_default level_info)

;; accessors
(define (fd)                           (self s_fd))
(define (set-fd fd)              (setq (self s_fd) fd))
(define (preprefix-func)               (self s_preprefixStr_func))
(define (set-preprefix-func fun) (setq (self s_preprefixStr_func) fun))
(define (log-level)                    (self s_logLevel))
(define (set-log-level l)        (setq (self s_logLevel) l))
(define (indent)                       (self s_indent))
(define (set-indent indent)      (setq (self s_indent) indent))

;; indirect getters
(define (preprefix-string)
  ((self s_preprefixStr_func))) ; call func
;; indirect setters
(define (use-timestamp-prefix)
  (set-preprefix-func (fn () (date (date-value) 0 "[%Y-%m-%d %X]"))))
(define (use-debug-prefix)
  (set-preprefix-func (fn () "[dbg]")))
(define (use-no-prefix)
  (setq (set-preprefix_func (fn () ""))))
(define (use-prefix-fun prefixFun)
  (set-preprefix-func prefixFun))

;; loglevel getters
(define (level)
  (self s_logLevel))
(define (get-level-default) ; needed?
  level_default)

;; loglevel setters
(define (level-default)
  (setq (self s_logLevel) level_default))
(define (level-all)
  (setq (self s_logLevel) level_all))
;;
(define (level-debug)
  (setq (self s_logLevel) level_debug))
(define (level-info)
  (setq (self s_logLevel) level_info))
(define (level-warn)
  (setq (self s_logLevel) level_warn))
(define (level-error)
  (setq (self s_logLevel) level_error))
(define (level-fatal)
  (setq (self s_logLevel) level_fatal))


(context MAIN) ; ...Logger



;; Logger extended for expression info: shows expressions both unevaluated and
;; evaluated. Evaluation happens before forwarding to foop, which is not suited,
;; because it changes (self).
(context 'LoggerExpr)

(define (default? symbol)
  (and (context? symbol)
       (default symbol)))
;;
(define (rep a)
  (if
   (float? a) (format "%f" a)
   (string? a) (append "\"" a "\"")
   (array? a) (append "[]" (string a))
   (quote? a) (append "'" (rep (eval a)))
   (context? a) (append
                 "[ctx] " (string a)
                 (if (Util:sym? (string a) a) ; no new sym by check for default
                     (let (defaultVal (default a))
                       (append ", " (string a) ":" (string a)
                               (if (= a defaultVal) " => " " -> ")
                               (if (context? defaultVal) "[ctx] " "")
                               (string defaultVal)))
                     ""))
   (Util:symbol-like? a) (Util:sym-string a)
   (string a)))
(define (name-rep a sepFlag restFlag)
  (append
   (if restFlag (if sepFlag "\n, " "; ") "")
   (if (number? a)
       (string a) ; source rep: 1 for 1.0 !
       (rep a))
   (if (and (Util:non-dynamic-symbol? a)
            (= a (eval a)))
       " => "
       " -> ")
   (if sepFlag ">>\n" "")
   (if (and (Util:dynamic-symbol? a)
            (not (Util:dynamic-target-context a)))
       (setq lastExprEval (append (:preprefix-string (self))
                                  " No evaluation: dynamic context missing."))
       (rep (setq lastExprEval (eval a))))
       ;; debugging:
       ;; (rep (begin
       ;;        (write-line 2 (string "\nlastExpr: " a))
       ;;        (set 'lastExprEval (eval a))
       ;;        (write-line 2 (string "\nlastExprEval: " lastExprEval))
       ;;        lastExprEval)))
   (if sepFlag "\n<<" "")))
(define (expr-info-string arguments sepFlag)
  (local (nameReps)
    (if (null? arguments)
        (format "%s %s:expr: no argument" Logger:c_warningStr (string (context)))
        (begin
         (push (name-rep (first arguments) sepFlag) nameReps)
         (dolist (a (rest arguments))
                 (push (name-rep a sepFlag true) nameReps -1))
         (join nameReps)))))
(define (tloc2string loc) ; used by lib/assert.lsp
  (format "%s %d"
          (string (last loc))
          (first loc)))
(define (expr-info-string-tloc tloc arguments sepFlag)
  (append
   (string "[" (tloc2string tloc) "] ")
   (expr-info-string arguments sepFlag)))


;;
;; iface (to be moved to corresponding context (instead of using by FOOP)
;; to be fully functional)
;;

;; without output, just expr info string
(define-macro (expr-str)
  (expr-info-string (args)))
;; new:
(define-macro (expr-str-sep)
  (expr-info-string (args) true))

;;
;; with output into log channel
(define-macro (expr)
  ; after mixin into created Logger reference is (context) ..
  (:msg (context) ; .. a FOOPReference with foop default getting :msg call
        (expr-info-string (args)))
  lastExprEval)
;; robustness against missing locStrOrSym
(define-macro (expr-loc (locStrOrSym ""))
  (:msg-loc (context)
            (string (eval locStrOrSym))
            (expr-info-string (args)))
  lastExprEval) ;)
;;
(define-macro (expr-sep)
  (:msg (context)
        (expr-info-string (args) true))
  lastExprEval)
(define-macro (expr-loc-sep (locStrOrSym ""))
  (:msg-loc (context)
            (string (eval locStrOrSym))
            (expr-info-string (args) true))
  lastExprEval)

;; convenience forward to macro in foop; returns wrapped expr
(define-macro (wrap)
  (eval (append '(:wrap (context)) (args))))


(context MAIN) ; ...LoggerExpr


(context 'LoggerDebug) ; specific dbg: functions

;; on/off switching of debug messages ...
(set 'symbols-to-be-switched
     '(msg msg-loc LoggerDebug:begin end wrap ; not defined as tweakable below
       ;; expr-str[-sep][_tweaked] not to be switched
       expr expr-loc
       expr-sep expr-loc-sep
       info info-loc) ; not defined as tweakable below
     'symbols-to-be-switched-store (Util:add-prefix-to-syms
                                    "_"
                                    symbols-to-be-switched))
;; use empty non-evaluating macros for switched off versions
(dolist (s symbols-to-be-switched-store)
        (set s (lambda-macro ())))
(setq LoggerDebug:debug true) ; default (overload MAIN:debug)
(define (switch)
  (setq LoggerDebug:debug (not LoggerDebug:debug)) ; does not change nodebug flag!
  ;; to be swapped with their prefixed stored counterparts
  (Util:swap-symbols symbols-to-be-switched
                     symbols-to-be-switched-store))
(define (on)
  (if (not debug) ; LoggerDebug:debug
      (MAIN:begin ; overloaded above
       (switch)
       "Now on.")
      "Already on."))
(define (off)
  (if debug ; LoggerDebug:debug
      (MAIN:begin ; overloaded above
        (switch)
        "Off.")
      "Already off."))


(context MAIN) ; ...LoggerDebug
;;EOF
