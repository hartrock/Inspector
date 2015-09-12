(context 'Libs)

(when (not Logger:default)
  (write-line 2 "[FATAL] Libs module needs Logger:default.")
  (exit 1))

(set 'Libs:log Logger:default)

(push (Logger:scriptpath) loaded)
(push (Logger:scriptpath) loadStack)

(define (~/-to-homedir path_str)
  (replace "^~(?=/)" path_str (env "HOME") 0))

(define (module-opt-overwrite modulePath overwriteFlag)
  (log:begin "module-opt-overwrite")
  ;;& (dbg:expr modulePath overwriteFlag)
  ;;& (dbg:expr logg)
  (let (alreadyLoaded (find modulePath (or loaded '())))
    ((if (and alreadyLoaded (not overwriteFlag))
         log:warn-loc
         log:info-loc)
     'module-opt-overwrite
     "\"" modulePath "\" "
     (if alreadyLoaded
         (if overwriteFlag
             "to be loaded again (overwrite)"
             "skipped (already loaded)")
         "to be loaded")
     ".") ; normal case
    (if (or (not alreadyLoaded) overwriteFlag)
        (begin
          (push modulePath loaded)
          (push (cons (first loadStack) modulePath) loadDeps)
          (push modulePath loadStack)
          (load (~/-to-homedir modulePath))
          (pop loadStack))))
  (log:end "module-opt-overwrite"))

(define (string-with-prefix symbol)
  (Util:sym-string symbol))
(define (load-lib ctxSym (overwriteFlag nil))
  ;;& (dbg:expr ctxSym)
  (++ loadLibDepth)
  (log:info-loc 'load-lib
                 (append "load lib for context " (string ctxSym) ", depth: "
                         (string loadLibDepth)))
  (let ((alreadyLoaded (find ctxSym libsLoaded))
        (alreadyLoadedCurr (find ctxSym libsLoadedCurr)))
    (if (or
         (and alreadyLoaded (not overwriteFlag))
         alreadyLoadedCurr)
        (begin
          (log:info-loc 'load-lib "lib '" ctxSym "' already seen: skipped.")
          (push ctxSym libsSometimesSkipped -1))
        (begin
          (push ctxSym libsLoadedCurr -1)
          (push ctxSym libsLoaded -1)
          (module-opt-overwrite (append Init:libdir "/" (string ctxSym) ".lsp")
                                overwriteFlag))))
  (-- loadLibDepth))

(setq prioHighest 5
      prioHigh    4
      prioMedium  3
      prioLow     2
      prioLowest  1)
(set 'loadLibDepth 0
     'libStack '(MAIN)
     'libsLoaded '()
     'libsInited '())
;; Loads each lib once: to overwrite a lib and its dependencies use overwrite
;; flag at end of arguments: e.g.
;;   (load-libs libSym1 libSym2 ... true)
;; (load-libs ...): args
;; - may be lib syms or lists of lib syms,
;; - optional followed by a true flag for overwriting libs already loaded.
(define (load-libs arguments
                   , overwriteFlag flatList, alreadyLoaded)
  ;;(log:info (context)) (println "context: " (context))
  (log:begin (string "load-libs " arguments))
  (when (and arguments
             (= (last arguments) true))
    (pop arguments -1)
    (set 'overwriteFlag true)
    (log:expr arguments loadLibDepth)
    (if (> loadLibDepth 0)
        (log:warn-loc 'load-libs
                       "Overwrite flag ignored for lib load level "
                       loadLibDepth ".")))
  (set 'flatList (flat arguments))
  (when (= loadLibDepth 0)
    (when (not overwriteFlag)
      (if (set 'alreadyLoaded (intersect flatList libsLoaded))
          (log:warn-loc 'load-libs "Libs " alreadyLoaded " already loaded.")))
    (set 'libsLoadedCurr '()
         'doOverwrite overwriteFlag))
  (log:expr flatList overwriteFlag)
  (dolist (a flatList)
          (when (not (context? (eval a)))
            (context a)
            (push (eval a) createdContexts -1)))
  ;(log:info (context))
  (context MAIN)
  (push (list (libStack 0) flatList) libDeps -1)
  (dolist (a flatList)
          (push a libStack)
          (load-lib a doOverwrite)
          (pop libStack))
  (when (= loadLibDepth 0)
    (letn
        ((toBeInited (reverse (copy libsLoadedCurr))) ; init dependencies first
         (prioFunc (fn (e) (or
                            (eval (sym 'initializePriority e))
                            prioMedium)))
         (toBeInitedPrioritized
          (sort (copy toBeInited)
                (fn (x y) (>= (prioFunc x) (prioFunc y))))))
      (log:expr libsLoadedCurr toBeInited toBeInitedPrioritized)
      ;; look for initialize func in each context; and call it, if it exists
      (dolist (lib toBeInitedPrioritized) ; higher prio, then more dependency
              (if (member lib libsInited)
                  (if (not doOverwrite)
                      (log:fatal-loc
                       'load-libs
                       "Lib " lib " already initialized (should not happen).")
                      (log:warn-loc
                       'load-libs
                       "Re-initialize lib " lib " (overwrite)."))
                  (log:info-loc
                   'load-libs
                   "Trying to initialize lib " lib "..."))
              (let (funSym (sym "initialize" lib nil))
                (if (Util:lambda-or-macro-symbol? funSym)
                    (begin
                      ((eval funSym))
                      (log:info-loc 'load-libs "..." funSym " done."))
                    (log:info-loc 'load-libs
                                   "...no " lib ":initialize found (OK).")))
              (push lib libsInited -1)))
    (set 'doOverwrite nil))
  (log:expr libsLoadedCurr libsLoaded libsSometimesSkipped)
  (log:end "load-libs"))

(context MAIN)
