;; minimal local module load
;; - load each module once
;; - avoid override of functions from (normal) Init.lsp

(context 'Init)
(when (not Init:minimalDone)
  (when (nil? basedir) ; do not override set one
    (set 'basedir (or (env "NEWLISP_APP_basedir")
                      ".")))
  (write-line 2 (append "basedir: " basedir)) ; guess and hope for the best
  (set 'moduledir (append basedir "/modules")
       'loadedModules '()
       'loaded '())
  (define (load-module-minimal filename ; no opt overwrite
                               , (filepath (append moduledir "/" filename)))
    (write-line 2 (append "[load-module] " filepath))
    (if (find filepath loadedModules)
        (write-line 2 (append "[load-module] -> " filepath
                              " skipped (already loaded)."))
        (begin
          (push filepath loadedModules)
          (push filepath loaded)
          (load filepath)
          (write-line 2 (append "[load-module] -> " filepath
                                " loaded."))))))

(context 'dbg)
(when (not Init:minimalDone)
  (define (dbg:begin arg)
    (write-line 2 (string arg "..")))
  (define (end arg)
    (write-line 2 (string ".." arg)))
  (define (expr)))

(context MAIN)
(when (not Init:minimalDone)
  ;; Switch needed for being able to override functionality of load-module by
  ;; defining Init:load-module-extended later (re-defining load-module while
  ;; being in use gives segfault).
  (define (load-module filename)
    ((if Init:extendedDone
         Init:load-module-extended
         Init:load-module-minimal)
     filename))
  (set 'Init:minimalDone true))

;;EOF
