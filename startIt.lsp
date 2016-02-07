#!/usr/bin/env newlisp
;; mimic .init.lsp if not loaded
(context 'Init)
(cond
 ((not Init:Init) ; guard
  (set 'nodebug true) ; nil for more debug messages
  ;; script dir detection
  (set 'appscript "startIt.lsp"
       'appdir ; be robust against CLI args not being appscript
       (0 (- (length appscript)) ; only leave dirpath
          (first (filter (fn (a) (find appscript a))
                         (main-args)))))
  (set 'appdir (if (null? appdir)
                   "." ; CWD
                   (0 -1 appdir))) ; rm trailing "/"
  ;; appdir to be used as basedir: rel modules/ and lib/
  (set 'basedir appdir)

  (load (append basedir "/modules/Init.lsp"))
  (set 'Init:Init true))
 (".init.lsp loaded"
  (set 'appdir (append basedir "/Inspector"))))


(context MAIN)

;;
;; handle CLI arguments

(load-module "getopts_patched_Ted.lsp")

(longopt "port" ; shortopt -p interferes with newlisp opt!
         (set 'WS:server_port (int getopts:arg)) "<port>"
         "e.g. 80 oder 8080 (default: 8080")

(shortlongopt "h" "help" (getopts:usage useStr) nil "print this help message")

(set 'arguments (getopts (2 (main-args))))

;;
;; load Inspector app

(load (append Init:appdir "/Inspector.lsp"))

;;
;; terminal info, (Inspector:start), terminal info

(println ">>>>>>>>>>>>>>>>>>>>>")
(println "You could load\n"
         "  http://localhost:" (or WS:server_port "8080") "/inspector.html"
            "#MAIN:hello-world\n"
         "now, and look for symbols of a remotely started newLISP instance.")
(println "<<<<<<<<<<<<<<<<<<<<<")

;; for seeing more, if started by another remote formerly started:
;;   (logg:level-all)
(Inspector:start)

(println ">>>>>>>>>>>>>>>>>>>>>")
(println "Return to newLISP interpreter loop.")
(println Inspector:help)
(println "<<<<<<<<<<<<<<<<<<<<<")
;;EOF
