#!/usr/bin/env newlisp

(context 'Init)

(set 'nodebug true) ; nil for more debug messages
;; script dir detection
(when (not appdir)
  (set 'appscript "startIt.lsp"
       'appscriptArgs (filter (fn (a) (find appscript a))
                              (main-args))
       'appdir ; be robust against CLI args not being appscript
       (if appscriptArgs
           (0 (- (length appscript)) ; only leave dirpath
              (first appscriptArgs))))
  (set 'appdir (if (null? appdir)
                   "." ; CWD
                   (0 -1 appdir)))) ; rm trailing "/"
;; set basedir: rel to it are modules/ and lib/
;; try to be compatible with preset Init:basedir not below appdir
(when (not basedir)
  (set 'basedir (append appdir "/Base")))


(context MAIN)

;; fat infrastructure
(when (not Init:extendedDone)
  (load (append Init:basedir "/modules/Init.lsp")))

;;
;; handle CLI arguments

(load-module "getopts_patched_Ted.lsp")

(longopt "port" ; shortopt -p interferes with newlisp opt!
         (set 'WS:server_port (int getopts:arg))
         "<port>"
[text]e.g. 8081 (default: 8080)
Notes:
- port 80 does not work with standard user rights;
- by giving different port arguments multiple instances can be run in parallel.
[/text])

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
