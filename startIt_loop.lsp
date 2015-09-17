#!/usr/bin/env newlisp
;; mimic .init.lsp if not loaded
(cond
 ((not Init:Init) ; first guard
  ;; second guard for not loading modules/init.lsp twice: allow load of it ..
  (set 'Init_allowLoadOnce true ; .. below, guard deleted in modules/Init.lsp
       'Init:nodebug true) ; nil for more debug messages
  ;; script dir detection
  (set 'Inspector:scriptname "startIt_loop.lsp"
       'Inspector:dir ; be robust against CLI args not containing scriptname
       (0 (- (length Inspector:scriptname)) ; only leave dirpath
          (first (filter (fn (a) (find Inspector:scriptname a))
                         (main-args)))))
  (if (null? Inspector:dir)
      (set 'Inspector:dir ".")) ; cwd
  ;; Inspector:dir to be used as Init:newlisp_dir: needs rel modules/ and lib/
  (set 'Init:newlisp_dir Inspector:dir)
  (set 'Init:moduledir (append Init:newlisp_dir "/modules"))
  (set 'Init:libdir (append Init:newlisp_dir "/lib"))
  (load (append Init:moduledir "/Init.lsp"))
  (set 'Init:Init true))
 ("default" ; .init.lsp loaded
  (set 'Inspector:dir (append Init:newlisp_dir "/Inspector"))))

;;
;; handle CLI arguments

(load-module "getopts_patched_Ted.lsp")

(longopt "port" ; shortopt -p interferes with newlisp opt!
         (set 'WS:server_port (int getopts:arg)) "<port>"
         "e.g. 80 oder 8080 (default: 8080")

(shortlongopt "h" "help" (getopts:usage useStr) nil "print this help message")

(set 'arguments (getopts (2 (main-args))))

(load (append Inspector:dir "/Inspector.lsp"))

;(Inspector:start)

(define (aLoop)
  (for (anIx 1 3)
       (println ">>>>>>>>>>>>>>>>>>>>>")
       (println "anIx: " anIx " (/3)")
       (println "You could (re)load\n"
                "  http://localhost:8080/symbols.html\n"
                "now, and look for the changed value of MAIN:anIx.")
       (println "Next increment by loading\n"
                "  http://localhost:8080/leave\n"
                ".")
       (println "<<<<<<<<<<<<<<<<<<<<<")
       (Inspector:start)))

(aLoop)
(println ">>>>>>>>>>>>>>>>>>>>>")
(println "Return to newLISP interpreter loop.")
(println Inspector:help)
(println "<<<<<<<<<<<<<<<<<<<<<")
