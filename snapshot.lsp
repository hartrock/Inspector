#!/usr/bin/env newlisp
(or Init:basedir ; if not set, this ..
    ;; .. guessing of CWD only works, if started/loaded in/from this script dir
    (set 'Init:basedir "./Base"))
(or Init:minimalDone
    (load (append Init:basedir "/modules/Init.minimal.lsp")))
(or Introspection:loadedFlag
    (load-module "Introspection.lsp"))

;; make snapshot
(set 'filepath "/tmp/snapshot.json")
(define (make-snapshot filepath)
  (write-file filepath
              (Introspection:symbols-to-JSON (Util:symbols-all))))

(make-snapshot filepath)
(println "You may look onto snapshot's symbols by\n"
         "  http:localhost:8080/symbols.html?file="
         filepath "\n.")
