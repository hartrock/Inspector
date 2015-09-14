#!/usr/bin/env newlisp
(load "modules/Util.lsp")
(load "modules/Introspection.lsp")

;; make snapshot
(set 'filepath "/tmp/snapshot.json")
(define (make-snapshot filepath)
  (write-file filepath
              (Introspection:symbols-to-JSON (Util:symbols-all))))

(make-snapshot filepath)
(println "You may look onto snapshot's symbols by\n"
         "  http:localhost:8080/symbols.html?file="
         filepath "\n.")
