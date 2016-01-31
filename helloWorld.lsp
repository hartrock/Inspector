#!/usr/bin/env newlisp

;; minimal stuff needed for remote Introspection loaded during remote startup:
;; -> see remote startup message in Inspector log

(define (hello-world)
  (println "###")
  (println "Hello World! Via stdout.")
  (println "###")
  (write-line 2 "###")
  (write-line 2 "Hello World! Via stderr.")
  (write-line 2 "###"))

(hello-world)

;; EOF
