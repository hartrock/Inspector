(println "aLoop.lsp..")
(define (aLoop)
  (set 's "-->")
  (set 'end 10)
  (for (anIx 1 end)
       (set 'b (* 2 anIx) 'c (+ 1 anIx) 'd (* b c) 'e (* anIx anIx))
       (set 's (dup s))
       (println " anIx: " anIx "\n")))
(println "..aLoop.lsp")
(println "Start with\n  (aLoop)\nor\n  (debug (aLoop))\n.")
;;EOF