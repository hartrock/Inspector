(println "A_Tree_1.lsp ...")
(println "  Contains some functions with dynamic symbols;")
(println "  you could try:")
(println "    (set 'tterm \"foo\")")
(println "  while inspecting:")
(println "    A_Tree_1:pts-target")
(println "  .")

(set 'Util:foo1 JSON)
(set 'Util:foo2 MAIN)
(set 'Util:MAIN MAIN)

(new Tree 'A_Tree_1)

(set 'v_MAIN->nil  nil
     'v_MAIN->MAIN MAIN
     'v_MAIN->Util Util)
(define (dbg-vars_MAIN)
  (dbg:expr v_MAIN->nil v_MAIN->MAIN v_MAIN->Util))
(define (set_fun_a)
  (set 's1 nil)
  (A_Tree_1 "fun_a" (lambda () (dbg:expr 's1:bar)(println s1:bar))))

(set 's1 Util)
(A_Tree_1 "\127"  "-> \\127")
(A_Tree_1 "\\127" "-> \\\\127")
(A_Tree_1 "\128"  (lambda () "a) -> \\128"))
(A_Tree_1 "\\128" "b) -> \\\\128")
(A_Tree_1 "2.\128" (lambda () (println "b) -> \\\\128")))
(A_Tree_1 "üä\128öÄ" "buz")

(set_fun_a)


(context A_Tree_1)

(set 'v_A_Tree_1->nil  nil
     'v_A_Tree_1->A_Tree_1 MAIN:A_Tree_1
     'v_A_Tree_1->MAIN MAIN
     'v_A_Tree_1->Util Util)
(define (dbg-vars)
  (dbg:expr-sep v_A_Tree_1->nil v_A_Tree_1->MAIN v_A_Tree_1->Util)
  )
(define (dbg-target)
  (dbg:expr-sep v_A_Tree_1->nil:tterm v_A_Tree_1->MAIN:tterm v_A_Tree_1->Util:tterm)
  (dbg:expr-sep v_MAIN->nil:tterm v_MAIN->MAIN:tterm v_MAIN->Util:tterm))
(define (pts-var)
  (dbg:expr-sep
   (map Util:prefix-term-strings
        '(v_A_Tree_1->nil v_A_Tree_1->A_Tree_1 v_A_Tree_1->MAIN v_A_Tree_1->Util))))
(define (pts-target)
  (dbg:expr-sep
   (map Util:prefix-term-strings
        '(v_A_Tree_1->nil:tterm v_A_Tree_1->A_Tree_1:tterm v_A_Tree_1->MAIN:tterm v_A_Tree_1->Util:tterm)))
  (dbg:expr-sep
   (map Util:prefix-term-strings
        '(v_MAIN->nil:tterm v_MAIN->MAIN:tterm v_MAIN->Util:tterm))))
(define (set_fun_a2)
  (set 's1 Util)
  (MAIN:A_Tree_1 "fun_a2" (lambda () (dbg:expr 's1:bar)(println s1:bar))))
(define (set_fun_b)
  (set 's2 Util)
  (MAIN:A_Tree_1 "fun_b" (lambda () (dbg:expr 's2:bar)(println s2:bar))))

(set_fun_a2)
(set_fun_b)


(context MAIN)

(A_Tree_1 "fun2" (fn () (println syc:bur)(dump syc:bur)))

(println "... A_Tree_1.lsp")
;;EOF
