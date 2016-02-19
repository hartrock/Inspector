(load "./modules/Tst.lsp")
(load "./modules/Util.lsp")

(Tst:begin)

(context MAIN)

(println "context: " (context))
(println "self implicit: " (set 'res (Util:string-to-sym "foo")))
(Tst:check (and (= res 'foo) (= res 'MAIN:foo)))
(println "self explicit: " (set 'res (Util:string-to-sym "MAIN:foo")))
(Tst:check (and (= res 'foo) (= res 'MAIN:foo)))

;; other
(println "other: " (set 'res (Util:string-to-sym "Ctx:foo")))
(Tst:check (= res 'Ctx:foo))

(context 'Ctx)

(println "context: " (context))
(println "self implicit: " (set 'res (Util:string-to-sym "foo")))
(Tst:check (and (= res 'foo) (= res 'Ctx:foo)))
(println "self explicit: " (set 'res (Util:string-to-sym "Ctx:foo")))
(Tst:check (and (= res 'foo) (= res 'Ctx:foo)))
;; other
(println "other B: " (set 'res (Util:string-to-sym "B:foo")))
(Tst:check (= res 'B:foo))
(println "other MAIN: " (set 'res (Util:string-to-sym "MAIN:foo")))
(Tst:check (= res 'MAIN:foo))

(Tst:end)
;;EOF
