(load-lib 'WS)

(load-module "Introspection.lsp")

(logg:level-warn)
;(dbg:off)


(context 'Inspector)
(set 'help
[text]Inspector to be started from interpreter by:
  (Inspector:start)
, and left from browser by loading:
  http://localhost:8080/leave
.
You can jump to a symbol by using its anchor; e.g. for jumping to MAIN:MAIN use:
  http://localhost:8080/symbols.html#MAIN:MAIN
.[/text])

(define (render-symbols rname requestHeaders)
  (cons (symbols MAIN) "text/plain"))

(define (symbols-JSON rname rparams requestHeaders
                      , listSymStr listVal syms
                        valStr
                        res)
  ;;(dbg:expr rname rparams)
  (if (and rparams (set 'file (lookup "file" rparams)))
      (begin
         (set 'strJSONFromFile (read-file file))
         (if strJSONFromFile
             (cons strJSONFromFile "application/json")
             (cons (append "Error loading file '" file "'.") "text/plain")))
      (begin
        ;; catch for val_nil:term constructs (leading to nil for failed
        ;; catch/eval): "ERR: context expected in function symbol? : val_nil"
        (if (and rparams (set 'listSymStr (lookup "inList" rparams)))
            (begin
              (set 'listVal (eval-string listSymStr))
              ;;(dbg:expr listSymStr listVal)
              (set 'syms
                   (if (list? listVal)
                       (unique
                        (ref-all nil
                                 listVal
                                 (fn (ignore s) (if (catch (symbol? s) 'r) r nil))
                                 true))
                       '()))) ; be robust against non-lists
            (set 'syms (Util:symbols-all))) ; default
        ;;(dbg:expr syms)
        (if (and rparams
                 (set 'valStr (lookup "noInspectorSymbols" rparams))
                 (eval-string valStr))
            (set 'syms (clean (lambda (s) (= (prefix s) Inspector)) syms)))
        (cons (Introspection:symbols-to-JSON syms) "application/json"))))

(define (read-file-here file)
  ;;(dbg:expr (append Inspector:dir "/" file))
  (read-file (append Inspector:dir "/" file)))

(define (setup)
  (WS:create-eval-resource "symbols" 'render-symbols)
  (WS:create-eval-resource "symbols-JSON" 'symbols-JSON)

  (WS:create-static-resource "symbols.html" (read-file-here "symbols.html")
                             "text/html")
  (WS:create-static-resource "jquery-2.1.4.js" (read-file-here "jquery-2.1.4.js")
                             "text/javascript")
  (WS:create-static-resource "tree.jquery.js" (read-file-here "tree.jquery.js")
                             "text/javascript")


  (WS:create-static-resource "egBase.js" (read-file-here "egBase.js")
                             "text/javascript")
  (WS:create-static-resource "symbols.js" (read-file-here "symbols.js")
                             "text/javascript")
  (WS:create-static-resource "inspector.css" (read-file-here "inspector.css")
                             "text/css")

  (WS:create-static-resource "jqtree.css" (read-file-here "jqtree.css")
                             "text/css"))

(Inspector:setup)

(define (start (serverPort WS:server_port)) ; use WS default if no arg
  (WS:start serverPort))


(context MAIN)

(logg:level-all)
(set 'foo "bar")
(when true
(logg:info (Introspection:json-expr "foo"))
(logg:info (Introspection:json-str "foo"))
(dbg:expr (Introspection:json-str logg))
(dbg:expr (Introspection:json-expr logg))
(dbg:expr (Introspection:json-expr +))
(dbg:expr (Introspection:json-expr 4711))
(dbg:expr (Introspection:json-expr 'foo))
(dbg:expr (Introspection:json-str 'foo))
(dbg:expr (Introspection:json-str 'bar))
(dbg:expr (Introspection:json-str (array 6 '(1 2 3))))
(dbg:expr (Introspection:json-str nil))
(dbg:expr (Introspection:json-str 'nil))
(dbg:expr (Introspection:json-str default))
(dbg:expr (Introspection:json-str 'default))

(dbg:expr (Introspection:json-str logg))
(dbg:expr (Introspection:json-str 'logg))
(dbg:expr (Introspection:json-str MAIN))
(dbg:expr (Introspection:json-str 'MAIN))

(dbg:expr (Introspection:json-str true))
(dbg:expr (Introspection:json-str 'true))

(dbg:expr (Introspection:json-str "foo \"bar\" buz"))
)
;(define (bar))
;(dbg:off)

(set 's_0 "bar\000buz")
(set 's_1 "bar\u4711buz")
(set 's_2 "我能吞下玻璃而不伤身体。")
(set 's_3 (append (dup "this is a test" 250) "\n" (dup "this is a test" 250)))
(setq (s_3 5) "\000")
(set 's_4 "this is a test")
(setq (s_4 5) "\000")
(save "/tmp/s_3.lsp" 's_3)
(save "/tmp/s_4.lsp" 's_4)
(set 's_5 'aSym)
(set 's_5b ''aSymQuoted)
;; float test may make problems, if ',' (comma) is expected as fraction
;; separator
;;(set 's_6 3.1415926535)
(set 's_7 111111111111111111111111111111111111111111111111111111111)
(set 's_8 'dbg:expr)
(set 's_9 'dbg)
(set 's_10 ''dbg:expr)
(set 's_11 ''dbg)
(set 's_MAIN 'MAIN)
(set 's_str "\"")
(set 's_str2 "foo\"bar")

(set 's_s "\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\032\033\034\035\036\037\038\039\040\041\042\043\044")
(set 's_u "\u0000\u0001\u0002\u0003\u0004")
(set 's_v "foo\011bar")
(set 's_slash "\/")
(set 's_slash2 "\"foo\\/bar\"")
