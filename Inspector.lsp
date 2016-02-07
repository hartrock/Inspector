(load-lib 'WS 'WebsocketFM) ; done there: (load-module "Introspection.lsp")

(logg:level-info)
;(dbg:off)


(context 'Inspector)

(set 'help
     (string
[text]Inspector to be started from interpreter by:
  (Inspector:start)
, and left from browser by loading:
  http://localhost:[/text]
WS:server_port
[text]/leave
.
You can jump to a symbol by using its anchor; e.g. for jumping to MAIN:MAIN use:
  http://localhost:8080/symbols.html#MAIN:MAIN
.[/text]))

(define (render-symbols rname requestHeaders)
  (cons (symbols MAIN) "text/plain"))
(define (heartbeat)
  (cons (date (date-value) 0 "[%Y-%m-%d %X] Here I am...") "text/plain"))
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
        (if (and rparams (set 'listSymStr (lookup "inList" rparams)))
            (set 'syms (Introspection:symbols-in-symStr listSymStr))
            (set 'syms (Util:symbols-all))) ; default
        ;;(dbg:expr syms)
        (if (and rparams
                 (set 'valStr (lookup "noInspectorSymbols" rparams))
                 (eval-string valStr))
            (set 'syms (clean (lambda (s) (= (prefix s) Inspector)) syms)))
        (cons (Introspection:symbols-to-JSON syms) "application/json"))))

(define (read-file-here file)
  (read-file (append Init:appdir "/" file)))

(define (setup)
  (WS:create-eval-resource "heartbeat" 'heartbeat)
  (WS:create-eval-resource "symbols" 'render-symbols)
  (WS:create-eval-resource "symbols-JSON" 'symbols-JSON)

  (WS:create-static-resource "index.html" (read-file-here "index.html")
                             "text/html")
  (WS:create-static-resource "favicon.ico" (read-file-here "favicon.ico")
                             "image/x-icon")
  
  (WS:create-static-resource "symbols.html" (read-file-here "symbols.html")
                             "text/html")
  (WS:create-static-resource "tree_help_common.html.inc"
                             (read-file-here "tree_help_common.html.inc")
                             "text/html")
  (WS:create-static-resource "jquery-2.1.4.js" (read-file-here "jquery-2.1.4.js")
                             "text/javascript")
  (WS:create-static-resource "tree.jquery.js" (read-file-here "tree.jquery.js")
                             "text/javascript")


  (WS:create-static-resource "egBase.js" (read-file-here "egBase.js")
                             "text/javascript")
  (WS:create-static-resource "symbolsTree.js" (read-file-here "symbolsTree.js")
                             "text/javascript")
  (WS:create-static-resource "symbols.js" (read-file-here "symbols.js")
                             "text/javascript")
  (WS:create-static-resource "inspector.css" (read-file-here "inspector.css")
                             "text/css")

  (WS:create-static-resource "jqtree.css" (read-file-here "jqtree.css")
                             "text/css"))
(define (setup-websocket)
  ;; taken from http://www.websocket.org/echo.html
  (WS:create-static-resource "websocket.html" (read-file-here "websocket.html")
                             "text/html")
  (WS:create-websocket-resource "wsPingPong" WebsocketFM:fm_echo)

  (WS:create-static-resource
   "CodeMirror-5.7.0/lib/codemirror.css" (read-file-here
                                          "CodeMirror-5.7.0/lib/codemirror.css")
   "text/css")
  (WS:create-static-resource
   "CodeMirror-5.7.0/lib/codemirror.js"
   (read-file-here "CodeMirror-5.7.0/lib/codemirror.js")
   "text/javascript")
  (WS:create-static-resource
   "CodeMirror-5.7.0/mode/scheme/scheme.js"
   (read-file-here "CodeMirror-5.7.0/mode/scheme/scheme.js")
   "text/javascript")
  (WS:create-static-resource
   "console.js" (read-file-here "console.js")
   "text/javascript")
  (WS:create-static-resource
   "console.css" (read-file-here "console.css")
   "text/css")
  (WS:create-static-resource
   "consoleAlone.css" (read-file-here "consoleAlone.css")
   "text/css")
  (WS:create-static-resource
   "consoleAlone.html" (read-file-here "consoleAlone.html")
   "text/html")
  (WS:create-static-resource
   "inspector.html" (read-file-here "inspector.html")
   "text/html")
  (WS:create-static-resource
   "interact.js" (read-file-here "interact.js")
   "text/javascript")
  (WS:create-websocket-resource "console" WebsocketFM:fm_console))

(Inspector:setup)
(Inspector:setup-websocket)

(define (start (serverPort WS:server_port)) ; use WS default 8080, if no arg
  (WS:start serverPort))


(context MAIN)

;; MARK: introspection tests
(when nil
  (logg:level-all)
  (set 'foo "bar")
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

;;(dbg:off)
(when nil
  (set 's_0 "bar\000buz")
  (set 's_1 "bar\u4711buz")
  (set 's_2 "我能吞下玻璃而不伤身体。")
  ;;(set 's "我能吞下玻璃而不伤身体。")(dup s 500)
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
)

;; EOF
