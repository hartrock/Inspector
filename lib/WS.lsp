;; Connection handling started with code from
;;   http://www.newlispfanclub.alh.net/forum/viewtopic.php?f=5&t=2197&p=12148&hilit=mod_lisp+howto#p12148
;; ; but this had to be made correct first...

(load-libs 'assert 'FM)
(module "crypto.lsp") ; websocket protocol

(assert:pre (context? logg)) ; needs a logger logg

(map context '(WS
               WS_A_Resource ; abstract FOOP class
               ;; instantiated FOOP classes
               WS_RStatic WS_RQueue WS_RCreator WS_REval WS_RWebsocket
               MAIN))
;; Trees
(new Tree 'WS_T_Resource)
(new Tree 'WS_T_Content)
(new Tree 'WS_T_Queue)
;;
(global 'WS_Headers_request) ; dynamically created/deleted Tree
;; 'WS_Headers_response) ;think refac to use it?


(context WS)
;; defaults
(set 'server_protocol "http" ; "mod_lisp"
     'server_port (or server_port 8080) ; 80 DNW for direct http
)

;;
;; util
;;

(define (val-or-nil key list_or_context)
  (cond
   ((context? list_or_context)
    (list_or_context key))
   ((list? list_or_context)
    (let (ass (assoc key list_or_context))
      (and ass (ass 1))))
   ((throw-error "implementation"))))

;; rewrite macro (with limitations)
(macro (em-cpop L (Ix 0)) ; emacro: expansion macro
  (if L
      (pop L Ix))) ; returns empty list, if empty list

;; rewrite macro (should be defined *before* first use...)
(macro (content-type-or-nil List_or_context)
  (val-or-nil "content-type" List_or_context))



;;
;; flow machine
;;

(set 'fm_main (FM))

(:add-nodes fm_main '(
   ("start"  "start")

   ("start_listening" "start listening")

   ("start_listening_error" "start listening error")
   ("listen" "listen")

   ("accept" "accept")

   ("accept_error" "accept error")
   ("handle_request" "handle request")
     ("get_request" handle_get-request)
       ("receiveNparse_header" rec-n-parse-header)
       ("rec_opt_content" handle_rec_opt_content)
         ("receive_content" rec-content)
     ("get_request_error" "get request error")
     ("compute_response" "compute response")
     
     ("send_response" "send response")

     ("cleanup_after_request" "cleanup after request")
     ("cleanup_after_request_error" "cleanup after request error")

   ("stop_server" "stop server")
))

(set 'fm_res (FM))
;; request handling flows
(:add-nodes fm_res '(
("START" handle_START) ;
("EXIT" handle_EXIT)

("compute_rprops"   handle_compute_rprops)
("check_debugMode" handle_check_debugMode)
("switch"          handle_switch)

("R_static" "static resource")
  ("R_static_GET" handle_static_GET)           ; OK  : more or less cacheable
  ("R_static_HEAD" handle_notImplementedYet)   ; OK  : more or less cacheable
  ("R_static_PUT" handle_notImplementedYet)    ; OK  : +w
  ("R_static_DELETE" handle_static_DELETE)     ; OK  : +w
    ("R_static_POST" handle_static_POST)       ; FAIL: op forbidden
    ("R_static_unknown" handle__unknown)       ; FAIL: op unknown
("R_creator" "creator resource")
  ("R_creator_GET" handle_creator_GET)         ; OK  : not cacheable by ext proxy
  ("R_creator_HEAD" handle_notImplementedYet)  ; OK  : needs hash header
    ("R_creator_PUT" handle_notImplementedYet) ; FAIL: op forbidden
    ("R_creator_DELETE" handle_creator_DELETE) ; FAIL: op forbidden
  ("R_creator_POST" handle_creator_POST)       ; OK  : +w
    ("R_creator_unknown" handle__unknown)      ; FAIL: op unknown
("R_queue" "queue resource")
    ("R_queue_GET" handle_queue_GET)           ; OK for pull: not cacheable by ext proxy, +r pull; FAIL for push
    ("R_queue_HEAD" handle_notImplementedYet)  ; FAIL: op forbidden ? +r pull
    ("R_queue_PUT" handle_notImplementedYet)   ; FAIL: op forbidden
    ("R_queue_DELETE" handle_queue_DELETE)     ; FAIL: op forbidden
  ("R_queue_POST" handle_queue_POST)           ; OK  : +w push or pull fifo/lifo
    ("R_queue_unknown" handle__unknown)        ; FAIL: op unknown
("R_unknown" "unknown resource")
    ("R_unknown_GET" handle_unknown_GET)       ; FAIL: op forbidden
    ("R_unknown_HEAD" handle_unknown_HEAD)     ; FAIL: op forbidden
  ("R_unknown_PUT" handle_unknown_PUT)         ; OK  : +w
    ("R_unknown_DELETE" handle_unknown_DELETE) ; FAIL: op forbidden
    ("R_unknown_POST" handle_unknown_POST)     ; FAIL: op forbidden
    ("R_unknown_unknown" handle__unknown)      ; FAIL: op unknown

("R_eval" "eval resource")
    ("R_eval_GET" handle_eval_GET)             ; OK  : less cacheable
    ("R_eval_HEAD" handle_notImplementedYet)   ; OK  : less cacheable
    ("R_eval_PUT" handle_notImplementedYet)    ; OK  : +w change eval func
    ("R_eval_DELETE" handle_notImplementedYet) ; OK  : +w del eval func
    ("R_eval_POST" handle_notImplementedYet)   ; FAIL: op forbidden
    ("R_eval_unknown" handle__unknown)         ; FAIL: op unknown

("R_websocket" "websocket resource")
    ("websocket_protocolError" handle_websocket_protocolError)
    ("R_websocket_GET" handle_websocket_GET)               ; OK  : upgrade req
        ("websocket_upgrade" handle_websocket_upgrade)
        ("websocket_do-protocol" handle_websocket_do-protocol_fork)
    ("R_websocket_HEAD" handle_websocket_protocolError)    ; FAIL: op invalid
    ("R_websocket_PUT" handle_websocket_protocolError)     ; FAIL: op invalid
    ("R_websocket_DELETE" handle_websocket_protocolError)  ; FAIL: op invalid
    ("R_websocket_POST" handle_websocket_protocolError)    ; FAIL: op invalid
    ("R_websocket_unknown" handle_websocket_protocolError) ; FAIL: op unknown

("finalize" handle_finalize)

)) ; ..:add-nodes fm_res


(context MAIN)

(new Class 'WS_A_Resource) ; good for overwrite of default functor


(context WS_A_Resource)

;; one of them to be overwritten by 'subclasses'
(define (static?))
(define (creator?))
(define (queue?))
(define (evaluator?))
(define (websocket?))


(context WS_RQueue)
(new WS_A_Resource)

(define (queue?)
  true)
(define (WS_RQueue id (MIMEtype "text/plain") queue_id POST-op)
  (list MAIN:WS_RQueue id MIMEtype queue_id POST-op))

(constant 's_id 1 's_MIMEtype 2 's_queue_id 3 's_op 4)
(define (MIMEtype)
  (self s_MIMEtype))
(define (queue-id)
  (self s_queue_id))
(define (op)
  (self s_op))


(context WS_RStatic)
(new WS_A_Resource)

(define (static?)
  true)

(define (WS_RStatic id (MIME_type "text/plain") creatorOrNil)
  (list MAIN:WS_RStatic id MIME_type creatorOrNil))

(constant 's_id 1 's_MIMEtype 2 's_creator 3) ; s_*: named self ix
(define (MIMEtype)
  (self s_MIMEtype))
(define (creator)
  (self s_creator))


(context WS_RCreator)
(new WS_A_Resource)

(define (creator?)
  true)

(define (WS_RCreator id (MIME_type "text/plain"))
    (list MAIN:WS_RCreator id MIME_type '() 0))

(constant 's_id 1 's_MIMEtype 2 's_indices 3 's_count 4) ; s_*: named self ix
(define (id)
  (self s_id))
(define (MIMEtype)
  (self s_MIMEtype))
(define (indices)
  (self s_indices))
(define (WS_RCreator:count)
  (self s_count))

(define (POST content) ; to be created
  (letn ((ix (++ (self s_count)))
        (rname (append (self s_id) "/" (string ix))))
   (WS:create-static-resource rname
                              content
                              (or (WS:content-type-or-nil WS_Headers_request)
                                  (self s_MIMEtype))
                              (self s_id)) ; inform me about its delete
   (push ix (self s_indices) -1)
   rname))
(define (available-statics-str)
  (join ; expects strings
   (map (lambda (ix) (string (self s_id) "/" ix "\n"))
        (self s_indices))))

(define (just-before-DELETE id)
  (let ((num (int ((++ (find "/" id)) id))))
    ;;(dbg:expr num)
    (pop (self s_indices) (find num (self s_indices)))))


(context WS_REval)
(new WS_A_Resource)

(define (evaluator?)
  true)

(define (WS_REvaluator id funSym)
      (list MAIN:WS_REvaluator id funSym))

(constant 's_id 1 's_funSym 2) ; s_*: named self ix
(define (funSym)
  (self s_funSym))

(define (evaluate)
  ((eval funSym) rname WS_headers_request))


(context WS_RWebsocket)
(new WS_A_Resource)

(define (websocket?)
  true)

(define (WS_RWebsocket id fmCtx)
  (list MAIN:WS_RWebsocket id fmCtx))

(constant 's_id 1 's_fm 2)
(define (id)
  (self s_id))
(define (fm)
  (self s_fm))


(context WS)

(constant 'content_lengthLimit (* 8 (* 1024 (* 1024)))) ; 8MB
(constant 'do-net-peek_sleep_base 2)
(constant 'do-net-peek_max_trials 11) ; max sleep before last 11. trial: 2^10ms

(define (prepare)
  (new Tree 'MAIN:WS_Headers_request))
  ;;(new Tree 'MAIN:WS_Headers_response))

(define (cleanup)
  (if WS_Headers_request (delete 'MAIN:WS_Headers_request))
  ;;(if WS_Headers_response (delete 'MAIN:WS_Headers_response))
  (set 'data_request nil
       'string_response nil))

(define (OLD_rec-n-parse-header , str )
  (set 'buf_len 1024) ; may be too small
  (println "buf_len: " buf_len)
  (net-receive curr_conn str buf_len "\nend\n")
  (println "str: " str)
  (letn ((res nil)
         (tokenized (parse (0 -1 str) "\n" 0)) ; parse without trailing \n
         (tokens (0 -1 tokenized)) ; without end tok
         (pairs (array (/ (length tokens) 2) 2 tokens)))
    (sort pairs)
    (WS_Headers_request (array-list pairs)) ; fill hash map
    ))

(define (rec-n-parse-header-mod_lisp fm)
  (local (key val)
    (while (and
            (setq key (read-line conn))
            (not (net-error))
            (!= key "end"))
      (setq val (read-line conn))
      (print (format "%30s" key))
      (println "  ->  " val)
      (WS_Headers_request key val)
      ))
  (if (net-error)
      (fm:advance "get_request_error")
      (fm:advance "rec_opt_content")))

(define (logg-key-val key val)
  (logg:info (format "%20s  ->  %s" key val)))

;; Measures to quickly timeout accepted preconnections without getting a header.
(constant 'requestHeader_timeout (* 100 1000)) ; 100ms
;;todo make it even more robust: read-line may wait forever for a "\n"
(define (rec-n-parse-header-http fm)
  (logg:info "[request...]")
  (local (ready line key val)
    (set 'ready (net-select conn "r" requestHeader_timeout))
    (dbg:expr ready (net-peek conn))
    (if (not ready)
        (dbg:expr (net-error))
        (set 'line (read-line conn)))
    (if (not (nil? line))
        (begin
          (set 'p (parse line " "))
          (when (= (length p) 3)
            (WS_Headers_request "method" (p 0))
            (WS_Headers_request "url" (p 1))
            (WS_Headers_request "protocol" (p 2))
            (logg-key-val "method" (p 0))
            (logg-key-val "url" (p 1))
            (logg-key-val "protocol" (p 2)))
          (while (and
                  (set 'ready (net-select conn "r" requestHeader_timeout))
                  (set 'line (read-line conn))
                  (!= line ""))
            ;;(dbg:expr line)
            (set 'p (parse line ": ")) ;&&&
            (if (= (length p) 2)
                (set 'key (lower-case (p 0))
                     'val (p 1)))
            (logg-key-val key val)
            (WS_Headers_request key val)
            )))
    (logg:info "[...request]")
    (if (or (nil? line) (not ready))
        (begin
          ;; http://stackoverflow.com/questions/5640144/c-how-to-use-select-to-see-if-a-socket-has-closed
          (if (and ready (= (net-peek conn) 0)) ; see link above and ..
              (logg:warn "Connection closed by other end.") ; ..interpreter code
              (and (not ready) (not (net-error)))
              (logg:warn "Connection timed-out."))
          (fm:advance "get_request_error")) ; net-error logged there
        (fm:advance "rec_opt_content"))))

;;todo reimplement with select
(define (do-net-peek conn , giveup_flag available nothing_count)
  (do-while (and (not giveup_flag) (= available 0))
            (setq available (net-peek conn))
            (if (= available 0)
                (begin
                  (++ nothing_count)
                  (logg:warn "(= (net-peek conn) 0)")
                  (if (< nothing_count do-net-peek_max_trials)
                      (let (time_ms (pow do-net-peek_sleep_base
                                         nothing_count))
                        (logg:warn "-> sleep " time_ms "ms")
                        (sleep time_ms))
                      (setq giveup_flag true)))))
  (if (> available 0)
      available)) ; else: ret nil

(define (rec-content-mod_lisp fm
                              , len available buf num_rec)
  (set 'len fm:len)
  (if
   (> len content_lengthLimit)
   (begin
     (logg:error "Content length " len " greater than length limit "
                 content_lengthLimit "."))
   (fm:advance "get_request_error"))
  (begin
    (setq data_request "")
    (let (gotten 0)
      (while (and (< gotten len)
                  (setq available (do-net-peek conn)))
        (setq num_rec (net-receive conn buf buf_len))
        (++ gotten num_rec)
        (extend data_request buf))
      (cond
       ((< gotten len)
        (logg:error "content data missing; gotten: "
                    gotten ": wrong content-length " len "?")
        (fm:advance "get_request_error"))
       ((> gotten len)
        (logg:error "More request data than expected; gotten " gotten
                    ", expected: " len " -> truncated.")
        (setq data_request (slice data_request 0 len))
        (fm:advance "get_request_error"))
       ("default"
        (fm:advance "compute_response"))))))

; HTTP/1.1 100 Continue
(define (rec-content-http fm
                          , len available buf num_rec)
  (set 'len fm:len)
  (if
   (> len content_lengthLimit)
   (begin
     (logg:error "Content length " len " greater than length limit "
                 content_lengthLimit ".")
     (fm:advance "get_request_error"))
   (begin
     (if (= (WS_Headers_request "expect") "100-continue")
         (net-send conn (append (WS_Headers_request "protocol")
                                " 100 Continue\r\n\r\n")))
     (setq data_request "")
     (let (gotten 0)
       (while (and (< gotten len)
                   (setq available (do-net-peek conn)))
         (setq num_rec (net-receive conn buf buf_len))
         (++ gotten num_rec)
         ;;(dbg:expr num_rec gotten len)
         (extend data_request buf))
       (cond
        ((< gotten len)
         (logg:error "content data missing; gotten: "
                     gotten ": wrong content-length " len "?")
         (fm:advance "get_request_error"))
        ((> gotten len)
         (logg:error "More request data than expected; gotten " gotten
                     ", expected: " len " -> truncated.")
         (setq data_request (slice data_request 0 len))
         (fm:advance "get_request_error"))
        ("default"
         (fm:advance "compute_response")))))))

(define (WS:init)
  (cond
   ((= server_protocol "http")
    (set 'rec-n-parse-header rec-n-parse-header-http
         'rec-content rec-content-http)
    (set 'create-header create-header-http
         'create-status-header create-status-header-http
         'create-end-header create-end-header-http)
    )
   ((= server_protocol "mod_lisp")
    (set 'rec-n-parse-header rec-n-parse-header-mod_lisp
         'rec-content rec-content-mod_lisp)
    (set 'create-header create-header-mod_lisp
         'create-status-header create-status-header-mod_lisp
         'create-end-header create-end-header-mod_lisp)
    )
   ("default"
    (logg:fatal "unknown server protocol " server_protocol)
    (exit 1)))
  ;; prefix_resource may be set from outside
  (constant 'URLprefix (or prefix_resource
                           "/") ; sep alone
            'URLprefix_len (length URLprefix)))


(define (handle_rec_opt_content fm)
  ;;(dbg:expr fm)
  (set 'fm:len (int (WS_Headers_request "content-length")))
  ;;(dbg:expr len)
  (if (> fm:len 0)
      (fm:advance "receive_content")
      (fm:advance "compute_response")))

(define (handle_get-request fm) ;failure result buf_len)
  (set 'buf_len (* 1024 1024))
  (fm:advance "receiveNparse_header")) ; (rec-n-parse-header conn)

(define (URL-to-localpath-and-params url
                                     , localURL splitURL localpath
                                       paramsURL params)
  (set 'localURL (URLprefix_len url) ; -> "" or localpath with opt args
       'splitURL (parse localURL "?") ; -> (empty) '() for ""
       'localpath (if splitURL
                      (splitURL 0) ; only there, if non-empty url string
                      "index.html") ; by "/" or "" after host:port in browser
       ;; join of rest for allowing '?'s after first '?' as part of query string
       'paramsURL (if (>= (length splitURL) 2) (join (rest splitURL) "?"))
       'params
       (when paramsURL
         ;;(dbg:expr paramsURL)
         (let (paramsList (parse paramsURL "&"))
           (map (lambda (p)
                  (letn ((split (parse p "="))
                         (key (split 0))
                         (val (if (= (length split) 2) (split 1)
                                  "true"))) ; treat missing "=val" as true flag
                    (cons key val)))
                paramsList))))
  (list localpath params))

(define (rsuffix rname)
  (let ((pos (find "\\." rname 0)))
    (and pos ((++ pos) rname))))

(define (static-resource-name? rname)
  (let ((suffix (rsuffix rname)))
    (and suffix (find "^(txt|html|json)$" suffix 0))))

(define (static-resource? rname)
  (and (WS_T_Resource rname)
       (:static? (WS_T_Resource rname))))
(define (creator-resource? rname)
  (and (WS_T_Resource rname)
       (:creator? (WS_T_Resource rname))))
(define (queue-resource? rname)
  (and (WS_T_Resource rname)
       (:queue? (WS_T_Resource rname))))
(define (eval-resource? rname)
  (and (WS_T_Resource rname)
       (:evaluator? (WS_T_Resource rname))))
(define (websocket-resource? rname)
  (and (WS_T_Resource rname)
       (:websocket? (WS_T_Resource rname))))

(define (post-resource? rname)
  (or (queue-resource? rname)
      (creator-resource? rname)))

(define (get-request?)
  (= (WS_Headers_request "method") "GET"))
(define (head-request?)
  (= (WS_Headers_request "method") "HEAD"))
(define (put-request?)
  (= (WS_Headers_request "method") "PUT"))
(define (delete-request?)
  (= (WS_Headers_request "method") "DELETE"))
(define (post-request?)
  (= (WS_Headers_request "method") "POST"))

(define (get-static-resource rname)
  (cons (WS_T_Resource rname) (WS_T_Content rname)))
(define (get-creator-resource rname)
  (cons (WS_T_Resource rname)))
(define (get-queue-resource rname)
  (cons (WS_T_Resource rname) (WS_T_Queue (:queue-id (WS_T_Resource rname)))))

(define (get-eval-resource rname)
  (WS_T_Resource rname))

;; helper
(define (h_set-static-resource rname content MIMEtype creatorOrNil)
  (WS_T_Resource rname (WS_RStatic rname MIMEtype creatorOrNil))
  (WS_T_Content rname content))
(define (h_set-creator-resource rname MIMEtype)
  (WS_T_Resource rname (WS_RCreator rname MIMEtype)))
(define (h_set-queue-resource rname MIMEtype queue_id operation)
  (WS_T_Resource rname (WS_RQueue rname MIMEtype queue_id operation)))
(define (h_set-eval-resource rname funSym)
  (WS_T_Resource rname (WS_REval rname funSym)))
(define (h_set-websocket-resource rname fmCtx)
  (WS_T_Resource rname (WS_RWebsocket rname fmCtx)))

;;
;; WS API ..
;;

;; static resource
;;
;; create new *without* overwrite of existing resources
(define (create-static-resource rname content (MIMEtype "text/plain") creatorOrNil)
  (assert:pre (nil? (WS_T_Resource rname)))
  (h_set-static-resource rname content MIMEtype creatorOrNil))
;; delete existing resource
(define (delete-static-resource rname
                                , creatorID)
  (assert:pre (WS_T_Resource rname))
  (when (set 'creatorID (:creator (WS_T_Resource rname))) ; inform creator
    (:just-before-DELETE (WS_T_Resource creatorID) rname))
  (WS_T_Content rname nil)
  (WS_T_Resource rname nil))
;; overwrite existing resource
(define (overwrite-static-resource rname content (MIMEtype "text/plain") creatorOrNil)
  (delete-static-resource rname)
  (create-static-resource rname content MIMEtype creatorOrNil))
;; create new *or* overwrite existing resource
(define (put-static-resource rname content (MIMEtype "text/plain") creatorOrNil)
  (when (WS_T_Resource rname)
    (delete-static-resource rname))
  (create-static-resource rname content MIMEtype creatorOrNil))

;; creator resource
;;
(define (create-creator-resource rname (MIMEtype "text/plain"))
  (assert:pre (nil? (WS_T_Resource rname)))
  (h_set-creator-resource rname MIMEtype))

;; queue resource
;;
(define (create-queue-resource rname (MIMEtype "text/plain") queue_id operation)
  (assert:pre (nil? (WS_T_Resource rname)))
  (assert:pre (not (nil? (WS_T_Queue queue_id)))) ; '() -> 'false', but not nil
  (assert:pre (member operation
                      '("push_fifo" "push_lifo" ; push variants
                        "pull_fifo_push_fifo" "pull_lifo_push_lifo" ; efficient
                        "pull_fifo_push_lifo" "pull_lifo_push_fifo"))) ; ineff.
  (h_set-queue-resource rname MIMEtype queue_id operation))
(define (create-FIFO id_push id_pull (MIMEtype "text/plain"))
  (assert:pre (!= id_push id_pull))
  (assert:pre (and
               (nil? (WS_T_Resource id_push))
               (nil? (WS_T_Resource id_pull))))
  (set 'qid (string id_push "_" id_pull))
  (assert (nil? (WS_T_Queue qid)))
  (WS_T_Queue qid '())
  (create-queue-resource id_push MIMEtype qid "push_fifo")
  (create-queue-resource id_pull MIMEtype qid "pull_fifo_push_fifo"))

;; eval resource
;;
(define (create-eval-resource rname funSym)
  (assert:pre (nil? (WS_T_Resource rname)))
  (h_set-eval-resource rname funSym))

;; websocket resource
;;
(define (create-websocket-resource rname fmCtx)
  (assert:pre (nil? (WS_T_Resource rname))) ; should not exist
  (h_set-websocket-resource rname fmCtx))

;;
;; .. WS API.
;;


(define (MIMEtype-for-suffix suffix)
  (cond
   ((= suffix "txt")
    "text/plain")
   ((= suffix "json")
    "application/json")
   ("default"
    (logg:warn "Unknown suffix " suffix ": using text/plain MIME type.")
    "text/plain")))
(define (static-resource-MIMEtype rname)
  (or (and (WS_T_Resource rname)
           (:MIMEtype (WS_T_Resource rname)))
      (MIMEtype-for-suffix (rsuffix rname))))

;; Headers according mod_lisp protocol:
;; this is (similar to HTTP, but not identical.
(define (create-status-header-mod_lisp status)
  (create-header-mod_lisp "Status" (string status)))
(define (create-header-mod_lisp key val)
  (append key "\r\n" val "\r\n"))
(define (create-end-header-mod_lisp)
  "end\r\n")

;; HTTP headers
(define (create-status-header-http status)
  (append "HTTP/1.1 " (http-status-message status) "\r\n"))
(define (create-header-http key val)
  (append key ": " val "\r\n"))
(define (create-end-header-http)
  "\r\n")

(define (create-headers keyVal_pairs)
  (join (map (lambda (p) (create-header (first p) (last p))) keyVal_pairs)))

(define (http-status-message statuscode)
  (append
   (case statuscode
    (101 "101 Switching Protocols") ; websocket
    (200 "200 OK")
    (201 "201 Created")
    (201 "204 No Content")
    (404 "404 Not Found")
    (405 "405 Method Not Allowed")
    (500 "500 Internal Server Error")
    (501 "501 Not Implemented")
    (true (append "HTTP error code: " (string statuscode)))
    )
   "."))
(define (websocket-status-message statuscode)
  (case statuscode
   (1000 "1000 normal closure")
   (1001 "1001 going away")
   (1002 "1002 protocol error")
   ;;   (100 "100 ")
   (1003 "1003 cannot accept type of data")
   (1004 "1004 reserved")
   (1005 "1005 no status code")
   (1006 "1006 abnormal closure")
   (1007 "1007 data not consistent with message type")
   (true (append "Websocket error code: " (string statuscode)))
   ))
(define (handle_START fm)
  ;; for (cleanup) see there
  (fm:advance "compute_rprops")
)

(define (handle_static_GET
         fm         ; access to FM with prev flow and other info
         dataOrNil) ; input from previous flow
  (let (content (last (get-static-resource rname)))
    (if (not content)
        (set 'http_status 404
             'str_content (string "Resource " rname " not found."))
        (set 'http_status 200
             'str_content content
             'MIMEtype (static-resource-MIMEtype rname))))
  (fm:advance "finalize" ; advance to - special - EXIT flow
              nil))  ; no input data for next flow

(define (handle_static_DELETE fm)
  (delete-static-resource rname)
  (set 'http_status 204)
  (fm:advance "finalize"))

(define (handle_unknown_GET fm)
  (set 'http_status 404
       'str_content (append "GET of unknown resource: " rname))
  (fm:advance "finalize"))

(define (handle_unknown_DELETE fm)
  (set 'http_status 404
       'str_content (append "DELETE of unknown resource: " rname))
  (fm:advance "finalize"))

(define (handle__unknown)
  (set 'http_status 501
       'str_content (string "Unimplemented request method "
                            (WS_Headers_request "method") "."))
  (fm:advance "finalize"))

(define (handle_eval_GET fm ; access to FM with prev flow and other info
                         , funSym evaluation)
  ;;(dbg:expr (get-eval-resource rname))
  (set 'funSym (:funSym (WS_T_Resource rname)))
  (if (nil? funSym)
      (set 'http_error 501)
      (if (and (symbol? funSym) (lambda? (eval funSym)))
          (begin (catch (set 'evaluation ((eval funSym)
                                          rname rparams WS_Headers_request)))
                 (if evaluation
                     (set
                      'http_status 200
                      'str_content (string (first evaluation))
                      'MIMEtype (last evaluation))
                     (set
                      'http_status 500)))
          (set 'http_status 500)))
  (fm:advance "finalize")) ; advance to - special - EXIT flow

(constant 'c_wswsPrefix "[websocket] ")

(define (handle_websocket_protocolError fm)
  (set 'http_status 405
       'str_content (string "Websocket protocol error: invalid request method "
                            (WS_Headers_request "method") "."))
  (fm:advance "finalize"))

(define (handle_websocket_GET fm)
  (cond
   ((not (and
          (= (WS_Headers_request "upgrade") "websocket")
          (member "Upgrade" (parse (WS_Headers_request "connection") ", "))))
    (fm:advance "websocket_protocolError"))
   ("else"
    (set 'key (WS_Headers_request "sec-websocket-key")
         'ver (WS_Headers_request "sec-websocket-version"))
    (cond
     ((not (and key ver (= ver "13")))
      (fm:advance "websocket_protocolError"))
     ("else"
      (fm:advance "websocket_upgrade"))))))

(define (h_ws-key_be64->ws-accept_be64 key_be64)
  (set 'keyPlus (append key_be64 "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
       'hash (crypto:sha1 keyPlus true)
       'hash_be64 (base64-enc hash))
  ;; example from http://tools.ietf.org/html/rfc6455#section-4.2.2 :
  ;; (WS:h_ws-key_be64->ws-accept_be64 "dGhlIHNhbXBsZSBub25jZQ==")
  (when nil
    (dbg:expr key_be64)
    (dbg:expr key)
    (dbg:expr keyPlus)
    (dbg:expr hash hash_be64))
  hash_be64)
(define (ws-key_be64->ws-accept_be64)
  (h_ws-key_be64->ws-accept_be64 (WS_Headers_request "sec-websocket-key")))

(define (handle_websocket_upgrade fm)
  (set 'http_status 101
       'accept_be64 (ws-key_be64->ws-accept_be64)
       'http_extra_header_str
       (append
        (create-headers
         '(("Upgrade" "websocket")
           ("Connection" "Upgrade")))
        (create-header "Sec-WebSocket-Accept" accept_be64)))
  (h_handle_finalize)
  ;;(dbg:expr conn)
  (send-response conn) ; conn 'global'
  (fm:advance "websocket_do-protocol"))

(define (websocket_do-protocol fm)
  (set 'fm_ws (:fm (WS_T_Resource rname))
       'fm_ws:conn conn)
  (net-close listen) ; no further use of listen socket by fork
  (set 'listen nil)
  (fm_ws:START)
  (fm:advance "EXIT"))
(define (handle_websocket_do-protocol_fork fm)
  (set 'pid (fork (websocket_do-protocol fm)))
  (logg:info "Websocket fork has PID " pid ".")
  (fm:advance "EXIT"
              true)) ; hasBeenForked


(define (handle_compute_rprops fm)
  ;; (set 'tmp (WS_Headers_request "url")) ; for debugging
  (let (localpath_and_params
        ;; (debug (URL-to-localpath-and-params tmp))) ; for debugging
        (URL-to-localpath-and-params (WS_Headers_request "url")))
    (set 'rname (localpath_and_params 0)
         'rparams (localpath_and_params 1)))
  (fm:advance "check_debugMode"))

(define (handle_check_debugMode fm)
  (if (or (= rname "debug") (= rname "leave"))
      (begin
        (set 'http_status 200
             'leave_after_reply true)
        (if (= rname "debug")
            (set 'debug_mode true
                 'str_content "WS left (debug mode).")
            (set 'str_content (string "WS left " (++ leaveCount) ". time.")))
        (fm:advance "finalize"))
      (fm:advance "switch")))

(define (handle_notImplementedYet fm)
  (set 'http_status 500)
  (set 'str_content (append "Unimplemented yet: " (:curr fm) "."))
  (fm:advance "finalize")
)

(define (handle_switch fm
                       , (node "R"))
  ;;(dbg:expr rname)
  (cond
   ((static-resource? rname)
    (extend node "_static"))
   ((creator-resource? rname)
    (extend node "_creator"))
   ((queue-resource? rname)
    (extend node "_queue"))
   ((eval-resource? rname)
    (extend node "_eval"))
   ((websocket-resource? rname)
    (extend node "_websocket"))
   ("default"
    (extend node "_unknown")))
  (fm:advance node) ; informational
  (cond
   ((get-request?)
    (extend node "_GET"))
   ((head-request?)
    (extend node "_HEAD"))
   ((put-request?)
    (extend node "_PUT"))
   ((delete-request?)
    (extend node "_DELETE"))
   ((post-request?)
    (extend node "_POST"))
   ("default"
    (extend node "_unknown")))
  (fm:advance node))

(define (handle_creator_GET fm)
  (set 'http_status 200
       'str_content (:available-statics-str (WS_T_Resource rname)))
  (fm:advance "finalize"))

(define (handle_queue_GET fm
                          , fit peeked)
  (let ((queue_id (:queue-id (WS_T_Resource rname)))
        (op (:op (WS_T_Resource rname))))
    (cond
     ((or (= op "push_fifo") (= op "push_lifo"))
      (set 'http_status 405
           'str_content (append "GET not allowed for resource: " rname))
      )
     ((or (set 'fit (or (= op "pull_fifo_push_fifo")
                        (= op "pull_lifo_push_lifo")))
          (= op "pull_fifo_push_lifo") (= op "pull_lifo_push_fifo"))
      (set 'peeked
           (and (WS_T_Queue queue_id)
                (if fit
                    ((WS_T_Queue queue_id) 0)
                    ((WS_T_Queue queue_id) -1))))
      (if peeked
          (set 'http_status 200
               'str_content (peeked 0)
               'MIMEtype (or (content-type-or-nil (peeked 1))
                             (:MIMEtype (WS_T_Resource rname))
                             "text/plain"))
          (set 'http_status 200
               'str_content "")) ; no MIMEtype
      )
     ("default"
      (set 'http_status 500
           'str_content (append (Logger:scriptname)
                                " implementation error.")))))
  (fm:advance "finalize"))

(define (handle_creator_DELETE fm)
  (set 'http_status 405
       'str_content (append "DELETE of creator resource: " rname))
  (fm:advance "finalize"))

(define (handle_queue_DELETE fm)
  (set 'http_status 405
       'str_content (append "DELETE of queue resource: " rname))
  (fm:advance "finalize"))

(define (handle_queue_POST fm
                           , fit popped)
  (let ((queue_id (:queue-id (WS_T_Resource rname)))
        (op (:op (WS_T_Resource rname))))
    (cond
     ((or (= op "push_fifo") (= op "push_lifo"))
      ;; pushing into (Queue queue_id) needs empty list there
      (if (= op "push_fifo")
          (push (list data_request (WS_Headers_request)) (WS_T_Queue queue_id) -1)
          (push (list data_request (WS_Headers_request)) (WS_T_Queue queue_id)))
      (set 'http_status 200
           'str_content "Push accepted.\n"))
     ((or (set 'fit (or (= op "pull_fifo_push_fifo")
                        (= op "pull_lifo_push_lifo")))
          (= op "pull_fifo_push_lifo") (= op "pull_lifo_push_fifo"))
      (set 'popped
           (if fit
               (pop (WS_T_Queue queue_id)) ; standard pop (efficient)
               (em-cpop (WS_T_Queue queue_id) -1))) ; non-standard pop, emacro
      (if popped
          (set 'http_status 200
               'str_content (popped 0)
               'MIMEtype (or (content-type-or-nil (popped 1))
                             (:MIMEtype (WS_T_Resource rname))
                             "text/plain"))
          (set 'http_status 200
               'str_content "") ; no MIMEtype
          ))
     ("default"
      (set 'http_status 500
           'str_content (append (Logger:scriptname)
                                " Implementation error.")))))
  (fm:advance "finalize"))

(define (handle_creator_POST fm)
  (let ((created (:POST (WS_T_Resource rname) data_request)))
    (set 'http_status 201
         'str_content (append created "\n"))
    (logg:info http_status " POST creator: " created " created"))
  (fm:advance "finalize"))

(define (handle_static_POST fm)
  (set 'http_status 405
       'http_extra_header_str (create-header "Allow" "GET")
       'str_content (append "POST at static resource " rname " not allowed."))
  (fm:advance "finalize"))

(define (handle_unknown_POST fm)
  (set 'http_status 405
       'str_content (append "POST at unknown resource " rname " not allowed."))
  (fm:advance "finalize"))
(define (handle_unknown_HEAD fm)
  (set 'http_status 405
       'str_content (append "HEAD at unknown resource " rname " not allowed."))
  (fm:advance "finalize"))

(define (handle_unknown_PUT fm)
  (WS_T_Content rname (or data_request ""))
  (WS_RStatic rname
              (or (content-type-or-nil WS_Headers_request)
                  "text/plain"))
  (set 'http_status 201
       'str_content (append "Resource " rname " created."))
  (logg:info 201 "PUT unknown: " rname " created")
  (fm:advance "finalize"))

(define (response-header-str http_status http_extra_header_str
                             content MIMEtype)
  (append
   (if http_status
       (create-status-header (or http_status 200))
       "")
   (if content
       (let (content_len (length content))
         (append
          (create-header "Content-Length" (string content_len))
          (if (or MIMEtype (> content_len 0))
              (create-header "Content-Type" (or MIMEtype "text/plain; charset=utf-8"))
              "")))
       "")
   (if (= sever_protocol "mod_lisp")
       (create-header "Keep-Socket" "0") ; mod_lisp param
       "")
   (or http_extra_header_str "")
   (if http_extra_response_headers_hook ; for injecting extra response headers
       (create-headers (http_extra_response_headers_hook))
       "")
   (if (!= http_status 101) ; longer lasting websocket and no forked request ..
       (create-header "Connection" "close") ; .. handling for other resources
       "")
   (create-end-header)))
;; for reuse by handle_websocket_upgrade
(define (h_handle_finalize fm)
  (when (and http_status (>= http_status 300))
    ((if (>= http_status 500)
         logg:error
         logg:warn)
     http_status " " (or str_content ""))
    (set 'str_content (append (http-status-message http_status) "\n"
                              (if str_content
                                  (append str_content "\n")
                                  ""))))
  (set 'str_head (response-header-str
                  http_status http_extra_header_str
                  str_content MIMEtype))
  (set 'string_response (append str_head (or str_content ""))))
  ;;(dbg:expr-sep string_response)
  ;;(dbg:expr (length str_content) (length string_response))
(define (handle_finalize fm)
  (h_handle_finalize)
  (fm:advance "EXIT"))
(define (handle_EXIT fm hasBeenForked)
  hasBeenForked)
;; fm_res FM loop
(define (compute-response
         , ; some vars being used by fm_res
         rname ; leads to nil val of WS:rname sym after return
         rparams ; URL params
         http_status http_extra_header_str str_content MIMEtype
         str_head str_content) ; string_response shared between fm_main, fm_res
  (fm_res:START)) ; rets true, if further computation has been forked

(define (send-response conn
                       , num_sent)
  ;;(dbg:expr string_response)
  (setq num_sent (net-send conn string_response))
  (logg:msg-loc 'send-response "num_sent: " num_sent)
  (if (nil? num_sent) ;todo: cleanup after net-error
      (logg:error (net-error))))

(define (listening-info)
  (if listening-info-string
      (logg:info "\n" listening-info-string)))

(define (server_loop)
  (local
      (curr funOrNil listen conn
       failure conn continue string_response
       hasBeenForked
       buf_len
       continue)
    (set 'continue true)
    (while continue ; almost forever
      (set 'curr (:curr fm_main)
           'funOrNil (:curr-func fm_main))
      (cond
       (funOrNil
        ((eval funOrNil) fm_main))
       ((= curr "start")
        (:advance fm_main "start_listening"))

       ((= curr "start_listening")
        (if (not (set 'listen (net-listen server_port)))
            (begin
              (logg:error "opening socket: " (net-error))
              (:advance fm_main "start_listening_error"))
            (:advance fm_main "listen")))
       ;;
       ((= curr "start_listening_error")
        (logg:fatal "cannot open socket for listeening: giving up.")
        (exit 1))

       ((= curr "listen")
        (prepare)
        (listening-info)
        (set 'conn (net-accept listen))
        (when (net-error)
          (logg:error "net-error: " (net-error))
          (:advance fm_main "accept_error"))
        (:advance fm_main "accept"))
       ;;
       ((= curr "accept_error")
        (logg:error "accept error: start listening anew (recreate socket).")
        (net-close listen)
        (logg:info "sleep 10s")
        (sleep 10000) ; avoid fast errorneous loop
        (:advance fm_main "start_listening"))

       ((= curr "accept")
        (:advance fm_main "handle_request"))

       ((= curr "handle_request")
        (:advance fm_main "get_request"))

       ;;((= curr "get_request") -> factored out
       ;;
       ((= curr "get_request_error")
        (logg:error (append
                     "Reading request failed"
                     (if (net-error)
                         (append ": " (net-error))
                         "")
                     "."))
        (:advance fm_main "cleanup_after_request_error"))

       ((= curr "compute_response")
        (if (set 'hasBeenForked (compute-response))
            (begin
              (logg:info "Computation of response has been forked.")
              (:advance fm_main "cleanup_after_request")) ; response delegated
            (:advance fm_main "send_response"))) ; responsible for response

       ((= curr "send_response")
        (send-response conn)
        (:advance fm_main "cleanup_after_request"))

       ((or (= curr "cleanup_after_request")
            (= curr "cleanup_after_request_error"))
        (when conn ; (dbg:expr-loc "cleanup" conn)
          (net-close conn)
          (set 'conn nil))
        (cleanup)
        (logg:info (dbg:expr-str-sep ((:stats fm_main))))
        (logg:info (dbg:expr-str-sep ((:stats fm_res))))
        (:advance fm_main "listen")
        ;;
        (when leave_after_reply
          (set 'continue nil
               'leave_after_reply nil)
          (net-close listen)
          (set 'listen nil)))
       ("default"
        (logg:fatal "Unknown flow " curr ": exiting.")
        (exit 1))
       ) ; cond
      ) ; (while
    ) ; (local
  ) ; fun


(define (start (serverPort server_port)) ; use default if no arg
  (set 'server_port serverPort)
  (init)
  (:advance fm_main "start")
  (server_loop))


;; ..WS

;; EOF
