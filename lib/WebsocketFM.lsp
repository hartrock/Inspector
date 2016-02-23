(load-libs 'WS 'Sys)

(load-module "WS_WS.lsp") ; think: mv to ../lib/

;; special: may have been loaded before without using load-module, and so
;; without registering
(when (not Introspection:loadedFlag)
  (load-module "Introspection.lsp"))

(load-module "UTF8.lsp") ; debugging: check for invalid UTF-8

(map context '(WebsocketFM_echo WebsocketFM_console))


(context WebsocketFM)

(define (START_common fm)
  (set 'fm:buff ""
       'fm:status nil
       'fm:bytes "")
  (logg:info fm " \"" (:name fm) "\" started."))
(define (handle_START fm)
  (START_common fm)
  (if true
      (begin
        (logg:info c_wswsPrefix "Send a ping...")
        (set 'fm:response (WS_WS:ping->msg "Ping..."))
        (fm:advance "send"))
      (fm:advance "lookForMessage")))

(define (handle_net-error fm)
  (logg:error (net-error))
  (fm:advance "EXIT"))
;;
(define (handle_connectionClose fm)
  (fm:advance "EXIT"))

(define (handle_receive fm
                        , res)
  ;; wait for incoming message
  (while (and (not (set 'res (net-select fm:conn "read" 1000000)))
              (not (net-error))))
  (if (net-error)
      (begin
        ;;(dbg:expr res)
        (fm:advance "net-error"))
      (begin
        (net-receive fm:conn fm:buff 1024)
        (extend fm:bytes fm:buff)
        (fm:advance "lookForMessage"))))

(define (handle_lookForMessage fm
                               , res)
  (set 'res (WS_WS:status-buffer->msg fm:status fm:bytes)
       'fm:msg (res 0)
       'fm:status (res 1)
       'fm:bytes (res 2))
  ;;(dbg:expr fm:status fm:bytes res)
  (if fm:msg
      (fm:advance "interpretMessage")
      (fm:advance "receive")))
;;
(define (handle_interpret_control_message fm)
  (cond
   ((WS_WS:oc_ping? fm:opcode)
    (set 'fm:response WS_WS:text->msg (append "Pong: " fm:payload))
    (fm:advance "send"))
   ((WS_WS:oc_pong? fm:opcode)
    (logg:info c_wswsPrefix "Pong opcode received.")
    (fm:advance "lookForMessage"))
   ((WS_WS:oc_connectionClose? fm:opcode)
    (fm:advance "connectionClose"))
   ("else"
    (logg:error "Unhandled opcode " fm:opcode " -> ignore.")
    (fm:advance "lookForMessage")))) ; todo: "receive" ?
;;
(define (handle_interpret_message fm)
  (set 'fm:payload (fm:msg 0)
       'fm:opcode (fm:msg 1))
  ;;(dbg:expr payload fm:payload fm:opcode)
  (cond
   ((WS_WS:oc_control? fm:opcode)
    (fm:advance "interpretControlMessage"))
   ((WS_WS:oc_text? fm:opcode)
    (if (not (null? (length fm:payload)))
        (fm:advance "interpretTextMessage")
        (begin
          (logg:warn c_wswsPrefix "Text message with empty payload -> ignore.")
          (fm:advance "lookForMessage"))))
   ("else"
    (logg:warn c_wswsPrefix "Unhandled opcode " fm:opcode " -> ignore.")
    (fm:advance "lookForMessage")))) ; todo: "receive" ?

(define (handle_send fm)
  (set 'WS:string_response fm:response)
  (WS:send-response fm:conn)
  (if fm:stateAfterSend
      (begin
        (fm:advance fm:stateAfterSend)
        (set 'fm:stateAfterSend nil)) ; go other route only once
      (fm:advance "lookForMessage")))


;; FMs
;;
(set 'fm_echo (FM "echo"))
(:add-nodes fm_echo '(
  ("START" handle_START)
  ("EXIT"  "exit state") ; do nothing
  ("receive" handle_receive)
  ("net-error" handle_net-error) ; -> EXIT
  ("lookForMessage" handle_lookForMessage)
  ("interpretMessage" handle_interpret_message)
  ("interpretControlMessage" handle_interpret_control_message)
    ("connectionClose" handle_connectionClose) ; -> EXIT
  ("send" handle_send)
  ;;
  ("interpretTextMessage"
   WebsocketFM_echo:handle_interpret_text_message)))

;;
(set 'fm_console (FM "console"))
(:add-nodes fm_console '(
  ("EXIT" WebsocketFM_console:handle_cleanupAndEXIT)
  ("receive" WebsocketFM_console:handle_receive)
  ("net-error" handle_net-error) ; -> EXIT (handle_cleanupAndEXIT)
  ("lookForMessage" handle_lookForMessage)
  ("interpretMessage" handle_interpret_message)
  ("interpretControlMessage" handle_interpret_control_message)
    ("connectionClose" handle_connectionClose) ; -> EXIT (handle_cleanupAndEXIT)
  ("send" handle_send)
  ("send_nested" WebsocketFM_console:handle_send_nested)
  ;;
  ("START"
   WebsocketFM_console:handle_START)
  ("interpretTextMessage"
   WebsocketFM_console:handle_interpret_text_message)
  ("interpretTextMessage_nested"
   WebsocketFM_console:handle_interpret_text_message_nested)
  ("remoteEval"
   WebsocketFM_console:handle_remoteEval)
    ("remoteEvalError"
     WebsocketFM_console:handle_remoteEvalError)
    ("remoteEval_pipeError"
     WebsocketFM_console:handle_remoteEval_pipeError)
    ("remoteEvalConsole"
     WebsocketFM_console:handle_remoteEvalConsole)
    ("remoteEvalIntrospection"
     WebsocketFM_console:handle_remoteEvalIntrospection)
    ("prepareRemoteInput"
     WebsocketFM_console:handle_prepareRemoteInput)
    ("sendRemoteInput"
     WebsocketFM_console:handle_sendRemoteInput)
    ("startTransferRemoteResults"
     WebsocketFM_console:handle_startTransferRemoteResults)
    ("transferRemoteResults"
     WebsocketFM_console:handle_transferRemoteResults)
    ("repeatTransferRemoteResults"
     WebsocketFM_console:handle_repeatTransferRemoteResults)
    ("sendRemoteResults"
     WebsocketFM_console:handle_sendRemoteResults)
    
  ("remoteControl"
   WebsocketFM_console:handle_remoteControl)
  ("remoteControl_nested"
   WebsocketFM_console:handle_remoteControl_nested)
    ("remoteControlError"
     WebsocketFM_console:handle_remoteControlError)
    ("remoteControlOK"
     WebsocketFM_console:handle_remoteControlOK)
    ("remoteControl_start"
     WebsocketFM_console:handle_remoteControl_start)
    ("remoteControl_kill"
     WebsocketFM_console:handle_remoteControl_kill)
    ("remoteControlError_nested"
     WebsocketFM_console:handle_remoteControlError_nested)
    ("remoteControlOK_nested"
     WebsocketFM_console:handle_remoteControlOK_nested)
    ("remoteControl_start_nested"
     WebsocketFM_console:handle_remoteControl_start_nested)
    ("remoteControl_kill_nested"
     WebsocketFM_console:handle_remoteControl_kill_nested)
    ;;
    ("remoteDiedEvent"
     WebsocketFM_console:handle_remoteDiedEvent)

  ("controlErrorResponse"
   WebsocketFM_console:handle_controlErrorResponse)
  ("controlErrorResponse_nested"
   WebsocketFM_console:handle_controlErrorResponse_nested)
  ("controlResponse"
   WebsocketFM_console:handle_controlResponse)
  ("emitEvent"
   WebsocketFM_console:handle_emitEvent)))


;;
;;
(context WebsocketFM_echo)

(define (handle_interpret_text_message fm)
  (set 'fm:response (WS_WS:text->msg (append "Echo: " fm:payload)))
  (fm:advance "send"))


;;
;;
(context WebsocketFM_console)

(define (net-select-continue-if-handled-sig sockOrSockList mode (timeout -1)
                                            , finished readies)
  (until finished
         (set 'readies (net-select sockOrSockList mode timeout))
         (if (net-error)
             (if sighandlerAction ; net-error triggered by SIGCHLD?
                 (set 'sighandlerAction nil) ; yes: continue
                 (set 'finished true)) ; no: real net-error
             (set 'finished true)))
  readies)
(define (handle_receive fm
                        , readies)
  (set 'readies (net-select-continue-if-handled-sig
                 (if fm:nestedFlag
                     (list fm:conn)
                     (cons fm:conn fm:in_selfPipeSIGCHLD))
                 "read"))
  (if (net-error)
      (fm:advance "net-error") ; net error is fatal here (e.g. websocket conn)
      (if (find fm:in_selfPipeSIGCHLD readies)
          (fm:advance "remoteDiedEvent")
          (begin
            (net-receive fm:conn fm:buff 1024)
            (extend fm:bytes fm:buff)
            (fm:advance "lookForMessage")))))
(define (handle_remoteDiedEvent fm)
  (read-line fm:in_selfPipeSIGCHLD) ; empty pipe
  (closePipes_remote)
  (set 'fm:res_what "remote_died")
  (fm:advance "emitEvent"))
(define (signal-handler sig)
  (dbg:begin "sig-handler")
  (set 'sighandlerAction true)
  (println "received signal: " sig)
  (dbg:end "sig-handler"))

;; Would be problematically for multiple SIGCHLDs at once: but this is no
;; problem for handling death of our *single* remote interpreter child.
;; Notes:
;; - There may be SIGCHLD send from outside - not triggered by dead of remote -
;;   to FM_console process (so this may be called again).
(define (signal-handler_SIGCHLD sig
                                , res pid code)
  (dbg:begin "sig handler")
  (set 'sighandlerAction true)
  (logg:info "received signal: " sig)
  (set 'res (wait-pid -1 nil)) ; removes custom SIGCHLD handler, so ..
  (set-sighandler)             ; .. reestablish it
  (set 'pid (res 0)
       'code (res 1))
  (logg:info "signnal-hander_SIGCHLD: gotten PID " pid)
  (if (> pid 0)
      (begin
        (push pid gottenPIDs)
        (push code gottenCodes)
        (when (and fm
                   (remoteDied?)) ; sets fm:remote_status, if remote has died
          (write-line fm:out_selfPipeSIGCHLD)))
      (logg:warn-loc 'signal-handler_SIGCHLD
                     "wait-pid: no pid (which usually should be there)"))
  ;;(Util:pause-by-loop 30)
  (dbg:end "sig handler"))

;; Note (tricky): for SIG_CHLD
;;   SIG_DFL (IGN) (zombies) != explicitely set SIG_IGN (no zombies)
;; ! See:
;;   http://www.microhowto.info/howto/reap_zombie_processes_using_a_sigchld_handler.html
;; ; and from
;;   man 2 waitpid
;; :
;; (The original POSIX standard left the behavior of setting SIGCHLD to SIG_IGN
;; unspecified. Note that even though the default disposition of SIGCHLD is
;; "ignore", explicitly setting the disposition to SIG_IGN results in different
;; treatment of zombie process children.)

(define (set-sighandler)
  (dbg:begin "set-sighandler")
  (signal Sys:SIGCHLD signal-handler_SIGCHLD)
  ;;(signal Sys:SIGUSR1 signal-handler)
  (dbg:end "set-sighandler"))
(define (set-sigCHLD-behavior)
  (set 'gottenPIDs '()
       'gottenCodes '())
  (dbg:begin "set-sigCHLD-behavior")
  ;;(dbg:expr-loc 'set-sigCHLD-behavior fm)
  (map set '(fm:in_selfPipeSIGCHLD fm:out_selfPipeSIGCHLD) (pipe))  
  (set-sighandler)
  (dbg:end "set-sigCHLD-behavior"))
(define (start-remoteInterpreter fm argumentsOrNil)
  (map set '(fm:r<-i_out fm:i_out->r) (pipe))
  (map set '(fm:i<-r_out fm:r_out->i) (pipe))
  (map set '(fm:i<-r_err fm:r_err->i) (pipe))
  (map set '(fm:i<-r_introspection fm:r_introspection->i) (pipe))
  (map set '(fm:i<-r_meta fm:r_meta->i) (pipe))
  (set 'fm:i<-r_all (list fm:i<-r_out fm:i<-r_err fm:i<-r_introspection
                          fm:i<-r_meta))
  (dbg:expr-loc 'start-remoteInterpreter
                fm:i_out->r
                fm:i<-r_out fm:i<-r_err fm:i<-r_introspection fm:i<-r_meta)
  (dbg:expr-loc 'start-remoteInterpreter
                fm:r<-i_out
                fm:r_out->i fm:r_err->i fm:r_introspection->i fm:r_meta->i)
  ;; [todo] robustness: compute full path name
  (set 'fm:remote_commandStr
       (append
        "/usr/bin/env"
        ;; for finding modules/* by remote
        " NEWLISP_APP_basedir=" Init:basedir
        " INSPECTOR_META_CHANNEL=" (string fm:r_meta->i) ; remote -> Inspector
        " newlisp"
        " -C" ; force prompts
        " " Init:appdir "/remote_prompt-event.lsp" ; uses INSPECTOR_META_CHANNEL
        " " Init:moduledir "/Init.minimal.lsp"
        " " Init:moduledir "/Introspection.lsp"
        (if argumentsOrNil
            (append " " argumentsOrNil)
            "")))
  (set 'fm:pid_remote (process fm:remote_commandStr
                               fm:r<-i_out
                               fm:r_out->i fm:r_err->i))
  (when fm:pid_remote ;todo error handling
    (set 'fm:remote_status c_running)))

(constant 'c_bufSize (* 1024 64) ; 128 is too small, 64k seems to be standard
          'c_bufSize_meta 32
          'c_debuggerPrompt "s|tep n|ext c|ont q|uit > "
          'c_debuggerPrompt$ (append c_debuggerPrompt "$")
          'c_interruptPrompt "(c)ontinue, (d)ebug, e(x)it, (r)eset:"
          'c_interruptPrompt$ (append c_interruptPrompt "$")
          'c_len_debuggerPrompt (length c_debuggerPrompt)
          'c_len_interruptPrompt (length c_interruptPrompt)
          'c_maxlen_prompt (max c_len_debuggerPrompt c_len_interruptPrompt))
(define-macro (read-if-ready fdSym fdsReadySym outputSym newOutputFlagSym
                             repeatFlagExpr
                             , (fd (eval fdSym))
                               (fdsReady (eval fdsReadySym)))
  (if (not (find fd fdsReady))
      (set newOutputFlagSym nil)
      (let ((repeatFlag (eval repeatFlagExpr))
            (buf))
        (set newOutputFlagSym true)
        (read fd buf c_bufSize)
        (if (eval outputSym)
            (extend (eval outputSym) buf)
            (set outputSym buf))
        (while (and repeatFlag (not (null? (peek fd))))
          (read fd buf c_bufSize)
          (extend (eval outputSym) buf)))))
(define (trailing str num) ; rets last num chars of str
  ((- (min num (length str))) str))
(define (str-ends-with? str end)
  (= (trailing str (length end))
     end))

(define (collect-remote-results fm readyFDs
                                , buf)
  ;;(dbg:expr-loc 'collect-remote-results readyFDs)
  (if (find fm:i<-r_meta readyFDs)
      (begin
        (set 'fm:r_meta_count 0) ; count meta (prompt) events
        ;; there are multiple prompt events after interrupt followed by reset!
        (while (not (null? (peek fm:i<-r_meta)))
          (read fm:i<-r_meta buf c_bufSize_meta "\n") ; read until '\n'
          ;;(dbg:expr buf)
          (++ fm:r_meta_count)
          (sleep 10))
        (set 'fm:r_meta buf)) ; stems from prompt-event
      (set 'fm:r_meta nil
           'fm:r_meta_count 0))
  (read-if-ready fm:i<-r_out readyFDs fm:r_out fm:r_out_new_flag fm:r_meta)
  (read-if-ready fm:i<-r_err readyFDs fm:r_err fm:r_err_new_flag fm:r_meta)
  (read-if-ready fm:i<-r_introspection readyFDs fm:r_introspection
                 fm:r_introspection_new_flag fm:r_meta)
  (if (and (nil? fm:r_meta) fm:r_out_new_flag) ; prepare second finish criterium
      (set 'fm:trail (trailing fm:r_out c_maxlen_prompt))))

;;
(define (end-of-chop-utf8 str)
  (length (chop str)))
(define-macro (chop-utf8-leaveRest outputSym
                                   , (str (eval outputSym))
                                     (eocu (end-of-chop-utf8 str))
                                     (res (0 eocu str))
                                     (left (eocu str)))
  (set outputSym left)
  ;(dbg:expr eocu (length str) (length res) (length left))
  ;;(dbg:expr (valid-utf8 left) (valid-utf8 res))
  ;(dbg:expr (eval outputSym))
  res)
  
(define (h_valid-utf8 ustr
              , (ulen (utf8len ustr))
                (useq (sequence 0 ulen)))
  (map (fn (ix , res)
         (if (catch
                 (nth ix ustr)
               'res)
             res
             nil))
       (0 -1 useq)))
(define (valid-utf8 ustr)
  (not (member nil (h_valid-utf8 ustr))))

(define (closeIf fd)
  (if fd
      (close fd)))
;; close pipes from/to remote from fm's side
(define (closePipes_remote)
  (dolist (fd fm:i<-r_all)
          (closeIf fd))
  (set 'fm:i<-r_all '())
  (closeIf fm:i_out->r)
  (set 'fm:i_out->r nil))
(define (close_selfPipeSIGCHLD)
  (close fm:in_selfPipeSIGCHLD)
  (close fm:out_selfPipeSIGCHLD)
  (set 'fm:in_selfPipeSIGCHLD nil
       'fm:out_selfPipeSIGCHLD nil))
;;
(define (unset-sigCHLD-behavior)
  (signal Sys:SIGCHLD "reset") ; uninstall signal-handler_SIGCHLD first, ..
  ;; .. because it uses this pipe
  (close_selfPipeSIGCHLD))

;; FM caller responsible for closing fm:conn ; MARK: to be changed if forked
(define (handle_cleanupAndEXIT fm)
  (unset-sigCHLD-behavior);here could be better than immediately after sending..
  (when (remoteAlive?)
    (destroy fm:pid_remote 9) ; .. (hard) kill sig (triggering SIGCHLD handler)
    (closePipes_remote))
  (set 'fm:nestedFlag nil))

(define (handle_sendRemoteResults
         fm
         promptEventFlag debuggerPromptFlag interruptPromptFlag
         (promptFlag (or promptEventFlag
                         debuggerPromptFlag
                         interruptPromptFlag))
         , obj rrObj res)
  (if nil ;promptFlag
      (begin
        (dbg:expr (utf8len fm:r_out))
        (if (not (null? (utf8len fm:r_out)))
            (begin
              (dbg:expr (nth 0 fm:r_out))
              (dbg:expr (nth (- (utf8len fm:r_out) 2) fm:r_out))))))
  (JSON:add-prop "type" "evalResult" obj) ;fm:req_type obj)
  (JSON:add-prop "responseID" (string (++ fm:responseCount)) obj)
  (JSON:add-prop "requestID" fm:req_ID obj)
  (JSON:add-prop "status" "OK" obj)

  (JSON:add-prop "triggerType" fm:req_type rrObj)
  (if (or (= fm:req_type "remoteEvalConsole")
          (= fm:req_type "remoteEvalIntrospection"))
      (begin
        (JSON:add-prop "remoteEvalID" fm:req_remoteEval_ID rrObj)
        (JSON:add-prop "resultID" (string fm:req_remoteEval_ID "_" (++ fm:resultCount)) rrObj)
        (when (= fm:req_type "remoteEvalIntrospection")
          (JSON:add-prop "remoteEval_type" fm:req_remoteEval_type rrObj)))
      (= fm:req_type "remoteControl")
      (begin
        (JSON:add-prop "remoteControlID" fm:req_remoteControl_ID rrObj)
        (JSON:add-prop "resultID" (string fm:req_remoteControl_ID "_"
                                          (++ fm:resultCount)) rrObj)
        (JSON:add-prop "remoteControl_type" fm:req_remoteControl_type))
      (begin
        (JSON:add-prop "resultID" (string (++ fm:resultCount)) rrObj)))
  (JSON:add-prop "evalStatus" (if promptFlag "finished" "running") rrObj)
  ;;(dbg:expr promptFlag debuggerPromptFlag interruptPromptFlag)
  (if promptFlag
      (JSON:add-prop "promptType" (if debuggerPromptFlag "debug"
                                      interruptPromptFlag "interrupt"
                                      "normal")
                     rrObj))
  (if (or fm:r_out_new_flag
          (and promptFlag (> (length fm:r_out) 0))) ; last Unicode char
      (JSON:add-prop "stdout" (JSON:newLISP-chunk-quote-non-UTF-8-val
                               (if promptFlag
                                   fm:r_out
                                   (chop-utf8-leaveRest fm:r_out)))
                     rrObj))
  (if (or fm:r_err_new_flag
          (and promptFlag (> (length fm:r_err) 0)))
      (JSON:add-prop "stderr" (JSON:newLISP-chunk-quote-non-UTF-8-val
                               (if promptFlag
                                   fm:r_err
                                   (chop-utf8-leaveRest fm:r_err)))
                     rrObj))
      ;;todo: clearify where there is quoting to reach valid UTF-8 for websocket
      ;;  text transmission (needing it)
      ;; (JSON:add-prop "stderr" (JSON:string-quote
      ;;                          (if promptFlag
      ;;                              fm:r_err
      ;;                              (chop-utf8-leaveRest fm:r_err)))
      ;;                rrObj))
  ;; chop not used, due to transferring as big JSON string chunk
  (when (or fm:r_introspection_new_flag
            (and promptFlag (> (length fm:r_introspection) 0)))
    ;;(dbg:expr (0 30 fm:r_introspection))
    ;;(write-file "/tmp/t.json" fm:r_introspection)
    (JSON:add-prop-valJSON "introspection"
                           (if promptFlag
                               fm:r_introspection
                               ;;(chop-utf8-leaveRest fm:r_introspection))
                               (assert "should not been reached" nil))
                           rrObj))
  (JSON:add-prop "evalResult" rrObj obj)
  (when nil
    (when (nil? (json-parse res))
      (dbg:expr (json-error))
      (dbg:expr (json-parse res))))
  (set 'res (JSON:toJSON obj))
  (dbg:expr (length res))
  ;;todo: always valid? (same as above) -> omit check, if this is the case
  (if (dbg:expr (UTF8:invalid-UTF-8-string? res))
      (begin
        (write-file "/tmp/invalid_UTF-8.txt" res)))
  (set 'fm:response (WS_WS:text->msg res))
  (set 'WS:string_response fm:response)
  (WS:send-response fm:conn)
  (if promptFlag ; eval finished?
      (fm:advance "lookForMessage")
      (fm:advance "transferRemoteResults")))

(constant 'c_none    0
          'c_running 1
          'c_dead    2)
(constant 'c_remoteStatus->str '("none" "running" "dead"))
(define (remoteStatus->str status)
  (c_remoteStatus->str status))

;; use curr fm
(define (checkRemoteDead)
  (dbg:begin  "checkRemoteDead")
  (assert (remoteAlive?) fm:pid_remote)
  ;;(dbg:expr fm:pid_remote gottenPIDs)
  (when (set 'ix (find fm:pid_remote gottenPIDs))
    (set 'fm:remote_status c_dead)
    (pop gottenPIDs ix)
    (set 'statusCode (pop gottenCodes ix)
         'fm:exit-code_remote (Sys:exit-code statusCode)
         'fm:term-sig_remote (Sys:term-sig statusCode)))
  (dbg:end "checkRemoteDead")
  (= fm:remote_status c_dead))
;;
(define (remoteDied?)
  ;;(dbg:expr-loc 'remoteDied? fm)
  (when (remoteAlive?)
    (checkRemoteDead)))
;;
(define (remoteAlive?)
  (= fm:remote_status c_running))
;;
;; May never been triggered (closing r<->i pipes at remote's side does not
;; work).
(define (handle_remoteEval_pipeError fm)
  (set 'fm:res_message "Remote connection error -> kill remote for cleanup.")
  (destroy fm:pid_remote 9)
  (fm:advance "remoteEvalError"))

(constant 'trr_sleep_ms 50) ; 1ms is too small
(define (handle_transferRemoteResults fm selectFlag
                                      , readies)
  (sleep trr_sleep_ms)
  (set 'readies (net-select-continue-if-handled-sig
                 (append fm:i<-r_all (list fm:in_selfPipeSIGCHLD fm:conn))
                 "r"))
  (if (net-error) ; 0.
      (begin
        (if (nil? (peek fm:conn))
                  (fm:advance "net-error") ; browser->inspector websocket conn
                  (fm:advance "remoteEval_pipeError")));inspector<->remote pipes
      (find fm:conn readies) ; 1.
      (begin
        (set 'fm:nestedFlag true
             'fm:stateAfterSend_nested "transferRemoteResults")
        (fm:advance "receive"))
      (find fm:in_selfPipeSIGCHLD readies) ; 2.
      (fm:advance "remoteDiedEvent")
      (begin ; 3.
        (collect-remote-results fm readies)
        (let ((promptEventFlag (= fm:r_meta "prompt-event\n"))
              (break))
          (when (> fm:r_meta_count 1) ; true, if reset after interrupt
            (logg:info "Multiple prompts: looks like reset after interrupt."))
          (when (and promptEventFlag
                     (not fm:r_out_new_flag)) ; no prompt seen in stdout so far
            (begin ;; collect-remote-results again (after pause and with ..
              ;;      .. smaller select set)
              (logg:info
               "Remote prompt event, but no prompt seen in stdout so far:"
               " -> look for remote output again.")
              (set 'readies (net-select-continue-if-handled-sig
                             fm:i<-r_all "r" 1000000)) ; 1s timeout
              (if (net-error)
                  (begin
                    (fm:advance "remoteEval_pipeError")
                    (set 'break true))
                  (begin
                    (when (not (find fm:i<-r_out readies))
                      (logg:error "prompt in remote stdout missing -> ignored"))
                    (collect-remote-results fm readies)))))
          (when (not break)
            (letn ((debuggerPromptFlag
                    (and (not promptEventFlag)
                         fm:trail
                         (str-ends-with? fm:trail c_debuggerPrompt)))
                   (interruptPromptFlag
                    (and (not debuggerPromptFlag)
                         fm:trail
                         (str-ends-with? fm:trail c_interruptPrompt)))
                   (evalFinishedFlag (or promptEventFlag
                                         debuggerPromptFlag
                                         interruptPromptFlag)))
              (if (and (= fm:remoteEval_resultTransfer "bigChunk")
                       (not evalFinishedFlag))
                  (fm:advance "repeatTransferRemoteResults") ; collect again
                  (fm:advance "sendRemoteResults"
                              promptEventFlag
                              debuggerPromptFlag
                              interruptPromptFlag))))))))
;;
;; Indirection needed for looping: advancing to same flow (state) is forbidden
;; (because this typically is an error).
(define (handle_repeatTransferRemoteResults fm)
  (fm:advance "transferRemoteResults"))
;;
(define (clear-buffers)
  (set 'fm:trail nil ; reset second finish criterium
       'fm:resultCount 0 ; reset for next eval
       'fm:r_meta nil ;
       'fm:r_out nil  ; clear ..
       'fm:r_err nil ; .. buffers
       'fm:r_introspection nil)) ;
(define (handle_startTransferRemoteResults fm)
  (clear-buffers)
  (fm:advance "transferRemoteResults"))

(define (handle_START fm)
  (WebsocketFM:START_common fm) ; common init
  (set 'fm:remote_status c_none)
  (set-sigCHLD-behavior)
  (fm:advance "lookForMessage"))

(define (messageResponse fm
                         , obj res)
  (set 'obj '())
  (JSON:add-prop "type" fm:res_type obj)
  (JSON:add-prop "responseID" (string (++ fm:responseCount)) obj)
  (if fm:req_ID
      (JSON:add-prop "requestID" fm:req_ID obj))
  (when (= fm:res_type "event")
    (JSON:add-prop "what" fm:res_what obj)
    (when (= fm:res_what "remote_died")
      (if fm:exit-code_remote
          (JSON:add-prop "exitCode" fm:exit-code_remote obj)
          fm:term-sig_remote
          (JSON:add-prop "termSig" fm:term-sig_remote obj))))
  (if fm:res_status
      (JSON:add-prop "status" fm:res_status obj))
  (if fm:res_message
      (JSON:add-prop "message" fm:res_message obj))
  (if fm:res_subtype
      (begin
        (JSON:add-prop "subtype" fm:res_subtype obj)
        (if fm:res_subprops
            (JSON:add-prop "subprops" fm:res_subprops obj))))
  (set 'res (JSON:toJSON obj))
  ;;(dbg:expr res)
  (set 'fm:response (WS_WS:text->msg res))
  (fm:advance "send"))
;;
(define (messageResponse_nested fm
                                , obj res)
  (set 'obj '())
  (JSON:add-prop "type" fm:or_res_type obj)
  (JSON:add-prop "responseID" (string (++ fm:responseCount)) obj)
  (if fm:or_req_ID
      (JSON:add-prop "requestID" fm:or_req_ID obj))
  ;; (when (= fm:res_type "event")
  ;;   (JSON:add-prop "what" fm:res_what obj)
  ;;   (when (= fm:res_what "remote_died")
  ;;     (if fm:exit-code_remote
  ;;         (JSON:add-prop "exitCode" fm:exit-code_remote obj)
  ;;         fm:term-sig_remote
  ;;         (JSON:add-prop "termSig" fm:term-sig_remote obj))))
  (if fm:or_res_status
      (JSON:add-prop "status" fm:or_res_status obj))
  (if fm:or_res_message
      (JSON:add-prop "message" fm:or_res_message obj))
  (if fm:or_res_subtype
       (begin
         (JSON:add-prop "subtype" fm:or_res_subtype obj)
         (if fm:or_res_subprops
             (JSON:add-prop "subprops" fm:or_res_subprops obj))))
  (set 'res (JSON:toJSON obj))
  ;;(dbg:expr res)
  (set 'fm:response (WS_WS:text->msg res))
  (fm:advance "send_nested"))

(define (handle_requestErrorResponse fm)
  (set 'fm:res_type "requestError"
       'fm:res_status "error")
  (messageResponse fm))
(define (handle_remoteEvalError fm)
  (set 'fm:res_type "remoteEvalError"
       'fm:res_status "error")
  (messageResponse fm))

(define (handle_remoteControlError fm)
  (set 'fm:res_type "remoteControlResponse"
       'fm:res_status "error")
  (messageResponse fm))
(define (handle_remoteControlError_nested fm)
  (set 'fm:or_res_type "remoteControlResponse"
       'fm:or_res_status "error")
  (messageResponse_nested fm))
(define (handle_remoteControlOK fm)
  (set 'fm:res_type "remoteControlResponse"
       'fm:res_status "OK")
  (messageResponse fm))
(define (handle_remoteControlOK_nested fm)
  (set 'fm:or_res_type "remoteControlResponse"
       'fm:or_res_status "OK")
  (messageResponse_nested fm))

(define (handle_controlErrorResponse fm)
  (set 'fm:res_type "controlResponse"
       'fm:res_status "error")
  (messageResponse fm))
(define (handle_controlErrorResponse_nested fm)
  (set 'fm:or_res_type "controlResponse"
       'fm:or_res_status "error")
  (messageResponse_nested fm))
(define (handle_controlResponse fm)
  (set 'fm:res_type "controlResponse"
       'fm:res_status "OK")
  (messageResponse fm))

(define (handle_emitEvent fm)
  (set 'fm:res_type "event")
  (messageResponse fm))

(define (prepareIntrospectionCommand fm
                                     , funStr)
  (set 'funStr
       (case fm:req_remoteEval_type
         ("symbolsMap"
          (if fm:req_remoteEval_multiFlag
              "Introspection:multi-symbols-to-JSON"
              "Introspection:symbols-to-JSON"))
         ("symbolsInLists"
          "Introspection:symbols-in-listSyms-to-JSON")
         ("symbolsInListsQuoted"
          "Introspection:symbols-in-listSymsQuoted-to-JSON")
         ))
  (cond
   ((nil? funStr)
    (set 'fm:res_message (string "No valid introspection type: "
                                 fm:req_remoteEval_type "."))
    nil) ; err case ret
   ((or (nil? fm:req_remoteEval_symsExpression)
        (null? (length fm:req_remoteEval_symsExpression)))
    (set 'fm:res_message "symsExpression missing.")
    nil) ; err case ret
   ("OK"
    ;;(dbg:expr fm:req_remoteEval_symsExpression)
    (append
     "(write "
     (string fm:r_introspection->i)
     " ("
     funStr
     " "
     fm:req_remoteEval_symsExpression
     "))\n"))))
(define (handle_prepareRemoteInput fm introspectionFlag)
  (if introspectionFlag
      (set 'fm:remote_input (prepareIntrospectionCommand fm))
      (if (or (nil? fm:req_remoteEval_consoleInput)
              (null? fm:req_remoteEval_consoleInput))
          (set 'fm:res_message "input missing.")
          (set 'fm:remote_input fm:req_remoteEval_consoleInput)))
  (if (nil? fm:remote_input)
      (fm:advance "remoteEvalError")
      (fm:advance "sendRemoteInput")))

(define (handle_sendRemoteInput fm)
  ;;(dbg:expr fm:remote_input)
  (write fm:i_out->r fm:remote_input)
  (fm:advance "startTransferRemoteResults"))

(define (handle_remoteEval fm introspectionFlag)
  (set 'fm:req_remoteEval_ID (lookup "ID" fm:req_remoteEval))
  (if introspectionFlag
      (set 'fm:req_remoteEval_type (lookup "type" fm:req_remoteEval)
           'fm:req_remoteEval_symsExpression (lookup "symsExpression"
                                                     fm:req_remoteEval)
           'fm:req_remoteEval_multiFlag (= (lookup "multiFlag"
                                                   fm:req_remoteEval)
                                           'true)
           'fm:remoteEval_resultTransfer "bigChunk") ; necessary (JSON str)
      (set 'fm:req_remoteEval_consoleInput (lookup "input"
                                                   fm:req_remoteEval)
           'fm:remoteEval_resultTransfer (lookup "resultTransfer"
                                                 fm:req_remoteEval)))
  (if (remoteAlive?)
      (fm:advance "prepareRemoteInput" introspectionFlag)
      (begin
        (set 'fm:res_message
             (append
              "No remote interpreter available (remote status: "
              (remoteStatus->str fm:remote_status)
              ")."))
        (fm:advance "remoteEvalError"))))
(define (handle_remoteEvalConsole fm)
  (fm:advance "remoteEval"
              nil)) ; introspectionFlag
(define (handle_remoteEvalIntrospection fm)
  (fm:advance "remoteEval"
              true)) ; introspectionFlag

(define (handle_remoteControl fm)
  (set 'fm:req_remoteControl_ID (lookup "ID" fm:req_remoteControl)
       'fm:req_remoteControl_type (lookup "type" fm:req_remoteControl))
  (if (or (= fm:req_remoteControl_type "start")
          (= fm:req_remoteControl_type "kill"))
      (begin
        (set 'fm:res_subtype fm:req_remoteControl_type)
        (fm:advance (append "remoteControl_" fm:req_remoteControl_type)))
      (begin
        (set 'fm:res_message "protocol error")
        (fm:advance "remoteControlError"))))
(define (handle_remoteControl_nested fm)
  (set 'fm:or_req_remoteControl_ID (lookup "ID" fm:or_req_remoteControl)
       'fm:or_req_remoteControl_type (lookup "type" fm:or_req_remoteControl))
  (if (or (= fm:or_req_remoteControl_type "start")
          (= fm:or_req_remoteControl_type "kill"))
      (begin
        (set 'fm:or_res_subtype fm:or_req_remoteControl_type)
        (fm:advance (append "remoteControl_" fm:or_req_remoteControl_type
                            "_nested")))
      (begin
        (set 'fm:or_res_message "protocol error")
        (fm:advance "remoteControlError_nested"))))

(define (handle_remoteControl_start fm)
  (if (remoteAlive?)
      (begin
        (set 'fm:res_message (string "Already running remote with PID "
                                     fm:pid_remote
                                     ": only one remote instance allowed."))
        (fm:advance "remoteControlError"))
      (let (arguments (lookup "arguments" fm:req_remoteControl))
        (start-remoteInterpreter fm arguments)
        (if (remoteAlive?)
            (begin
	      (set 'fm:stateAfterSend "startTransferRemoteResults")
              (set 'fm:res_subprops (list
                                     (list "remoteCommand" fm:remote_commandStr)
                                     (list "pid" fm:pid_remote))
                   'fm:res_message (string "Start of remote with PID "
                                           fm:pid_remote "."))
              (fm:advance "remoteControlOK"))
            (begin
              (set 'fm:res_message "Start of remote failed.")
              (fm:advance "remoteControlError"))))))
;;
(define (handle_remoteControl_start_nested fm)
  (set 'fm:or_res_message (append "Nested command: start of"
                                  (if (remoteAlive?)
                                      " additional"
                                      "")
                                  " remote not allowed."))
  (fm:advance "remoteControlError_nested"))

(define (handle_remoteControl_kill fm)
  (if (not (remoteAlive?))
      (begin
        (set 'fm:res_message (string "No running remote: nothing to kill."))
        (fm:advance "remoteControlError"))
      (let (sig (lookup "signal" fm:req_remoteControl))
        (destroy fm:pid_remote sig) ; no ret val to check
        (set 'fm:res_subprops (list
                               (list "pid" fm:pid_remote)
                               (list "signal" sig))
             'fm:res_message (string "'Kill' of remote with PID "
                                     fm:pid_remote " with signal " sig "."))
        (fm:advance "remoteControlOK"))))
;; same as previous func, except 'or_ prefixes and "_nested" postfixes
(define (handle_remoteControl_kill_nested fm)
  (if (not (remoteAlive?))
      (begin
        (set 'fm:or_res_message (string "No running remote: nothing to kill."))
        (fm:advance "remoteControlError_nested"))
      (let (sig (lookup "signal" fm:or_req_remoteControl))
        (destroy fm:pid_remote sig) ; no ret val to check
        (set 'fm:or_res_subprops (list
                                  (list "pid" fm:pid_remote)
                                  (list "signal" sig))
             'fm:or_res_message (string "'Kill' of remote with PID "
                                        fm:pid_remote " with signal " sig "."))
        (fm:advance "remoteControlOK_nested"))))
;; Think: fails, if evaluation will be mixed with introspection.
(define (clear-old-FM-values fm)
  (set 'fm:stateAfterSend nil)
  ;;
  (set
   'fm:res_type nil
   'fm:res_what nil
   'fm:res_status nil
   'fm:res_message nil
   'fm:res_subtype nil
   'fm:res_subprops nil)
  (set
   'fm:or_res_type nil
   ;;'fm:or_res_what nil
   'fm:or_res_status nil
   'fm:or_res_message nil
   'fm:or_res_subtype nil
   'fm:or_res_subprops nil)
  (set 'fm:req nil
       'fm:req_ID nil
       'fm:req_type nil
       ;; same for "remoteEvalConsole", "remoteEvalIntrospection"
       'fm:req_remoteEval nil
       'fm:req_remoteEval_ID nil
       'fm:req_remoteEval_type nil ; differs from fm:req_type
       'fm:remote_input nil
       'fm:req_remoteEval_consoleInput nil
       'fm:req_remoteEval_symsExpression nil ; introspection
       'fm:req_remoteEval_multiFlag nil ; introspection
       'fm:remoteEval_resultTransfer nil
       ;;
       'fm:req_remoteControl nil
       'fm:req_remoteControl_ID nil
       'fm:req_remoteControl_type nil ; differs from fm:req_type
       )
  (set 'fm:or_req nil
       'fm:or_req_ID nil
       'fm:or_req_type nil
       ;; same for "remoteEvalConsole", "remoteEvalIntrospection"
       ;; 'fm:req_remoteEval nil
       ;; 'fm:req_remoteEval_ID nil
       ;; 'fm:req_remoteEval_type nil ; differs from fm:req_type
       ;; 'fm:remote_input nil
       ;; 'fm:req_remoteEval_consoleInput nil
       ;; 'fm:req_remoteEval_symsExpression nil ; introspection
       ;; 'fm:req_remoteEval_multiFlag nil ; introspection
       ;; 'fm:remoteEval_resultTransfer nil
       ;;
       'fm:or_req_remoteControl nil
       'fm:or_req_remoteControl_ID nil
       'fm:or_req_remoteControl_type nil ; differs from fm:req_type
       ))

(define (handle_interpret_text_message fm)
  (if fm:nestedFlag
      (fm:advance "interpretTextMessage_nested")
      (begin
        (clear-old-FM-values fm)
        (set 'fm:req (json-parse fm:payload))
        (dbg:expr fm:req)
        (if fm:req ; request
            (begin
              (set 'fm:req_type (lookup "type" fm:req)
                   'fm:req_ID (lookup "requestID" fm:req))
              (case fm:req_type
                ("remoteEvalConsole"
                 (set 'fm:req_remoteEval (lookup fm:req_type fm:req))
                 (fm:advance fm:req_type))
                ("remoteEvalIntrospection"
                 (set 'fm:req_remoteEval (lookup fm:req_type fm:req))
                 (fm:advance fm:req_type))
                ("remoteControl"
                 (set 'fm:req_remoteControl (lookup fm:req_type fm:req))
                 (fm:advance fm:req_type))
                ("test websocket connection"
                 (set 'fm:res_message
                      "reply to \"test websocket connection\"")
                 (fm:advance "controlResponse"))
                (true
                 (set 'fm:res_message
                      (append "Request type \""
                              (or fm:req_type "")
                              "\" not handled."))
                 (logg:warn fm:res_message)
                 (fm:advance "controlErrorResponse"))))
            (begin ; pong raw text messages
              (set 'fm:response (WS_WS:text->msg
                                 (append "[raw] payload: " fm:payload)))
              (fm:advance "send"))))))

(define (handle_interpret_text_message_nested fm)
  (set 'fm:or_req (json-parse fm:payload))
  (dbg:expr fm:or_req)
  (if fm:or_req ; request
      (begin
        (set 'fm:or_req_type (lookup "type" fm:or_req)
             'fm:or_req_ID (lookup "requestID" fm:or_req))
        (case fm:or_req_type
          ("remoteControl"
           (set 'fm:or_req_remoteControl (lookup fm:or_req_type fm:or_req))
           (fm:advance (append fm:or_req_type "_nested")))
          (true
           (set 'fm:or_res_message
                (append "Request type \""
                        (or fm:or_req_type "")
                        "\" not handled."))
           (logg:warn fm:res_message)
           (fm:advance "controlErrorResponse_nested"))))
      (begin
        (set 'fm:or_res_message
             "Protocol error: invalid JSON message -> not handled.")
        (fm:advance "send_nested"))))

(define (handle_send_nested fm)
  (assert fm:stateAfterSend_nested) ; to be set by nested trigger func
  (set 'WS:string_response fm:response)
  (WS:send-response fm:conn)
  (set 'fm:nestedFlag nil)
  (fm:advance fm:stateAfterSend_nested)
  (set 'fm:stateAfterSend_nested nil))

;; EOF
