(set 'Init:basedir ".")
(load "modules/Init.lsp")
(load-module "Tst.lsp")
(load-lib 'WS)

(Tst:begin)

(context MAIN)

(set 'hwStr "Hello World!\n")

(define (setup-WS)
  (WS:create-static-resource
   "hw.txt"
   hwStr)
  (WS:create-FIFO "queue" "queue/fifo" "text/plain"))

;;  (WS:create-creator-resource "exercises" "application/json")
;;  (WS:create-FIFO "results" "results/fifo" "application/json"))

(setup-WS)
(set 'pid (fork (WS:start 8888)))
(dbg:expr pid)
(sleep 100) ; give server a few moments for starting listening
(Tst:msg (string "test server started (" pid ")"))

(Tst:msg "get static resource")
(Tst:check (= (get-url "http://localhost:8888/hw.txt") hwStr))

;; queue resource
(Tst:msg "push to queue resource")
(Tst:check (= (post-url "http://localhost:8888/queue" hwStr "text/plain" 1000)
              "Push accepted.\n"))
(Tst:msg "pull from queue resource")
(Tst:check (= (post-url "http://localhost:8888/queue/fifo" "" "text/plain" 1000)
              hwStr))

(set 'times 1000)
(Tst:msg (string "get static resource " times " times."))
(Tst:msg (string
          "time: "
          (time (for (ix 1 times)
                     (if (not (= (get-url "http://localhost:8888/hw.txt")
                                 hwStr))
                         (throw-error "assertion failed"))))))
(when nil
  (sleep 1000)
  (Tst:msg "send SIGCHLD")
  (destroy pid 17) ; SIGCHLD
  (sleep 10000))

(Tst:msg "stop test server")
(destroy pid) ; SIGTERM

(Tst:end)
