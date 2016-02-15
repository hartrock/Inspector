(context 'Sys) ; system

;; [todo] signal C constants/macros: honor other platforms as Linux x86, too

(define (unsigned-char i)
  (& i 0xff))
(define (signed-char i
                     , (uc (unsigned-char i)))
  (if (<= uc 127)
      uc
      (- (++ (- 255 uc)))))

;; Linux x86: taken from
;;  man 7 signal
(constant 'SIGCHLD 17
          'SIGUSR1 10)

;; Linux x86; taken from
;;   /usr/include/x86_64-linux-gnu/bits/waitstatus.h

;;/* If WIFEXITED(STATUS), the low-order 8 bits of the status.  */
;;#define __WEXITSTATUS(status)   (((status) & 0xff00) >> 8)
(define (WEXITSTATUS status)
  (>> (& status 0xff00) 8))
;;/* If WIFSIGNALED(STATUS), the terminating signal.  */
;;#define __WTERMSIG(status)      ((status) & 0x7f)
(define (WTERMSIG status)
  (& status 0x7f))

;;/* Nonzero if STATUS indicates normal termination.  */
;;#define __WIFEXITED(status)     (__WTERMSIG(status) == 0)
(define (WIFEXITED status)
  (zero? (WTERMSIG status)))

;;/* Nonzero if STATUS indicates termination by a signal.  */
;;#define __WIFSIGNALED(status) \
;;  (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)
(define (WIFSIGNALED status)
  (> (>> (signed-char (++ (& status 0x7f)))
         1)
     0))

;; unsigned exit status
(define (exit-status status)
  (if (WIFEXITED status)
      (WEXITSTATUS status)))
;; signed exit status
(define (exit-code status
                   , (es (exit-status status)))
  (if es
      (signed-char es)))
(define (term-sig status)
  (if (WIFSIGNALED status)
      (WTERMSIG status)))


(define (getpid)
  (sys-info -3))

;; PID together with count for each new tmp filename shoud be sufficiently
;; unique
(setq tmp_fn_count 0)
(define (tmp_fn (suffix "") , res) ; opt suffix
  (++ tmp_fn_count)
  (append "/tmp/" (scriptname) "_" (string (getpid)) "_tmp_"
          (string (- tmp_fn_count 1)) ; first index 0
          suffix))

;;EOF
