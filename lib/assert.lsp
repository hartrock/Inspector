(load-libs 'Navi 'Algo 'Util 'Tweak)

(context 'assert)

(define (do-assert expr)
  (setq last_noERR (catch (eval expr) 'last_result))
  (setq last_ERR
        (if last_noERR
            nil
            (last-error))))
(define (do-asserts exprs stop-condition post-action)
  (setq last_exprs exprs)
  (dolist (expr exprs (stop-condition))
          (do-assert (setq last_expr expr)))
  (post-action))
(define (string-result-common loc)
  (append
   (if (> (length last_exprs) 1)
       (append ", amongst expressions: " (string last_exprs))
       "")
   (if loc ; callerSym, noInFunc
       (letn ((callerSym (loc 1)) (caller-fun (eval callerSym))
              (noInFunc (loc 0)))
         (if (not (or (lambda? caller-fun) (macro? caller-fun)))
             (append
              " (caller " (string callerSym) " is not a lambda/macro)")
             (append
              "\n-> " (string noInFunc) ". assert"
              " called from " (string callerSym))))
       ""))) ; no loc
(define (create-test-info)
  (format "(%d: %+d -%d)" (+ testOK testFail) testOK testFail))
(define (result-action msg failFlag testFlag loc)
  (let (testInfo
        (if testFlag  nil))
    (if failFlag
        (begin
          (++ failureCount)
          (if testFlag
              (begin
                (++ testFail)
                (print msg) ; NL below
                (println
                 ;;(if testVerbose (format "-----> %s\n" (string result)) "")
                 (format "%s
>>>>> -------- <<<<<
>>>>> Failure! <<<<< %s
>>>>> -------- <<<<<"
                         (test-prefix loc) (create-test-info))))
              (throw-error msg)))
        (begin
          (++ successCount)
          (if testFlag
              (begin
                (++ testOK)
                (println msg)
                (println (test-prefix loc) "--> OK " (create-test-info))))))
    (not failFlag)))
;;
(define (assert-empty-warning loc)
  (dbg:warn (format "%s with no expressions to be applied to: ignored %s"
                    (string assertSym) (string-result-common loc))))
(define (assert-exprs assertSym exprs testFlag loc)
  (if (null? exprs)
      (begin
        (assert-empty-warning loc)
        true)
      ;; neutral start condition
      (let ((last_result true) (last_noERR true)
            (failFlag nil) (msg "")
            (failCond (fn () (or (not last_noERR)
                                 (not last_result)))))
        (do-asserts
         exprs
         failCond ; loop enter cond
         (fn () ; action at last
           (setq failFlag (failCond))
           (if failFlag
               (setq
                msg (append
                     (if (not last_noERR) (format "[ERR %d] " (last_ERR 0)) "")
                     (append (string assertSym) " failed for expr: "
                             (string last_expr))
                     (string-result-common loc))))
           (result-action msg failFlag testFlag loc))))))
;;
(define (assert-exprs_ERR assertSym exprs errCode testFlag loc)
  (if (null? exprs)
      (begin
        (assert-empty-warning)
        true)
      ;; neutral start condition
      (let ((last_result nil) (last_noERR nil) (last_ERR (cons errCode nil))
            (failFlag nil) (msg "")
            (failCond (fn ()(or last_noERR
                                (and errCode (!= errCode (last_ERR 0)))))))
        (do-asserts
         exprs
         failCond ; loop enter cond
         (fn () ; action at last
           (setq failFlag (failCond))
           (if failFlag
               (setq
                failFlag true
                msg (append
                     (format "[ERR %s] " (if last_noERR
                                             "none"
                                             (string (last_ERR 0))))
                     (format "%s expecting ERR %s failed for expr: %s"
                             (string assertSym)
                             (if (nil? errCode)
                                 "some ERR"
                                 (format "ERR %d" errCode))
                             (string last_expr))
                     (string-result-common loc))))
           (result-action msg failFlag testFlag loc))))))

;;
;;;;;; Tests using assert:test
;;

(define (test-prefix loc)
  (format "[test]%-20s"
          (if (nil? loc)
              ""
              (format "[%s]" (LoggerExpr:tloc2string loc)))))
(define (test-on)
  (setq testMode true
        testCount 0 testOK 0 testFail 0))
(define (test-off)
  (setq testMode nil))
(define (print-stats)
  ;; (assert:do-tests t1)
  (println (format "\n%s (%d: %+3d -%d) %s"
                   (test-prefix) (+ testOK testFail) testOK testFail
                   (if (null? testFail) "=> Success." "=> Failure!"))))
(define (do-tests) ; funcSym args
  ;(println "(args): " (args))
  (pre (not (null? (args))))
  (test-on)
  (dolist (a (args)) (if (symbol? a) ((eval a)) (a)))
  (test-off)
  (++ testCountAll (+ testOK testFail))
  (++ testOKAll testOK)
  (++ testFailAll testFail)
  (print-stats)
  (= testCountAll testOKAll))

;; unused
(define (verbose-on)
  (setq testVerbose true))
(define (verbose-off)
  (setq testVerbose nil))


(define-macro (assert:assert) ; assert is ctx symbol; forwarded to here
  (assert-exprs 'assert (args)))
(define-macro (ERR errCode)
  (assert-exprs_ERR 'ERR (args)
                    (if (integer? errCode) errCode (eval errCode))))
(define-macro (test)
  (assert-exprs 'test (args) true))
(define-macro (testERR errCode)
  (assert-exprs_ERR 'testERR (args)
                    (if (integer? errCode) errCode (eval errCode))
                    true))
;; Alt:
;; (set (Util:add-postfix-to-sym "_tweaked" 'assert)
;;      (lambda-macro (loc)
;;        (assert-exprs 'assert (args)
;;                      nil loc)))
(define-macro (assert_tweaked loc)
  (assert-exprs 'assert (args)
                nil loc))
(define-macro (ERR_tweaked loc errCode)
  (assert-exprs_ERR 'ERR (args)
                    (if (integer? errCode) errCode (eval errCode))
                    nil loc))
(define-macro (test_tweaked loc)
  (assert-exprs 'test (args)
                true loc))
(define-macro (testERR_tweaked loc errCode)
  (assert-exprs_ERR 'testERR (args)
                (if (integer? errCode) errCode (eval errCode))
                true loc))

;; set symbols to assert:assert and assert:assert_tweaked
(set 'pre assert 'post assert)
(set 'pre_tweaked assert_tweaked 'post_tweaked assert_tweaked)
(set 'MAIN:assert_tweaked assert_tweaked) ; forward tweaked MAIN:assert
;;
(setq assertSyms
      (push 'MAIN:assert ; -> to get context sym instead of assert:assert
            '(assert pre post ERR test testERR))
      assertTweakedSyms
      (Util:add-postfix-to-syms "_tweaked" assertSyms))
(define (syms-cmp ignored e)
  (find e assertSyms))
(define (sym-cmp e)
  (syms-cmp nil e))
(define (any-pre-sym? symbol)
  (or (= symbol 'pre) (= symbol 'pre_tweaked)))


;; used for asserting do-tests, assert:ERR
;; Note: 'assert is assert:assert here (and no ctx ref)
(setq assertFuncs (append assertSyms assertTweakedSyms)
      testFuncs '(test-on test-off print-stats ; manually
                  do-tests)) ; calling the former


;; tools

(define (show-pre-recursive funcSym)
  ;;(reset)
  (let (ch (Algo:convex-hull (list funcSym) Util:calls-into-context))
    (dolist (s ch)
            (let (sig (append (string s)
                              " "
                              (join (map string (nth '(0) (eval s))) " ")
                              " "))
              (dbg:begin sig)
              (map (fn (r)
                     (dbg:msg (format
                                "    %s"
                                (string (nth (Navi:ref-parent r) (eval s))))))
                   (ref-all nil (eval s)
                            (fn (ignored s) (any-pre-sym? s))))
              (dbg:end (append " " (string s)))))))


;; init ctx
(setq initializePriority Init:prioHigh) ; tweak 'assert before others
(define (initialize)
  (dbg:begin 'initialize)
  (Tweak:context MAIN:assert)
  (dbg:end 'initialize))

;;EOF
