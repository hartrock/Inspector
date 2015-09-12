(dbg:on)
(dbg:begin "FM.lsp")

(load-libs 'assert)

;(dbg:on)

(load-module "FOOPReference.lsp")

(new Class 'FM_NodeState)
(context FM_NodeState)

(constant
 's_count 1 's_nodeId 2 ; name to be computed from node_id
 's_enteredBy 3  ; node (count nodeId) node func in evaluation or nil
                                        ; ('s_inEvaluation)
 's_funcSym 4 ; later there may bo multiple in FM node defs to be choosen from
 ;; flag needed, because funcVal may be nil from or without evaluation
 's_funcEvaluatedFlag 5 's_funcVal 6            ; own
 's_alienFuncEvaluatedFlag 7 's_alienFuncVal 8 ; alien ; 's_eval_origin
)
(define (FM_NodeState advanceCount id enteredBy funcSymOrNil)
  (cons (context) (list advanceCount id enteredBy funcSymOrNil
                        nil nil nil nil)))
(define (nodeId)
  (self s_nodeId))
(define (funcSym)
  (self s_funcSym))
(define (funcVal)
  (self s_funcVal))
(define (set-funcVal val)
  (setq (self s_funcVal) val))
(define (funcEvaluated?)
  (self s_funcEvaluatedFlag))
(define (set-funcEvaluated flag)
  (setq (self s_funcEvaluatedFlag) flag))
(define (alienFuncVal)
  (self s_alienFuncVal))
(define (set-alienFuncVal val)
  (setq (self s_alienFuncVal) val))
(define (alienFuncEvaluated?)
  (self s_alienFuncEvaluatedFlag))
(define (set-alienFuncEvaluated flag)
  (setq (self s_alienFuncEvaluatedFlag) flag))

(context MAIN)

(new FOOPReference 'FM)
(context FM)

(constant ; 0 for FOOP Class, 1 for FOOP reference context
 's_nodes 2 's_stats 3
 's_advanceCount 4
 's_curr_state 5 's_prev_state 6
 's_inEvaluation 7 ;'s_reached
)

(define (tree-sym ix nameStr)
  (sym (string (context) "_" ix "_" nameStr) MAIN))

;; each call creates a new flow machine FOOP, referenced by a ref context
;; default
(define (FM)
  (letn ( ;; generic
         (ref_context (new-ref-context))
         ;; specific
         (foop (list (context)   ; 0: FOOP Class
                     ref_context ; 1: FOOP reference context
                     (new Tree (tree-sym foopCount "nodes")) ; 2 ; foopCount ..
                     (new Tree (tree-sym foopCount "stats")) ; 3 ; .. from FR
                     nil nil nil nil)))
    ;; set ref context default to FOOP
    (set (sym (string ref_context) ref_context) foop)
    ;; forward ref context call to FOOP Class
    (set (sym "advance" ref_context)
         (lambda (next) (:advance (context) next)))
    (set (sym "loop" ref_context)
         (lambda () (:loop (context))))
    ref_context))

(define (delete-all-instances)
  (let (ix foopCount) ; local ix taking and not changing val of outer
    (while (> ix 0)
      (let ((nodesSym (tree-sym ix "nodes"))
            (statsSym  (tree-sym ix "stats")))
        (delete nodesSym) (delete nodesSym)
        (delete statsSym)  (delete statsSym))
      (-- ix)))
  ;; counts foopCount down to zero: so do this *after* using foopCount ..
  (delete-all-refs)) ; .. for cleaning FM specific things above

;; (add-node "foo")
;; (add-node "foo" "this is foo")
;; (add-node "foo" 'funcSym)
;; (add-node "foo" '("this is foo" 'funcSym))
(define (add-node id (node id))
  ;;(dbg:expr id node)
  (assert (not ((self s_nodes) id)))
  ((self s_nodes) id node)
  ((self s_stats) id 0)) ; init, to get ++ below working
(define (add-nodes l)
  (dolist (e l)
          (add-node (e 0) (e 1))))

;; accessors
(define (nodes)
  (self s_nodes))
(define (FM:stats) ; override MAIN:stats
  (self s_stats))
(define (advanceCount)
  (self s_advanceCount))
(define (curr-state)
  (self s_curr_state))
(define (prev-state)
  (self s_prev_state))
(define (inEvaluation)
  (self s_inEvaluation))
(define (set-inEvaluation count_id_list)
  (setq (self s_inEvaluation) count_id_list))
;; accessors to FM_NodeState FOOP
(define (curr)
  (if (curr-state) (:nodeId (curr-state))))
(define (prev)
  (if (prev-state) (:nodeId (prev-state))))
(define (curr-func)
  (:funcSym (curr-state)))

(define (h_description nodeVal)
  (cond
   ((string? nodeVal)
    nodeVal)
   ((symbol? nodeVal
    ""))
   ((list? nodeVal) ; be robust: first found string in list or ""
    (let (strings (filter string? nodeVal))
      (if strings (first strings) "")))
   ("")))
(define (h_funcSym-or-nil nodeVal)
  (cond ;& empty list check
   ((symbol? nodeVal)
    nodeVal)
   ((list? nodeVal)
    (first nodeVal))))

(define (advance next)
  (++ (self s_advanceCount))
  (logg:info (format "[%s %d] %21s -> %-21s"
                     (string (reference))
                     (advanceCount)
                     (string (curr)) next)) ; curr_ starts being nil
  (assert ((nodes) next)) ; node has to be known
  (assert (!= (curr) next))

  (setq (self s_prev_state) (curr-state))
  (setq (self s_curr_state)
        (FM_NodeState (advanceCount) next (inEvaluation)
                      (h_funcSym-or-nil ((nodes) next))))
  ;;(dbg:expr (prev-state) (curr-state))
  (logg:info (format "[%s %d] %21s    %-21s"
                     (string (reference))
                     (advanceCount)
                     ""
                     (if (:funcSym (curr-state))
                         (string (:funcSym (curr-state)))
                         "(no func)")))
  (++ ((stats) (curr))))

(define (eval-curr-func)
  (if (inEvaluation)
      (logg:fatal "implementation: recursive evaluation not allowed."))
  (if (:funcEvaluated? (curr-state))
      (logg:fatal "implementation: FM func " (:funcSym (curr-state)) " already evaluated."))
  (set-inEvaluation (cons (advanceCount) (curr)))
  (let (val ((eval (curr-func)) (reference))) ; call with (self) ref
    (if (= (inEvaluation) (cons (advanceCount) (curr)))
      (setq
       (:set-funcEvaluated (self s_curr_state) true)
       (:set-funcVal (self s_curr_state) val))
      (setq
       (:set-alienFuncEvaluated (self s_curr_state) true)
       (:set-alienFuncVal (self s_curr_state) val))))
  (set-inEvaluation nil))

(define (loop)
  (while (!= (curr) "EXIT")
    (assert (curr-func))
    (eval-curr-func)))

;; init

(define (initialize)
  (dbg:begin 'initialize)
  (tweak:context MAIN:FM)
  (dbg:end 'initialize))


(context MAIN)

(dbg:end "FM.lsp")
