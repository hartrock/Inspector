(load-libs 'assert)

(load-module "FOOPReference.lsp")

(new Class 'FM_NodeState)
(context FM_NodeState)

(constant
 's_count 1     ; advance count
 's_nodeId 2    ; 
 's_enteredBy 3 ; (count nodeId) from former node in evaluation or nil
 's_funcSym 4   ; later there may bo multiple in FM node defs to be choosen from
 's_arguments 5 ; (fm:advance <nodeId> ...): optional args for calling node func
 ;; Evaluation results set by (call-curr-func):
 ;; 1. curr evaluation:
 ;;   - only set, if there is no (fm:advance) in node func
 ;;   - flag needed, because funcVal may be nil from or without evaluation
 's_funcEvaluatedFlag 6
 's_funcVal 7
 ;; 2. alien evaluation of former node func (in case of using (fm:loop)):
 ;;   - set for 'next' node of *last* (fm:advance next ...) call in node func
 ;;     (there may be multiple (fm:advance ...) calls in node func)
 ;;     - after finishing its eval,
 ;;     - before eval of 'next' node func
 ;;   - inside eval of 'next' node func
 ;;       (:alienEvaluation (:curr-state fm))
 ;;     is then the same as
 ;;       (:lastEvaluation fm)
 ;;     , if:
 ;;       a) func's node has been reached by (fm:advance next ...) call *inside*
 ;;          eval of some node func, and
 ;;       b) func eval has not called (fm:advance next ...) itself.
 's_alienEvaluation 8
 ;; Note: straightforward for getting last FM evaluation is:
 ;; 1. to use:
 ;;   (:lastEvaluation fm), (:lastEvaluation-val fm), ...
 ;;   at begin of node func; or
 ;; 2. to use optional args from advancer func:
 ;;   (fm:advance <nodeID> <optArgs>)
 ;;   at (usually one and last) advance call.
 )

(define (FM_NodeState advanceCount id enteredBy funcSymOrNil argumentsOrNil)
  (cons (context) (list advanceCount id enteredBy funcSymOrNil argumentsOrNil
                        nil nil nil)))
(define (nodeId)
  (self s_nodeId))
(define (funcSym)
  (self s_funcSym))
(define (arguments)
  (self s_arguments))
(define (arguments?)
  (not (null? (self s_arguments))))
(define (set-arguments arguments)
  (setq (self s_arguments) arguments))
(define (funcVal)
  (self s_funcVal))
(define (set-funcVal val)
  (setq (self s_funcVal) val))
(define (funcEvaluated?)
  (self s_funcEvaluatedFlag))
(define (set-funcEvaluated flag)
  (setq (self s_funcEvaluatedFlag) flag))
(define (alienEvaluation)
  (self s_alienEvaluation))
(define (set-alienEvaluation val)
  (setq (self s_alienEvaluation) val))


(context MAIN)

(new FOOPReference 'FM)
(context FM)

(constant ; 0 for FOOP Class, 1 for FOOP reference context
 's_name 2 's_nodes 3 's_stats 4
 's_advanceCount 5
 's_curr_state 6 's_prev_state 7
 's_inEvaluation 8
 's_lastEvaluation 9 ;;'s_reached
)

;; each call creates a new flow machine FOOP, referenced by a ref context
;; default
(define (FM nameStrOrNil)
  (let (ref_context (new-FOOPReference)) ; generic; 0, 1
    ;; specific
    (extend ref_context
            (list
             (or nameStrOrNil (string ref_context)) ; 2
             (new-tree "nodes") ; 3
             (new-tree "stats") ; 4
             nil nil nil nil nil))
    (set ; forwards of ref context calls to FOOP Class
     (sym "advance" ref_context)
     (lambda (next)
       (if (args) ; forward opt args
           (eval (append '(:advance (context) next) (args)))
           (:advance (context) next)))
     (sym "curr" ref_context)           (lambda () (:curr (context)))
     (sym "curr-func" ref_context)      (lambda () (:curr-func (context)))
     (sym "call-curr-func" ref_context) (lambda () (:call-curr-func (context)))
     ;; 
     (sym "loop" ref_context)           (lambda () (:loop (context)))
     (sym "START" ref_context)
     (lambda ()
       (if (args) ; forward opt args
           (eval (append '(:START (context)) (args)))
           (:START (context))))
     (sym "nodes" ref_context)          (lambda () (:nodes (context))))
    ref_context))

;; override default for deleting 'sub' contexts
(define (delete-ref)
  (delete-context (self s_nodes))
  (delete-context (self s_stats))
  (delete-FOOPReference)) ; default

;; (add-node "foo")
;; (add-node "foo" "this is foo")
;; (add-node "foo" 'funcSym)
;; (add-node "foo" '("this is foo" 'funcSym))
(define (add-node id         ; if there is only an id ..
                  (node "")) ; .. ensure that something appears in node Tree
  (assert (not ((self s_nodes) id))) ; allow just one definition for each id
  ((self s_nodes) id node)
  ((self s_stats) id 0)) ; init, to get ++ below working
(define (add-nodes l)
  (dolist (e l)
          (if (= (length e) 1)
              (add-node (e 0)) ; only informational without node func
              (add-node (e 0) (e 1)))))

;; accessors
(define (name)
  (self s_name))
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
(define (lastEvaluation)
  (self s_lastEvaluation))
(define (set-lastEvaluation count_id_val_list)
  (setq (self s_lastEvaluation) count_id_val_list))
(define (lastEvaluation-count)
  ((lastEvaluation) 0))
(define (lastEvaluation-id)
  ((lastEvaluation) 1))
(define (lastEvaluation-val)
  ((lastEvaluation) 2))
;; accessors to FM_NodeState FOOP
(define (curr) ; FR iface
  (if (curr-state) (:nodeId (curr-state))))
(define (prev)
  (if (prev-state) (:nodeId (prev-state))))
(define (curr-arguments)
  (:arguments (curr-state)))
(define (curr-arguments?)
  (:arguments? (curr-state)))
(define (curr-func) ; FR iface
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
    (let (syms (filter symbol? nodeVal))
      (if syms (first syms))))))

;; high level iface
(define (advance next) ; FR iface, opt args forwarded to call of next node func
  ;;(println "args: " (args))
  (++ (self s_advanceCount))
  (logg:info (format "[%s %d] %21s -> %-21s"
                     (string (reference))
                     (advanceCount)
                     (string (curr)) next)) ; curr_ starts being nil
  (assert ((nodes) next)) ; node has to be known
  (assert (!= (curr) next))

  (setq (self s_prev_state) (curr-state))
  ;;(if nil ;(args)
  ;;    (dbg:expr (args)))
  (setq (self s_curr_state)
        (FM_NodeState (advanceCount) next (inEvaluation)
                      (h_funcSym-or-nil ((nodes) next)) (args)))
  ;;(dbg:expr (prev-state) (curr-state))
  (logg:info (format "[%s %d] %21s    %-21s"
                     (string (reference))
                     (advanceCount)
                     ""
                     (if (:funcSym (curr-state))
                         (string (:funcSym (curr-state)))
                         "(no func)")))
  (++ ((stats) (curr))))

(define (call-curr-func) ; FR iface
  (when (inEvaluation)
    (logg:fatal "implementation: recursive evaluation not allowed.")
    (exit 1))
  (when (:funcEvaluated? (curr-state))
    (logg:fatal "implementation: FM func " (:funcSym (curr-state)) " already evaluated.")
    (exit 1))
  (letn ((countBeforeEval (advanceCount))
         (inEval (cons countBeforeEval (curr))))
    (set-inEvaluation inEval)
    ;; DNW: (dbg:expr (self))
    (letn ((val
            (if (curr-arguments?)
                (eval (append
                       '((eval (curr-func)) (reference)) ;call with (self) ref
                       (curr-arguments)))
                ((eval (curr-func)) (reference))))
           (lastEval (push val inEval -1)))
      (set-lastEvaluation lastEval)
      ;; (curr-state) usually has been changed by (fm:advance ...) inside func
      (if (= countBeforeEval (advanceCount))
          (begin ; (curr-state) not changed:
            ;; mark for avoiding eval same node again
            (:set-funcEvaluated (self s_curr_state) true)
            (:set-funcVal (self s_curr_state) val))
          ;; eval is alien for node usually to be evaluated next
          (:set-alienEvaluation (self s_curr_state) lastEval)))
    (set-inEvaluation nil)))


;;
;; high level iface

;; loop beginning with curr node (to be set before by (fm:advance ...))
(define (loop) ; FR iface
  (while (!= (curr) "EXIT")
    (assert (curr-func))
    (call-curr-func))
  (when (curr-func)
    (call-curr-func)) ; optional EXIT func
  (lastEvaluation-val))

;;
;; function iface for complete FM

;; Preconditions: both START and EXIT nodes have to be defined;
;; - START node: with node func advancing to next node;
;; - EXIT  node: may be without node func.
;; Loop after advancing to START node: optional args are forwarded to
;; (fm:advance START ...) before start of looping, and then further to
;; (also opt) START node func.
;; By (optional) START arguments combined with returning last evaluation at EXIT
;; an FM can be used same way as a function: so it can serve as a replacement
;; of a complicated one; starting with tracking func's flow by just
;; - creating named, but empty, FM nodes; and
;; - adding (fm:advance <nextNodeID>) calls in to be tracked func
;; first.
(define (START) ; FR iface, opt args forwarded to call of (opt) START func
  (if (args)
      (eval (append '(advance "START") (args)))
      (advance "START"))
  (loop))


;; init

(define (initialize)
  (Tweak:context MAIN:FM))

;;EOF
