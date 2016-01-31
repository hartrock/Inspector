(load-libs 'assert 'Util)


(context Navi)

;; valid ref? Checks for having integer elements only in list.
;; Preconditions: none.
(define (ref? r)
  (and (list? r)
       ;(all-satisfy r (fn (e) (integer? e)))))
       (dbg:expr Util)
       (Util:all-satisfy r integer?)))

;; Valid list ref?
;; Checks, if r is a valid ref into list l.
;; Try to avoid exceptions in case of checking validity of sibling indices by
;; deref'ing parent and looking from there.
;; Preconditions: see assert:pre.
(define (list-ref? l r)
  (assert:pre (list? l)) ; ref may be nil
  (and (ref? r)
       (or (null? r) ; ref to l
           (let (pr (ref-parent r))
             (and
              (catch (l pr) 'res)
              (list-index? (l pr) (last r)))))))
;; (list-ref? l r): r; nil otherwise
;; Preconditions: (list? l), checked by callee.
(define (list-ref l r)
  (if (list-ref? l r)
      r))
;;
(define (list-index? l ix)
  (assert:pre (and (list? l) (integer? ix)))
  (let (len (length l))
    (and (<= (- len) ix) (< ix len))))

; 0: '() (root), 1: root+1, ..., (length r): r; -1: r-1, ..., -(length r): '()
(define (ref-from-root r n)
  (assert:pre (and (ref? r) (integer? n)))
  (if (> (abs n) (length r))
      nil
      (0 n r)))
; 0: r, 1: r-1, ..., (length r): '() (root); -1: root+1, -(length r): r
(define (ref-to-root r n)
  (assert:pre (and (ref? r) (integer? n)))
  (if (zero? n)
      r
      (if (> (abs n) (length r))
          nil
          (0 (- n) r))))

(define (h_dopath ref-dir-root r fun border (breakCond nil))
  ;;(begin (dbg:expr r) true)
  (assert:pre (and (ref? r) (integer? border)))
  (let (breakCondFun
        (if breakCond
            (fn (ix) (let (p (ref-dir-root r ix)) (breakCond p)))
            (fn (ix) (dbg:expr ix) nil)))
    (if (> (abs border) (length r))
        nil ; do nothing
        (if (>= border 0)
            (for (ix border (length r) 1 (breakCondFun (int ix)))
                 (fun (ref-dir-root r (int ix))))
            (for (ix 0 (+ (length r) border) 1 (breakCondFun (int ix)))
                 (fun (ref-dir-root r (int ix))))))))

(define (dopath-from-root r fun (border 0) (breakCond nil))
  (h_dopath ref-from-root r fun border breakCond))
(define (dopath-to-root r fun (border 0) (breakCond nil))
  (h_dopath ref-to-root r fun border breakCond))

(define (h_sub-paths-from-or-to-root r pushIx)
  (assert:pre (ref? r))
  (let (res '())
    (dotimes (ix (++ (length r)))
             (push (0 ix r) res pushIx))
    res))
(define (sub-paths-from-root r)
  (h_sub-paths-from-or-to-root r -1))
(define (sub-paths-to-root r)
  ;(dbg:expr r)
  (h_sub-paths-from-or-to-root r 0))

(define (dopath-from-or-to-root sym_path_breakOrNil body funcSym)
  ;;(dbg:expr sym_path_breakOrNil (length sym_path_breakOrNil))
  ;;(dbg:expr util)
  (eval (Util:create-dolist (sym_path_breakOrNil 0) ; sym
                            (list funcSym (sym_path_breakOrNil 1)) ; list
                            (if (>= (length sym_path_breakOrNil) 3)
                                (sym_path_breakOrNil 2)) ; break cond
                            body)))
(define-macro (dopath-from-root sym_path_breakOrNil body)
  ;;(dbg:expr sym_path_breakOrNil)
  (dopath-from-or-to-root sym_path_breakOrNil body 'sub-paths-from-root))
(define-macro (dopath-to-root sym_path_breakOrNil body)
  (dopath-from-or-to-root sym_path_breakOrNil body 'sub-paths-to-root))


;; Go to parent; nil for non-existing parent.
;; Precondition: (ref? r), checked by callee.
(define (ref-parent r)
  (ref-to-root r 1))
;; Go to child ix (default 0 for first one).
;; Precondition: see assert:pre.
(define (ref-child r (ix 0))
  (assert:pre (and (ref? r) (integer? ix)))
  (push ix r -1))

;; if parent ref is valid: ref to parent; nil otherwise
;; Preconditions: (list? l), (ref? r); checked by callees.
(define (list-ref-parent l r)
  (list-ref l (ref-parent r)))
;; if child ref (given by ix) is valid: ref to child; nil otherwise
;; Preconditions: (list? l), (ref? r), [opt] (integer? ix); checked by callees.
(define (list-ref-child l r (ix 0))
  (list-ref l (ref-child r ix)))
;;

(define (ref-index r ix)
  (assert:pre (and (ref? r) (integer? ix) (not (null? r))))
  (setq (last r) ix)
  r)
(define (list-ref-index l r ix)
  (list-ref l (ref-index r ix)))

;; helper: sign-switch, if not both are the same in comparison >= 0
(define (sign-switch? a b)
  (!= (>= a 0) (>= b 0)))

;; take care of crossing list 'border' (sign switch)
(define (ref-offset r off)
  (assert:pre (integer? off))
  (and (ref? r) ; valid ref
       (not (null? r)) ; valid parent
       (integer? off) ; valid offset
       (if (zero? off) ; no change?
           r
           (let (old_ix (last r))
             (if (sign-switch? old_ix (++ (last r) off))
                 nil
                 r)))))

;; do not ret nil
(define (ref-first r)
  (ref-index r 0))
(define (ref-last r)
  (ref-index r -1))
;; may ret nil
(define (ref-next r)
  (ref-offset r 1))
(define (ref-prev r)
  (ref-offset r -1))
;; check against list
(define (list-ref-offset l r off)
  (list-ref l (ref-offset r off)))
(define (list-ref-next l r)
  (list-ref l (ref-next r)))
(define (list-ref-prev l r)
  (list-ref l (ref-prev r 1)))

;; preds
(define (ref-first? r)
  (assert:pre (ref? r))
  (= (last r) 0))
(define (ref-last? r)
  (assert:pre (ref? r))
  (= (last r) -1))

;; preds
(define (list-ref-first? l r)
  (assert:pre (and (list? l) (ref? r)))
  (let (rp (ref-parent r) ix (last r))
    (and (list-ref? l rp)
         (or (= ix 0) (= ix (- (length (l rp))))))))
(define (list-ref-last? l r)
  (assert:pre (and (list? l) (ref? r)))
  (let (rp (ref-parent r) ix (last r))
    (and (list-ref? l rp)
         (or (= ix -1) (= ix (-- (length (l rp))))))))

;; ref to
;; - elem in root list: 0
;; - root list: nil
;; preds
(define (ref-dimension r)
  (assert:pre (ref? r))
  (length r))
;; preds
(define (ref-nested? r)
  (> (ref-dimension r) 1))
(define (ref-apply-position? r)
  (and (ref-nested? r) (zero? (last r))))


;; init &&

(define (initialize)
  (dbg:begin 'initialize)
  (Tweak:context Navi)
  (dbg:end 'initialize))

;;EOF
