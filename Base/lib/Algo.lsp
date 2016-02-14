(context 'Algo)

(define (together-with-neighbors elems neighborsFunc)
  ;; (dbg:expr elems)
  (let (neighbors '())
    (dolist (e elems)
            (extend neighbors (neighborsFunc e)))
    (unique (extend elems neighbors))))
;;
(define (convex-hull elems neighborsFunc)
  (local (elemsWithNeighbors)
    (while
        (difference ; seq agnostic
         (setq elemsWithNeighbors (together-with-neighbors ;compute before check
                                   elems
                                   neighborsFunc))
         elems)
      (setq elems elemsWithNeighbors))) ; prepare next step
  elems) ; loop may not be entered

;;EOF
