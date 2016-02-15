;; patch of getopts
(module "getopts.lsp")

(context getopts)

;; helper
(define (opt_str o)
  (append (if (= (length o) 1) "-" "--")
          o))
(define (correspondingOrNil o)
  (if (= (length (o 1)) 4) (o 1 3)))
(define (opts_str o)
  (let (oc (correspondingOrNil o))
    (append (opt_str (o 0)) (if oc (append ", " (opt_str oc)) ""))))

;; Prints corresponding opts, too; if given '-f' or '--file'  it  prints '-f, --file ...' or '--file, -f ...',
;; if opts have been given by (shortlongopt '-f' '--file' ...).

(setq formatStrShorter "  %-16s  %s")
(setq formatStrLonger "  %-32s  %s")
(define (println_opt o) ; helper
  (println
   (format (if (empty? long) formatStrShorter formatStrLonger)
           (append (opts_str o)
                   (if (o 1 1) (append " " (o 1 1)) "")) ; param
           (o 1 2)))) ; desc

;; reverse of option lists avoided here, by pushing opts back in f_shortlongopt
(define (usage useStrOrNil)
  (println "Usage: " (Logger:scriptname) " " (or useStrOrNil "[options]"))
  (dolist (o short) (println_opt o))
  (dolist (o (clean correspondingOrNil long)) (println_opt o))
  (exit 0))

;; Idea is to treat long and short options with same meaning together; which is by giving them together in one call.
;; If there is a change later old corresponding options will be removed before installing the new ones: e.g.
;; replacement of '-a, -aaaa' and '-b, -bbbb' by another call with arguments '-a' and '-bbbb'
;; - gives '-a, -bbbb', and
;; - removes '-b' and '-aaaa'.
(define (f_shortlongopt sopt lopt action arg? desc)
  ;;(println "sopt: " sopt ", lopt: " lopt ", action: " action ", arg?: " arg? ", desc: " desc)
  (let ((a_lopt (assoc lopt getopts:long))) ; needed in case of looking into it after pop-assoc
    (if a_sopt
        (let (co (getopts:correspondingOrNil (assoc sopt getopts:short)))
          (pop-assoc co getopts:long))) ; may destroy old co opt info, so we need a_lopt below ..
    (if a_lopt
        (let (co (getopts:correspondingOrNil a_lopt)) ; .. here stored assoc is needed.
          (pop-assoc co getopts:short)))
    (if sopt
        (let (l_sopt (list sopt (list action arg? (or desc "") lopt)))
          (if (assoc sopt getopts:short)
              (setf (assoc sopt getopts:short) l_sopt)
              (push l_sopt getopts:short -1))))
    (if lopt
        (let (l_lopt (list lopt (list action arg? (or desc "") sopt)))
          (if (assoc lopt getopts:long)
              (setf (assoc lopt getopts:long) l_lopt)
              (push l_lopt getopts:long -1))))))


(context MAIN)

;; new macro
(define-macro (shortlongopt sopt lopt action arg? desc)
  (getopts:f_shortlongopt sopt lopt action arg? desc))

;; overwrites
(define-macro (shortopt sopt action arg? desc)
  (getopts:f_shortlongopt sopt nil action arg? desc))
(define-macro (longopt lopt action arg? desc)
  (getopts:f_shortlongopt nil lopt action arg? desc))
