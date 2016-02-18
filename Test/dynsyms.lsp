(set 'dynsym_nil nil
     'dynsym_Util Util)
(set 'Util:there_string "I'm there"
     'Util:there_nil nil)

(define (use-dynsym_nil)
  (println "dynsym_nil:notReachable: " dynsym_nil:notReachable))

(define (use-dynsym_Util_notThere)
  (println "dynsym_Util:notThere: " dynsym_Util:notThere))
(define (use-dynsym_Util_there_string)
  (println "dynsym_Util:there_string: " dynsym_Util:there_string))
(define (use-dynsym_Util_there_nil)
  (println "dynsym_Util:there_nil: " dynsym_Util:there_nil))
