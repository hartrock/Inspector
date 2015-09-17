#!/usr/bin/env newlisp
;; mimic .init.lsp if not loaded
(cond
 ((not Init:Init) ; first guard
  ;; second guard for not loading modules/init.lsp twice: allow load of it ..
  (set 'Init_allowLoadOnce true ; .. below, guard deleted in modules/Init.lsp
       'Init:nodebug true) ; nil for more debug messages
  ;; script dir detection
  (set 'Inspector:scriptname "startIt_pingPong.lsp"
       'Inspector:dir ; be robust against CLI args not containing scriptname
       (0 (- (length Inspector:scriptname)) ; only leave dirpath
          (first (filter (fn (a) (find Inspector:scriptname a))
                         (main-args)))))
  (if (null? Inspector:dir)
      (set 'Inspector:dir ".")) ; cwd
  ;; Inspector:dir to be used as Init:newlisp_dir: needs rel modules/ and lib/
  (set 'Init:newlisp_dir Inspector:dir)
  (set 'Init:moduledir (append Init:newlisp_dir "/modules"))
  (set 'Init:libdir (append Init:newlisp_dir "/lib"))
  (load (append Init:moduledir "/Init.lsp"))
  (set 'Init:Init true))
 ("default" ; .init.lsp loaded
  (set 'Inspector:dir (append Init:newlisp_dir "/Inspector"))))

;;
;; handle CLI arguments

(load-module "getopts_patched_Ted.lsp")

(longopt "port" ; shortopt -p interferes with newlisp opt!
         (set 'WS:server_port (int getopts:arg)) "<port>"
         "e.g. 80 oder 8080 (default: 8080")

(shortlongopt "h" "help" (getopts:usage useStr) nil "print this help message")

(set 'arguments (getopts (2 (main-args))))

(load (append Inspector:dir "/Inspector.lsp"))

;(Inspector:start)

(define (aLoop)
  (set 's "-->")
  (set 'end 20)
  (for (anIx 1 end)
       ;; do some more interesting stuff ;-)
       (set 'b (* 2 anIx) 'c (+ 1 anIx) 'd (* b c) 'e (* anIx anIx))
       (set 's (dup s))
       (println ">>>>>>>>>>>>>>>>>>>>>"
                [text]
In words reaching ping-pong mode sounds more complicated than it is by just
doing it. But here are following the steps needed.

1. Run
     ./startIt_pingPong.lsp
   : it calls
     (Inspector:start)
   → control goes to browser GUI.

2. a) Go to
        http://localhost:8080/symbols.html?pingPong#MAIN:aLoop
      (this is searching a lambda/macro/list symbol of interest in corresponding
      [ctx] folder).
   b) Double-click (or select & Return) MAIN:aLoop, to create a
        '[lambda] MAIN:aLoop'
      folder containing symbols of this lambda at *top* of tree control (in
      front of all other ([ctx]) folders) - so far no ping-pong -.
   c) Go to created folder at top and *open* it: this is the *trigger* for
      starting ping-pong (Note: if there are more user created folders at top
      *all* have to be opened).

3. Now the following happens:
   a) Inspector GUI loads symbols in top folder from Inspector server and
      visualizes their current evaluations;
   b) thereafter it releaes the server by sending a command for finishing its
      webservice;
   c) server process is free to do other stuff for a while - in the meantime GUI
      polls to wait for this to happen -;
   d) until it gives back control to GUI by calling (Inspector:start) again.

   In the example this will be done repeatedly in a simple counting loop.
[/text]
">>>>>>>>>>>>>>>>>>>>>"
" anIx: " anIx "\n")

       (Inspector:start)))

(aLoop)
(println ">>>>>>>>>>>>>>>>>>>>>")
(println "Return to newLISP interpreter loop.")
(println Inspector:help)
(println "<<<<<<<<<<<<<<<<<<<<<")
