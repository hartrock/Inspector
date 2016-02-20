;; Remote's prompt-event cannot be given as command via stdin *after* loading
;; code given by user arguments: if user code will be interrupted (SIGINT),
;; Ctrl-c handler reads from stdin...
;; So set prompt-event in remote startup code.
;; Env var will be used for transferring pipe fd.
(set 'Init:Inspector.metaChannel (int (env "INSPECTOR_META_CHANNEL")))
(prompt-event (fn (ctx)
                (write-line ; ends output with '\n'
                 Init:Inspector.metaChannel ; write ..
                 "prompt-event") ; .. to meta channel, but ..
                nil)) ; .. leave the prompt unchanged
;;EOF
