;; Set-up so that Emacs for server mode and to enable
;; easier use of emacsclient

(setq server-mode t)
(server-start)
(setq server-use-tcp t)
(setq server-window 'pop-to-buffer)
(setq server-socket-dir t)
