;; Variables to setup Emacs IRC for freenode auto-login

(load "~/.emacs.d/my-customizations/my-erc-pass.el")

(setq erc-autojoin-channels-alist (quote (("freenode.net" "#python" "#clojure"))))
(setq erc-autojoin-mode t)
(setq erc-hide-list (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "Users on *")))
(setq erc-nick "michaelreid")


