;; Ruby Master - Ruby Tapas
;;
;; http://devblog.avdi.org/2011/10/11/rvm-el-and-inf-ruby-emacs-reboot-14/

;; First setup rvm
(require 'rvm)
(rvm-use-default)

;; Then add hook
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

;; Ensuring PATH variable is available on MacOSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
