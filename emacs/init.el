;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;; 
;;    ;; ;; ;;  PACKAGES MGMT  ;; ;; ;;        ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define package repositories
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))



; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)


;; Define list of local packages that need to install if not alrady installed
;; to make transportable across systems
;;
;; For all environments:
;; - projectile
;; - auto-complete
;; - s
;; - smart-mode-line
;; - multiple-cursors
;; - no-easy-keys
;; - ace-jump-mode (quickly jump to spot in file)
;; - magit (git interface)
;; - helm (M-x replacement)
;; - helm-dash (Use helm to search Dash docsets)
;; - project explorer (project sidebar)
;; - w3m (requires w3m installed in apt)
;;
;; For Python Environment:
;; - epc
;; - jedi
;;


;; This method from jedi-starter.el github user wernerandrew
;; https://raw.githubusercontent.com/wernerandrew/jedi-starter/master/jedi-starter.el
(defvar local-packages '(projectile auto-complete epc jedi s smart-mode-line magit
				    multiple-cursors no-easy-keys ace-jump-mode 
				    helm project-explorer w3m helm-dash))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

;; This delightful bit adapted from:
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;; 
;;    ;; ;; ;;  MY CUSTOMISATION FILES  ;; ;; ;;    ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the python emacs kit
(load-file "~/.emacs.d/emacs-for-python/epy-init.el")

;; Add `my-customizations` directory to load path
(add-to-list 'load-path "~/.emacs.d/my-customizations")

;; Set-up to use cut and paste in Emacs in terminal
(load "my-cut-paste.el")

;; Settings for using emacs-w3m
(load "my-emacs-w3m-settings.el")

;; Settings for Emacs server
(load "my-server-mode.el")

;; Session Management file
(load "my-session-mgmt.el")

;; Settings to load *scratch* buffer
(load "my-scratch.el")

;; Settings to autoload smex
(load "my-smex.el")

;; Load smart-mode-line settings
(load "my-safe-mode-line.el")

;; Load smart-mode-line settings
(load "my-display-time-settings.el")

;; Ace-jump mode settings to jump to point in buffer with keyboard
(load "my-ace-jump-mode.el")

;; Load helm-mode and config
(load "my-helm-mode-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;; 
;;    ;; ;; ;;  MY CUSTOM SETTINGS  ;; ;; ;;      ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off the Menu Bar
(menu-bar-mode -1)

;; Unbind C-z for to avoid suspending
;; the session in tmux 
(global-unset-key "\C-z")

;; Don't lock files
(setq create-lockfiles "nil")

;; All yes or no queries to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Set column number mode on
(setq column-number-mode t)

;; Display time mode on
(display-time-mode)

;; Disable arrow keys and others
(require 'no-easy-keys)
(no-easy-keys 1)

;; Display battery percentage
(display-battery-mode)

;; Start ido-vertical mode auto
(ido-vertical-mode t)

;; Require and start helm
;; (require 'helm)
;; (require 'helm-config)
;; (helm-mode t)

;; Projectile
(projectile-global-mode)
(setq projectile-enable-caching t)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(magit-item-highlight-face nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "color-33" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "brightred"))))
 '(font-lock-function-name-face ((t (:foreground "brightcyan"))))
 '(font-lock-keyword-face ((t (:foreground "color-200"))))
 '(font-lock-string-face ((t (:foreground "color-135"))))
 '(magit-diff-add ((t (:foreground "color-40"))) t)
 '(magit-diff-del ((t (:foreground "red"))) t)
 '(magit-diff-file-header ((t (:foreground "brightyellow"))) t)
 '(magit-diff-hunk-header ((t (:foreground "brightwhite"))) t)
 '(magit-item-highlight ((t (:background "color-20" :foreground "brightwhite"))) t)
 '(magit-item-mark ((t (:background "color-20" :foreground "brightwhite"))) t)
 '(magit-log-head-label-bisect-bad ((t (:background "color-20" :foreground "red" :box 1))) t)
 '(minibuffer-prompt ((t (:foreground "color-27")))))
