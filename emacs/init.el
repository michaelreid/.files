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
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
(load-file "~/.emacs.d/emacs-for-python/epy-init.el")

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;; 
;;    ;; ;; ;;  MY CUSTOMIZATIONS  ;; ;; ;;    ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add `my-customizations` directory to load path
(add-to-list 'load-path "~/.emacs.d/my-customizations")

;; Set-up to use cut and paste in Emacs in terminal
(load "my-cut-paste.el")

;; Set-up Emacs IRC settings 
(load "my-erc-settings.el")

;; Settings for using emacs-w3m
(load "my-emacs-w3m-settings.el")

;; Settings for Emacs server
(load "my-server-mode.el")

;; Session Management file
(load "my-session-mgmt.el")

;; Not load with *scratch* buffer
(load "my-scratch.el")

;; Turn Off the Menu Bar
(menu-bar-mode -1)

;; Unbind C-z for to avoid suspending
;; the session in tmux 
(global-unset-key "\C-z")

;; Don't lock files
(setq create-lockfiles "nil")

;; All yes or no queries to 'y or n'
(fset 'yes-or-no-p 'y-or-n-p)
