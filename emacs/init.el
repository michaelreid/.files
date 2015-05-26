;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;; 
;;    ;; ;; ;;  CUSTOM SETTINGS  ;; ;; ;;      ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Display battery percentage
(display-battery-mode)


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

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;; 
;;    ;; ;; ;;  MY CUSTOMISATION FILES  ;; ;; ;;    ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the python emacs kit
(load-file "~/.emacs.d/emacs-for-python/epy-init.el")

;; Add `my-customizations` directory to load path
(add-to-list 'load-path "~/.emacs.d/my-customizations")

;; Add powerline directory to load path
(add-to-list 'load-path "~/.emacs.d/my-customizations/my-powerline")

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
