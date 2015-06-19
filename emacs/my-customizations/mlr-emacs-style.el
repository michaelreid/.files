;; MLR Customizing the theme of Emacs


;; Set the font for emacs
;; (set-frame-font "Bitstream Vera Sans Mono")

;; Set 11pt font
(set-face-attribute 'default nil :height 110)

;; Set color theme
(require 'color-theme)
(require 'color-theme-molokai)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-molokai)))

;; Disable smartmodeline
(setq sml/theme 'dark)

;; Powerline settings from EmacsWiki
(require 'powerline)

(powerline-default-theme)

;; Tweak powerline setup:
(set-face-attribute 'powerline-active1 nil
		    :foreground "#bbbbbb"
		    :background "#111111")

(set-face-attribute 'powerline-active2 nil
		    :foreground "#000000"
		    :background "#00f0ff")

(set-face-attribute 'powerline-inactive1 nil
		    :foreground "#00f0ff"
		    :background "#111111")

(set-face-attribute 'powerline-inactive2 nil
		    :foreground "#111111"
		    :background "#555555")

(set-face-attribute 'mode-line nil :foreground "#00f0ff")


(setq powerline-default-separator 'slant)
(setq powerline-height 20)
(setq powerline-text-scale-factor 1.6)
(setq powerline-display-hud t)


;; Tweak molokai theme's fringes and cursor
(set-face-attribute 'cursor nil :background "#00ff00")
(set-face-attribute 'fringe nil :background "#1B1D1E")


;; Tweak helm display
(set-face-attribute 'helm-source-header nil
		    :height 1.7
		    :foreground "#00f0ff"
		    :background nil)
