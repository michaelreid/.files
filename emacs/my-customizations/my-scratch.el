;; Turn off the scratch buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Remove the Scratch Buffer
(kill-buffer "*scratch*")
;; Remove the Messages Buffer
(kill-buffer "*Messages*")

;; Initial Scratch message is `nil` (if it loads)
(setq initial-scratch-message nil)
