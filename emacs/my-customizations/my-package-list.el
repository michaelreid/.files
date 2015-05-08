;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                       ;; 
;;    ;; ;; ;;  MY PACKAGES  ;; ;; ;;    ;;
;;                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Settings to make using Emacs easier   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SMEX: a `M-x enhancement for Eamcs	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'smex "smex")

(global-set-key (kbd "M-x") 'smex)

;; smex setting so that space inserts a hyphen
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
	 `(lambda ()
	    (interactive)
	    (if (string= " " (this-command-keys))
		(insert ?-)
	      (funcall ,ido-cannot-complete-command)))))
    ad-do-it))


;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
	   (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
	    (acronym-matches (list))
	    (remove-regexes '("-menu-")))
	;; Creating the list of the results to be set as first
	(dolist (item items)
	  (if (string-match (concat regex "[^-]*$") item) ;; strict match
	      (add-to-list 'acronym-matches item)
	    (if (string-match regex item) ;; appending relaxed match
		(add-to-list 'acronym-matches item t))))

	;; Filtering ad-return-value
	(dolist (to_remove remove-regexes)
	  (setq ad-return-value
		(delete-if (lambda (item)
			     (string-match to_remove item))
			   ad-return-value)))

	;; Creating resulting list
	(setq ad-return-value
	      (append acronym-matches
		      ad-return-value))

	(delete-dups ad-return-value)
	(reverse ad-return-value))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Enabled modes prefered on startup    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Editing environment   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


