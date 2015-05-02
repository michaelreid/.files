;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;; Settings for using emacs-w3m: ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; based on http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html


;; set w3m as the default browser in emacs
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;; set the user agent as a mobile user-agent
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533)")

;; turn off the tab bar in emacs-w3m
(setq w3m-use-tab nil)

;; turn off the location/address bar in w3m
(setq w3m-use-header-line nil)

;; sets a custom 'M-x hn' function to quickly visit HackerNews
(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

;; function that appends 'http://' to a url provide
    (defun w3m-open-site (site)
      “Opens site in new w3m session with ‘http://’ appended”
      (interactive
       (list (read-string “Enter website address(default: w3m-home):” nil nil w3m-home-page nil )))
      (w3m-goto-url-new-session
       (concat “http://” site)))


;; custom function to Wikipedia search 
(defun wikipedia-search (search-term)
  “Search for SEARCH-TERM on wikipedia”
   (interactive
    (let ((term (if mark-active
		    (buffer-substring (region-beginning) (region-end))
		  (word-at-point))))
      (list
       (read-string
	(format “Wikipedia (%s):” term) nil nil term)))
    )
   (browse-url
    (concat
     “http://en.m.wikipedia.org/w/index.php?search=”
      search-term
      ))
         )

;; Can we pass the -M argument to the w3m command (Monochrome)?
;; (setq w3m-command-arguments-alist "-M")
   

