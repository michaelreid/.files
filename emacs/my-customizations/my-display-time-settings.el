;; Settings to display time in the modeline
(setq display-time-day-and-date t)
(setq display-time-interval 30)
(setq display-time-24hr-format nil)
(setq display-time-string-forms
      (quote
       ((if (and (not display-time-format)
		 display-time-day-and-date)
	    (format-time-string "%a %b %e   " now) "  ")
	(format-time-string
	 (or display-time-format
	     (if display-time-24hr-format "%H:%M" "%-I:%M%p")) now)
	         "    Load" load "    " (if mail " Mail" ""))))
