(defcustom joblog/file
  (file-name-concat org-directory "jobs.txt")
  "Filename containing a log of job applications."
  :type 'file)

(defun joblog/read-date ()
  "Read a date from the calendar."
  ;; https://stackoverflow.com/a/27678211/9184674
  (let* ((adv '(lambda (fn &rest args)
		 (let* ((date (calendar-cursor-to-date))
			(year  (nth 2 date))
			(month (nth 0 date))
			(day   (nth 1 date)))
		   (setq joblog/calendar-selection
			 (format "%s/%s/%s" year month day)))
		 (funcall fn args)
		 (exit-recursive-edit))))
    (advice-add 'calendar-exit :around adv)
    (calendar)
    (message "Select the desired date, then type `q' to exit.")
    (recursive-edit)
    (advice-remove 'calendar-exit adv)
    joblog/calendar-selection))

(defun joblog/log ()
  "Prompt the user to log a job application.
The information provided by the user will be inserted at the very
top of `joblog/file'."
  (interactive)
  (let ((company (read-string "Company: "))
	(title (read-string "Job title: "))
	(location (read-string "Location: "))
	(date (joblog/read-date)))
    (with-current-buffer (find-file-noselect joblog/file)
      (goto-char (point-min))
      (insert (format "%s: %s (%s) -- %s\n" company title date location))
      (save-buffer))))

(provide 'joblog)
