;;; joblog.el --- Job application logging utility -*- lexical-binding: t -*-

;; Copyright (C) 2024 Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; License: ISC - See LICENSE for legalese
;; Version: 2024.12.03

(require 'calendar)
(eval-when-compile
  (require 'rx))

(defgroup joblog ()
  "Log your job applications."
  :prefix "joblog-"
  :group 'convenience)

(defcustom joblog-status-list
  '("Rejected" "Interviewed" "Accepted")
  "List of job status strings."
  :type '(repeat string))

(defcustom joblog-mark-old-entries nil
  "Marks old entries with an overlay."
  :type 'boolean)

(defcustom joblog-file nil
  "Filename containing a log of job applications."
  :type 'file
  :set '(lambda (sym val)
	  (set-default-toplevel-value sym val)
	  (when val
	    (unless (file-exists-p val)
	      (with-temp-buffer (write-file val)))
	    (add-to-list 'auto-mode-alist
			 (cons (rx (eval (file-name-nondirectory val)) eos)
			       (quote joblog-mode))))
	  val))

(defcustom joblog-recent-entry-interval 14
  "Number of days below which a joblog entry is considered recent.
This variable is used by `joblog-recent-entries' to limit
completions to a subset that is the most recent.  This variable
should only matter to you if you set `joblog-visit-predicate' to
`joblog-recent-entries'."
  :type 'integer)

(defvar joblog--entry-overlay nil
  "Stores the overlay that marks old entries.")

(defvar joblog--calendar-selection nil
  "Stores dates selected from the calendar.")

(defconst joblog--company-regexp
  (rx bol (group (one-or-more nonl)) ":")
  "Matches the name of a company.")

(defconst joblog--date-regexp
  (rx (and "(" (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)) ")"))
  "Matches the date of a log entry.")

(defconst joblog--location-regexp
  (rx (and "--" (one-or-more whitespace) (group (one-or-more nonl)) (or "#" eos)))
  "Matches the location.")

(defun joblog--status-regexp ()
  (concat "<" (regexp-opt joblog-status-list) ">"))

(defface joblog-company-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for company names."
  :group 'joblog)

(defvar joblog-company-face 'joblog-company-face)

(defface joblog-date-face
  '((t (:inherit font-lock-constant-face)))
  "Face for dates."
  :group 'joblog)

(defvar joblog-date-face 'joblog-date-face)

(defface joblog-status-face
  '((t (:inherit font-lock-doc-face)))
  "Face for job status."
  :group 'joblog)

(defvar joblog-status-face 'joblog-status-face)

(defface joblog-location-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for job location."
  :group 'joblog)

(defvar joblog-location-face 'joblog-location-face)

(defconst joblog--font-lock-defaults
  (list (list (cons (rx "#" (zero-or-more nonl) eol)
		    font-lock-comment-face)
	      (cons joblog--company-regexp
		    joblog-company-face)
	      (cons joblog--date-regexp
		    joblog-date-face)
	      (cons (rx (regexp (joblog--status-regexp)))
		    joblog-status-face)
	      (cons joblog--location-regexp
		    joblog-location-face)))
  "Specifies the fontification properties of `joblog-mode'.")

(defun joblog--day-difference (date)
  "Return the difference between DATE and the current time.
DATE is a valid IS 8601 date string."
  (let ((unit (* 24 60 60))
	(then (date-to-time date))
	(now (encode-time (parse-time-string (current-time-string)))))
    (/ (time-subtract now then)
       unit)))

(defun joblog-recent-entries (entries)
  "Return ENTRIES no higher than `joblog-recent-entry-interval'."
  (seq-filter
   (lambda (entry)
     (< (joblog--day-difference
	 (progn (string-match joblog--date-regexp entry)
		(match-string-no-properties 1 entry)))
	joblog-recent-entry-interval))
   entries))

(define-error 'joblog-empty-file
	      "Create some entries first with \\[joblog].")

(defun joblog--history (buffer regex &optional subexp)
  "Return all occurences of REGEX in BUFFER.
This is a generic function, callers should manually perform any
required string manipulation directl on the list."
  (let ((subexpression (or subexp 0))
	(results nil))
    (save-excursion
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (push
	   (buffer-substring-no-properties
	    (match-beginning subexpression)
	    (match-end subexpression))
	   results))))
    results))

(defun joblog--entry-list (buffer)
  "Return a list of all entries in BUFFER."
  (joblog--history buffer
		   (rx bol (and (not "#")
				(+ nonl)) eol)))

(defun joblog--company-list (buffer)
  "Return an list of all previously entered companies as symbols.
BUFFER is the buffer to search through."
  (delete-dups (joblog--history buffer joblog--company-regexp 1)))

(defun joblog--location-list (buffer)
  "Return an list of all previously entered locations.
BUFFER is the buffer to search through."
  (delete-dups (joblog--history buffer joblog--location-regexp 1)))

(defun joblog--calendar-select (fn &rest args)
  "Copies calendar selection to joblog--calendar-selection.
This function is advised by another, it will call function FN
with arguments ARGS."
  (let* ((date (calendar-cursor-to-date))
	 (year  (nth 2 date))
	 (month (nth 0 date))
	 (day   (nth 1 date)))
    (setq joblog--calendar-selection
	  (format "%04d-%02d-%02d" year month day)))
  (funcall fn args)
  (exit-recursive-edit))

(defun joblog--read-date ()
  "Read a date from the calendar."
  ;; https://stackoverflow.com/a/27678211/9184674
  (let* (joblog--calendar-selection)
    (advice-add 'calendar-exit :around 'joblog--calendar-select)
    (calendar)
    (message "Select the desired date, then type `q' to exit.")
    (recursive-edit)
    (advice-remove 'calendar-exit 'joblog--calendar-select)
    joblog--calendar-selection))

(defun joblog--prompt-status ()
  "Prompt the user for a job status.
Return the job status formatted to match `joblog--status-regexp'."
  (format "<%s>" (completing-read "Set status: " joblog-status-list)))

(defun joblog--completion-table (completions)
  "Return a completion table for COMPLETIONS.
The purpose of this function is to preserve the natural
chronological ordering of COMPLETIONS."
  (lambda (string pred action)
    (if (eq action 'metadata)
	`(metadata (display-sort-function . ,#'reverse))
      (complete-with-action action completions string pred))))

(defun joblog--insert-entry (company title date location)
  "Insert entry with COMPANY, TITLE, DATE and LOCATION information."
  (beginning-of-line)
  (insert
   (format "%s: %s (%s)" company title date)
   (cond ((string-empty-p location) "")
	 (t (concat " -- " location)))
   "\n"))

;;;###autoload
(defun joblog ()
  "Prompt the user to log a job application.
The information provided by the user will be inserted at the very
top of `joblog-file'."
  (interactive)
  (let* ((buffer (find-file-noselect joblog-file))
	 (company-history (joblog--company-list buffer))
	 (location-history (joblog--location-list buffer))
	 (company (completing-read "Company: " company-history))
	 (title (read-string "Job title: "))
	 (location (completing-read "Location: " location-history))
	 (date (joblog--read-date))
	 (today (format-time-string "%Y-%m-%d")))
    (with-current-buffer (find-file-noselect joblog-file)
      (if (string-equal date today)
	  (progn
	    (goto-char (point-min))
	    (joblog--insert-entry company title date location))
	(with-current-buffer buffer
	  (goto-char (point-min))
	  (let ((found nil))
	    (while (and (not found)
			(re-search-forward joblog--date-regexp nil t))
	      (let ((entry-date
		     (buffer-substring-no-properties
		      (match-beginning 1)
		      (match-end 1))))
		(when (string> date entry-date)
		  (setq found t)
		  (forward-line -1)
		  (joblog--insert-entry company title date location)))))))
      (save-buffer))))

;;;###autoload
(defun joblog-visit ()
  "Prompt the user to visit an existing log entry."
  (interactive)
  (unless (and joblog-file (file-regular-p joblog-file))
    (user-error "Customize `joblog-file' first."))
  (condition-case err
      (let* ((buffer (find-file-noselect joblog-file))
	     (entries (or (joblog--entry-list buffer)
			  (signal 'joblog-empty-file nil)))
	     (choice (completing-read
		      "Entry: "
		      (joblog--completion-table entries)
		      nil t)))
	(switch-to-buffer buffer)
	(joblog-mode)
	(goto-char (point-min))
	(re-search-forward choice)
	(beginning-of-line))
    (joblog-empty-file
     (user-error
      (error-message-string err)))))

;;;###autoload
(defun joblog-change-status (&optional save)
  "Edit the status field of an existing log entry.
If SAVE is non-nil, save the buffer."
  (interactive (list t))
  (save-window-excursion
    (joblog-visit)
    (let ((status (joblog--prompt-status))
	  (boundary (line-end-position)))
      (cond
       ;; Status is already set, replace with new status
       ((number-or-marker-p
	 (re-search-forward (joblog--status-regexp) boundary t))
	(replace-match status))
       ;; No previous status, determine where to insert based on
       ;; location field
       ((number-or-marker-p
	 (re-search-forward joblog--location-regexp boundary t))
	(goto-char (match-beginning 0))
	(insert status " "))
       ;; No previous status, determine where to insert based on date
       ;; field
       ((number-or-marker-p
	 (re-search-forward joblog--date-regexp boundary t))
	(goto-char (match-end 0))
	(insert " " status))))
    (save-buffer save)))

(defun joblog--locate-old-entries (buffer)
  "Return the first and last position of old entries in BUFFER."
  (with-current-buffer buffer
    (let ((first-old-entry nil))
      (while (and (not first-old-entry)
		  (re-search-forward joblog--date-regexp nil t))
	(when (> (joblog--day-difference
		  (match-string-no-properties 1))
		 joblog-recent-entry-interval)
	  (setq first-old-entry (line-beginning-position))))
      (cons first-old-entry (point-max)))))

(defun joblog--make-entry-overlay ()
  "Creates an overlay which marks any old entries."
  (save-excursion
    (save-window-excursion
      (when-let*
	  ((joblog-mark-old-entries)
	   (buffer (find-file joblog-file))
	   (coordinates (joblog--locate-old-entries buffer))
	   (start (car coordinates))
	   (end (cdr coordinates)))
	(setq joblog--entry-overlay (make-overlay start end buffer))
	(overlay-put joblog--entry-overlay 'face 'shadow)))))

;;;###autoload
(define-derived-mode
  joblog-mode
  fundamental-mode
  "Joblog"
  (setq font-lock-defaults joblog--font-lock-defaults)
  (setq-local comment-start "#")
  (setq-local comment-start-skip
	      (rx (one-or-more "#")
		  (zero-or-more space)))
  (joblog--make-entry-overlay))

(provide 'joblog)
