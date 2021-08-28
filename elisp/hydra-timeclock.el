(require 'timeclock)

(defun timeclock-project-summary-format-time-date (time)
  """Format TIME seconds since 1970 as a date string."""
  (format-time-string "%Y/%m/%d %a" time))

(defun timeclock-project-summary-format-time (time)
  """Format TIME seconds as hours and minutes."""
  ;; For some reason it's off by 1 hour unless I add 23 hours worth of seconds
  ;; Using format-time-string in this way is probably a hack... :(
  (format-time-string "%H:%M" (time-convert (+ (* 23 60 60) time) 'list)))

(defun timeclock-proj-accum-info-element-to-string (proj-accum-element)
  """Convert alist element PROJ-ACCUM-ELEMENT to a human-readable line."""
  (let* ((key      (car proj-accum-element))
         (time-sum (cdr proj-accum-element))
         (project  (nth 0 key))
         (date     (nth 1 key))
         (date-split (split-string date "/"))
         (year  (string-to-number (nth 0 date-split)))
         (month (string-to-number (nth 1 date-split)))
         (day   (string-to-number (nth 2 date-split)))
         (encoded-date     (encode-time (list 0 0 12 day month year nil nil nil))))
    (format "%s - Spent %s on %s"
            (timeclock-project-summary-format-time-date encoded-date)
            (timeclock-project-summary-format-time time-sum)
            project)))

(defun timeclock-project-summary-string ()
  """Generate lines containing project hour sums based on timelog."""
  (string-join (mapcar 'timeclock-proj-accum-info-element-to-string (timeclock-time-per-project-per-day)) "\n"))

(defun timeclock-project-summary ()
  """Open and refresh timeclock project summary buffer."""
  (interactive)
  (with-help-window "*timeclock-project-summary*"
    (princ "Timeclock Project Summary\n")
    (princ (format "Todays date is %s\n" (timeclock-project-summary-format-time-date nil)))
    (princ "\n")
    (princ (timeclock-project-summary-string))))

(defun timeclock-time-per-project-per-day ()
  """Generate alist based on timelog mapping (PROJECT DATE) to hours worked in the project at that day."""
  (when (file-readable-p timeclock-file)
    (let* ((event              nil)
           (event-time         nil)
           (event-time-decoded nil)
           (event-date         nil)
           (event-direction    nil)
           (event-reason       nil)
           (prev-date          nil)
           (prev-in-time       nil)
           (prev-in-reason     nil)
           (project-accum      '()))

      (with-temp-buffer
        (insert-file-contents timeclock-file)
        (goto-char 0)

        (while (setq event (timeclock-read-moment))
          (forward-line 1)

          (setq event-direction (nth 0 event))
          (setq event-time      (nth 1 event))
          (setq event-reason    (nth 2 event))
          (setq event-time-decoded (decode-time event-time))
          (setq event-date (format "%d/%d/%d"
                                   (nth 5 event-time-decoded) ; year
                                   (nth 4 event-time-decoded) ; month
                                   (nth 3 event-time-decoded))) ; day

          ;; (message (concat "Processing "
          ;;                  event-direction " "
          ;;                  (prin1-to-string event-time-decoded) " "
          ;;                  event-reason))

          ;; Handle missing out on previous date. Reset prev-in-time when changing date.
          (when (not (string= event-date prev-date))
            (setq prev-in-time event-time)
            (setq prev-in-reason "ERROR"))
          (setq prev-date event-date)

          ;; Process in
          (when (string= "i" event-direction)
            (setq prev-in-time event-time)
            (setq prev-in-reason event-reason))

          ;; Process out
          (when (or (string= "o" event-direction) (string= "O" event-direction))
            ;; Ignore entry if it's the first one of the day. First should be an "i".
            (when (and prev-in-time prev-in-reason)
              (let* ((time-diff (time-subtract event-time prev-in-time))
                     (cur-val   (alist-get (list prev-in-reason event-date) project-accum nil nil #'equal)))
                ;; Update total for that project
                (setf (alist-get (list prev-in-reason event-date) project-accum nil nil #'equal)
                      (+ time-diff (if cur-val cur-val 0))))))))
      ;; Return value
      project-accum)))

(defun timeclock-out-no-reason ()
  """Run timeclock-out, but without any specified REASON."""
  (interactive)
  (timeclock-out ""))

(pretty-hydra-define hydra-timeclock (:color teal :quit-key "q"
                                             :pre (timeclock-reread-log)
                                             :title "🕑 Timeclock: %(timeclock-status-string)")
  ("Report"
   (("i" timeclock-in "In")
    ("c" timeclock-change "Change project")
    ("o" timeclock-out-no-reason "Out")
    ("O" timeclock-out "Out with reason"))
   "Manual edit"
   (("f" timeclock-visit-timelog "Find timelog"))
   "Summarize"
   (("s" timeclock-project-summary "Project summary"))))

(provide 'hydra-timeclock)
;;; hydra-timeclock.el ends here
