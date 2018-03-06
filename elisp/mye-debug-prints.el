(defun mye-current-line ()
  (string-to-number (format-mode-line "%l")))

(defun mye-write-newline (str)
  (beginning-of-visual-line)
  (open-line 1)
  (push-mark)
  (insert str)
  (pop-mark)
  (indent-for-tab-command))

(defun mye-overwrite-line (str)
  (beginning-of-visual-line)
  (kill-visual-line)
  (insert str)
  (indent-for-tab-command))

(defun mye-debug-print-str (str)
  (when (equal major-mode 'emacs-lisp-mode)
    (format "(message \"DBG %s l%d: %s\") ;; DBGPRNT::: %s %s"
            (buffer-name)
            (line-number-at-pos)
            str str "REMOVE ME")))

(defun mye-line-get-dbg-msg (line)
  (substring line
             (+ (length " DBGPRNT::: ") (string-match " DBGPRNT::: " line))
             (string-match " REMOVE ME" line)))

(defun mye-line-is-dbg-line (line)
  (and
   (string-match " DBGPRNT::: " line)
   (string-match " REMOVE ME" line)))

(defun mye-debug-overwrite-line (str)
  (mye-overwrite-line (mye-debug-print-str str)))

(defun mye-update-debug-line ()
  (when (mye-line-is-dbg-line (thing-at-point 'line t))
    (mye-debug-overwrite-line (mye-line-get-dbg-msg (thing-at-point 'line t)))))

(defun mye-is-last-line ()
  (string-match "\n" (thing-at-point 'line t)))

(defun mye-update-debug-buffer-iteration ()
  (mye-update-debug-line)
  (when (>= (mye-is-last-line) 0)
    (next-line)
    (mye-update-debug-buffer-iteration)
  ))

(defun mye-update-debug-buffer (buffer)
  (with-current-buffer buffer
    (save-excursion
      (beginning-of-buffer)
      (mye-update-debug-buffer-iteration))))

(defun debug-print-insert (str)
  (interactive "sPrint: ")
  (message "s: %s" (mye-debug-print-str str))
  (if str
      (mye-write-newline (mye-debug-print-str str))
    (message "Failed to write")))

(defun debug-print-update-current-buffer ()
  (interactive)
  (mye-update-debug-buffer (buffer-name)))

(provide 'mye-debug-prints)
