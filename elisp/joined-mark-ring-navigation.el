(require 'subr-x)

(defvar joined-mark-ring '())
(defvar joined-mark-ring-source-commands '())
(defvar joined-mark-ring-index 0)
(defvar joined-mark-ring-max-size 12)

(defun marker-to-string (marker)
  (format "%s" marker))

(defun add-to-joined-mark-ring (element)
  ;; Pop elements we've diverged from
  (while (> joined-mark-ring-index 0)
    (setq joined-mark-ring-index (- joined-mark-ring-index 1))
    (setq joined-mark-ring (cdr joined-mark-ring))
    (setq joined-mark-ring-source-commands (cdr joined-mark-ring-source-commands)))
  ;; Actually add... if it is not a duplicate
  (unless (and (car joined-mark-ring) (string= (marker-to-string element)
                                               (marker-to-string (car joined-mark-ring))))
    (setq joined-mark-ring (cons element joined-mark-ring))
    (setq joined-mark-ring-source-commands (cons this-command joined-mark-ring-source-commands)))
  ;; Pop elements if we have too many
  (while (> (length joined-mark-ring) joined-mark-ring-max-size)
    (setq joined-mark-ring (butlast joined-mark-ring))
    (setq joined-mark-ring-source-commands (butlast joined-mark-ring-source-commands))))

(defun joined-mark-ring-push-mark (&optional a b c)
  (add-to-joined-mark-ring (copy-marker (mark-marker))))

(defun joined-mark-ring-push-point (&optional a b c)
  (add-to-joined-mark-ring (copy-marker (point-marker))))

(defun marker-line (marker)
  (save-excursion
    (switch-to-buffer (marker-buffer marker))
    (save-excursion
      (goto-char marker)
      (line-number-at-pos))))

(defun joined-mark-ring-string-format-mark (mark command)
  (format " %s:%d -- %s" (marker-buffer mark) (marker-line mark) command))

(defun beginning-of-line-point ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun end-of-line-point ()
  (save-excursion
    (end-of-line)
    (point)))

;; TODO: Display what command was ran when populating X marker
(defun joined-mark-ring-string ()
  (with-temp-buffer
    (dotimes (i (length joined-mark-ring))
      (insert (joined-mark-ring-string-format-mark (nth i joined-mark-ring)
                                                   (nth i joined-mark-ring-source-commands)))
      (insert "\n"))
    (goto-char (point-min))
    (forward-line joined-mark-ring-index)
    (add-text-properties (point-min) (point) '(comment t face font-lock-comment-face))
    (insert ">")
    (add-text-properties (beginning-of-line-point) (end-of-line-point) '(comment t face font-lock-keyword-face))
    (goto-char (point-min))
    (insert "joined-mark-ring:\n")
    (goto-char (point-min))
    (add-text-properties (beginning-of-line-point) (end-of-line-point) '(face link-visited))
    (buffer-substring (point-min) (point-max))))

(defun goto-marker (mark)
  (switch-to-buffer (get-buffer (marker-buffer mark)))
  (goto-char mark))

(defun move-joined-mark-ring-index (diff)
  (if (= (length joined-mark-ring) 0) (message "joined-mark-ring is empty.")
    (progn
      ;; On first non-repeated command...
      (when (and (eq this-command 'pop-joined-mark-ring)
                 (not (or (eq last-command 'pop-joined-mark-ring)
                          (eq last-command 'unpop-joined-mark-ring))))
        (joined-mark-ring-push-point))
      ;; Add diff
      (setq joined-mark-ring-index (+ joined-mark-ring-index diff))
      ;; Constrain to joined-mark-ring
      (when (< joined-mark-ring-index 0) (setq joined-mark-ring-index 0))
      (when (>= joined-mark-ring-index (length joined-mark-ring)) (setq joined-mark-ring-index (- (length joined-mark-ring) 1)))
      ;; Update point and display results
      (message (joined-mark-ring-string))
      (goto-marker (nth joined-mark-ring-index joined-mark-ring))
      (nav-flash-show))))

(defun pop-joined-mark-ring ()
  (interactive)
  (move-joined-mark-ring-index 1))

(defun unpop-joined-mark-ring ()
  (interactive)
  (move-joined-mark-ring-index -1))

(provide 'joined-mark-ring-navigation)
;;; joined-mark-ring-navigation.el ends here
