(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (let (value)
    (dotimes (i 10 value)
      (scroll-up-line)
      (next-line))))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (let (value)
    (dotimes (i 10 value)
      (scroll-down-line)
      (previous-line))))

(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (next-line))

;; src https://www.emacswiki.org/emacs/MarkCommands
(defun mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))


;; src: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; KEY BIND LIST
    (define-key map (kbd "M-g") 'goto-line)
    (define-key map (kbd "M-p") 'scroll-up-bind)
    (define-key map (kbd "M-n") 'scroll-down-bind)
    (define-key map (kbd "C-,") 'other-window)
    (define-key map (kbd "C-.") 'wind-bck)
    (define-key map (kbd "C-;") 'other-frame)
    (define-key map (kbd "C-/") 'advertised-undo)
    (define-key map (kbd "C-:") 'mark-current-word)
    map)
  "my-keys-minor-mode keymap.")

;; Refuses to work inside the key map
;; Doing both cause I don't know which works
(global-set-key [C-tab] 'c-tab-bind)
(global-set-key (kbd "<C-tab>") 'c-tab-bind)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(provide 'mye-keys)
