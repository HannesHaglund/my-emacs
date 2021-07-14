(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (next-line 20)
  (recenter nil t))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (previous-line 20)
  (recenter nil t))

;; The hydra-repeat supplied by the hydra package is not using universal argument correctly
;; This is a fixed version
(defun hydra-repeat (&optional arg)
  "Repeat last command with last prefix arg.
When ARG is non-nil, use that instead."
  (interactive "p")
  (if (eq arg 1)
      (unless (string-match "hydra-repeat$" (symbol-name last-command))
        (setq hydra-repeat--command last-command)
        (setq hydra-repeat--prefix-arg last-prefix-arg))
    (setq hydra-repeat--prefix-arg arg))
  (setq current-prefix-arg hydra-repeat--prefix-arg)
  (call-interactively hydra-repeat--command)) ; call-interactively instead of funcall

(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (next-line))

(setq beginning-of-code-line-or-bugger-original-point nil)
(setq beginning-of-code-line-or-buffer-times-pressed 0)
(defun beginning-of-code-line-or-buffer ()
  "Move point to beginning of code, line or buffer depending on times pressed."
  (interactive)
  (when (not (eq last-command this-command))
    (setq beginning-of-code-line-or-bugger-original-point nil)
    (setq beginning-of-code-line-or-buffer-times-pressed 0))
  (when beginning-of-code-line-or-bugger-original-point
    (goto-char beginning-of-code-line-or-bugger-original-point)
    (setq beginning-of-code-line-or-bugger-original-point nil))
  (setq beginning-of-code-line-or-buffer-times-pressed
        (+ beginning-of-code-line-or-buffer-times-pressed 1))
  (if (eq beginning-of-code-line-or-buffer-times-pressed 1)
      (beginning-of-line-text)
    (if (eq beginning-of-code-line-or-buffer-times-pressed 2)
        (beginning-of-line)
      (when (eq beginning-of-code-line-or-buffer-times-pressed 3)
        (setq beginning-of-code-line-or-bugger-original-point (point))
        (goto-char (point-min))
        (setq beginning-of-code-line-or-buffer-times-pressed 0)))))

(setq end-of-code-line-or-bugger-original-point nil)
(setq end-of-code-line-or-buffer-times-pressed 0)
(defun end-of-code-line-or-buffer ()
  "Move point to end of code, line or buffer depending on times pressed."
  (interactive)
  (when (not (eq last-command this-command))
    (setq end-of-code-line-or-bugger-original-point nil)
    (setq end-of-code-line-or-buffer-times-pressed 0))
  (when end-of-code-line-or-bugger-original-point
    (goto-char end-of-code-line-or-bugger-original-point)
    (setq end-of-code-line-or-bugger-original-point nil))
  (setq end-of-code-line-or-buffer-times-pressed
        (+ end-of-code-line-or-buffer-times-pressed 1))
  (if (eq end-of-code-line-or-buffer-times-pressed 1)
      (mwim-end)
    (if (eq end-of-code-line-or-buffer-times-pressed 2)
        (end-of-line)
      (when (eq end-of-code-line-or-buffer-times-pressed 3)
        (setq end-of-code-line-or-bugger-original-point (point))
        (goto-char (point-max))
        (setq end-of-code-line-or-buffer-times-pressed 0)))))

(provide 'basic-keybinds)
