(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (scroll-up-line 10))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (scroll-down-line 10))

(defun scroll-down-bind-enter-hydra-nav ()
  (interactive)
  (scroll-down-bind)
  (hydra-navigation/body))

(defun scroll-up-bind-enter-hydra-nav ()
  (interactive)
  (scroll-up-bind)
  (hydra-navigation/body))

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
