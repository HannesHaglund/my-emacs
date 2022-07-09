(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (forward-line 20)
  (recenter nil t))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (forward-line -20)
  (recenter nil t))

(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (forward-line 1))

(defun revert-buffer-no-confirm ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer nil t))

(provide 'basic-keybinds)
;;; basic-keybinds.el ends here
