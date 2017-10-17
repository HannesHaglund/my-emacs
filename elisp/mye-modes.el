(provide 'mye-modes)

;; src: https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun apply-tab-settings ()
  (setq-default indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq default-tab-width 4))

;; All modes
(apply-tab-settings)
(infer-indentation-style)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; prog-mode
(add-hook 'prog-mode-hook 'linum-mode)
