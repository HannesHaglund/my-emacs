(pretty-hydra-define hydra-navigation (:title "↝ Navigation" :quit-key "q")
  ("avy"
   (("v" avy-goto-char "avy goto char")
    ("L" avy-goto-line "avy goto line"))
   "Basic navigation"
   (("e" end-of-code-line-or-buffer "end of line")
    ("a" beginning-of-code-line-or-buffer "beginning of line")
    ("f" forward-char "forward")
    ("b" backward-char "back")
    ("n" scroll-down-bind "down")
    ("p" scroll-up-bind "up"))
   "Find file"
   (("xf" helm-find-files "find file")
    ("cpf" helm-projectile-find-file "find file in project")
    ("cpp" helm-projectile-switch-project "change project")
    ("gp" helm-projectile-grep "grep project")
    ("gd" helm-do-ag "grep directory")
    ("g," helm-ag-pop-stack "grep pop stack"))
   "Search"
   (("s" isearch-forward "isearch" :color blue)
    ("r" isearch-backward "isearch backward" :color blue)
    ("o" helm-occur "occur"))
   "Other"
   (("xb" helm-buffers-list "change buffer")
    ("," other-window "change window")
    ("l" recenter-top-bottom "recenter view")
    ("." hydra-repeat "repeat"))))

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

(provide 'hydra-navigation)
