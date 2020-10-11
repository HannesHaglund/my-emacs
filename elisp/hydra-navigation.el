(pretty-hydra-define hydra-navigation (:title "↝ Navigation" :quit-key "q")
  ("Avy char"
   (("cc" avy-goto-char "char")
    ("cl" avy-goto-char-in-line "line")
    ("ct" avy-goto-char-timer "timer")
    ("c2c" avy-goto-char "char-2")
    ("c2b" avy-goto-char-below "char-2 below")
    ("c2a" avy-goto-char-above "char-2 above"))
   "Avy word"
   (("ww" avy-goto-word-1 "word")
    ("wb" avy-goto-word-1-below "below")
    ("wa" avy-goto-word-1-above "above")
    ("w0w" avy-goto-word-0 "word-0")
    ("w0b" avy-goto-word-0-below "below")
    ("w0a" avy-goto-word-0-above "above"))
   "Avy line"
    (("ll" avy-goto-line "line")
     ("lb" avy-goto-line-below "below")
     ("la" avy-goto-line-above "above")
     ("ln" goto-line "numerical"))
   "Find file"
   (("xff" helm-find-files "find file")
    ("xfp" helm-projectile-find-file "find file in project")
    ("xfd" helm-find "find file in directory")
    ("gp" helm-projectile-grep "grep project")
    ("gd" helm-do-ag "grep directory")
    ("g," helm-ag-pop-stack "grep pop stack"))
   "Other"
   (("xb" helm-buffers-list "change buffer")
    ("xp" helm-projectile-switch-project "change project")
    ("x," other-window "change window")
    ("." xref-find-definitions "find definitions")
    ("," xref-pop-marker-stack "xref pop")
    ("lc" recenter-top-bottom "recenter view")
    ("s" isearch-forward "isearch" :color blue)
    ("r" isearch-backward "isearch backward" :color blue)
    ("z" hydra-repeat "repeat")
    ("u" universal-argument "universal argument"))
   "Basic navigation"
   (("e" end-of-code-line-or-buffer "end of line")
    ("a" beginning-of-code-line-or-buffer "beginning of line")
    ("f" forward-char)
    ("b" backward-char)
    ("n" next-line)
    ("p" previous-line))))

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
