(pretty-hydra-define hydra-eval (:color blue :title "𝄠 Elisp eval" :quit-key "q")
  ("Actions"
   (("e" helm-eval-expression "expression")
    ("b" eval-buffer "buffer")
    ("r" eval-region "region")
    ("d" eval-defun  "defun"))))

(provide 'hydra-eval)
;;; hydra-eval.el ends here
