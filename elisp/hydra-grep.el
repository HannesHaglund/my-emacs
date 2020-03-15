(pretty-hydra-define hydra-grep (:color teal :title "⛳ Grep" :quit-key "q")
  ("In buffers"
   (("t" helm-do-ag-this-saved-file "this buffer")
    ("b" helm-do-ag-buffers "all buffers"))
   "In project"
   (("a" helm-do-ag-project-root "ag")
    ("g" helm-projectile-grep "git grep"))
   "Navigation"
   (("s" helm-ag-pop-stack "stack pop"))))

(defun helm-do-ag-this-saved-file ()
  (interactive)
  (save-buffer)
  (helm-do-ag-this-file))

(provide 'hydra-grep)
