(pretty-hydra-define hydra-magit (:color teal :title "⎆ Magit" :quit-key "q")
  ("Dispatch"
   (("r" magit-dispatch "repo")
    ("f" magit-file-dispatch "file"))
   "Status"
   (("s" magit-status "status"))))

(provide 'hydra-magit)
