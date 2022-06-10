(require 'pretty-hydra)

(pretty-hydra-define hydra-project (:color teal :quit-key "q"
                                           :title "📁 Project %(project-current)")
  ("Find file"
   (("f"   project-find-file "file")
    ("d"   project-find-dir "dir"))

   "Search/Tags"
   (("g"   consult-ripgrep "grep"))

   "Buffers"
   (("b"   project-switch-to-buffer "switch to buffer")
    ("K"   project-kill-buffers "Kill all buffers"))

   "Cache"
   (("x"   project-remove-known-project "remove known project"))

   "Project"
   (("p"   project-switch-project "switch project"))))

(provide 'hydra-project)
;;; hydra-project.el ends here
