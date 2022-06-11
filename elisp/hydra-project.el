(require 'pretty-hydra)

(defun project-find-project-then-find (dir)
  "\"Switch\" to another project by running an Emacs command.
When called in a program, it will use the project corresponding to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((default-directory dir)
        (project-current-inhibit-prompt t))
    (call-interactively 'project-find-file)))

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
   (("p"   project-find-project-then-find "switch project"))))

(provide 'hydra-project)
;;; hydra-project.el ends here
