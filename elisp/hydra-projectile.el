(require 'pretty-hydra)
(require 'shell-repl)
(require 'projectile)

(defun shell-repl-project-root ()
  "Run shell-repl in projectile-project-root."
  (interactive)
  (shell-repl (projectile-project-root)))

(pretty-hydra-define hydra-projectile (:color teal :quit-key "q"
                                              :title (concat
                                                             " Projectile in "
                                                             (projectile-project-root)))
  ("Find file"
   (("f"   projectile-find-file "file")
    ("s-f" projectile-find-file-dwim "file dwim")
    ("r"   projectile-recentf "recent file")
    ("d"   projectile-find-dir "dir"))

   "Search/Tags"
   (("g"   consult-ripgrep-inherit-region "grep")
    ("t"   ggtags-update-tags "update gtags")
    ("o"   projectile-multi-occur "multi-occur"))

   "Buffers"
   (("i"   projectile-ibuffer)
    ("b"   projectile-switch-to-buffer "switch to buffer")
    ("K"   projectile-kill-buffers "Kill all buffers"))

   "Cache"
   (("x"   projectile-remove-known-project "remove known project")
    ("X"   projectile-cleanup-known-projects "cleanup non-existing")
    ("z"   projectile-cache-current-file "cache current" :color red)
    ("c"   projectile-invalidate-cache "clear cache" :color red))

   "Project"
   (("p"   projectile-switch-project "switch project"))

   "Misc"
   (("M-!" shell-repl-project-root "shell")
    ("1" shell-repl-project-root "  shell"))))

(provide 'hydra-projectile)
;;; hydra-projectile.el ends here
