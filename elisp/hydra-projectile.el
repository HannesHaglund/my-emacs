(defun helm-projectile-grep-project-root ()
  "Run helm-projectile-grep on (projectile-project-root)"
  (interactive)
  (helm-projectile-grep (projectile-project-root)))

(pretty-hydra-define hydra-projectile (:color teal :quit-key "q"
                                              :title "📁 Projectile in %(projectile-project-root)")
  ("Find file"
   (("f"   helm-projectile-find-file "file")
    ("s-f" helm-projectile-find-file-dwim "file dwim")
    ("r"   helm-projectile-recentf "recent file")
    ("d"   helm-projectile-find-dir "dir"))

   "Search/Tags"
   (("a"   helm-do-ag-project-root "ag")
    ("g"   helm-projectile-grep-project-root "git grep")
    ("t"   ggtags-update-tags "update gtags")
    ("o"   projectile-multi-occur "multi-occur"))

   "Buffers"
   (("i"   projectile-ibuffer)
    ("b"   helm-projectile-switch-to-buffer "switch to buffer")
    ("K"   projectile-kill-buffers "Kill all buffers"))

   "Cache"
   (("x"   projectile-remove-known-project "remove known project")
    ("X"   projectile-cleanup-known-projects "cleanup non-existing")
    ("z"   projectile-cache-current-file "cache current" :color red)
    ("c"   projectile-invalidate-cache "clear cache" :color red))

   "Project"
   (("p"   helm-projectile-switch-project "switch project"))))

(provide 'hydra-projectile)
