(require 'pretty-hydra)
(require 'grep-toolbox)

(pretty-hydra-define hydra-projectile (:color teal :quit-key "q"
                                              :title "📁 Projectile in %(projectile-project-root)")
  ("Find file"
   (("f"   projectile-find-file "file")
    ("s-f" projectile-find-file-dwim "file dwim")
    ("r"   projectile-recentf "recent file")
    ("d"   projectile-find-dir "dir"))

   "Search/Tags"
   (("g"   grep-toolbox-consult-ripgrep "grep")
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
   (("p"   projectile-switch-project "switch project"))))

(provide 'hydra-projectile)
;;; hydra-projectile.el ends here
