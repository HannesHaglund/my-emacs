

(pretty-hydra-define hydra-dired (:color pink :title "📁 Dired" :quit-key "q")
  ("Marking"
    (("m" dired-mark "mark")
    ("u" dired-unmark "unmark")
    ("U" dired-unmark-all-marks "unmark all")
    ("t" dired-toggle-marks "toggle marks")
    ("E" dired-mark-extension "mark extension"))
   "Marked file manipulation"
    (("C" dired-do-copy "copy marked files")
    ("D" dired-do-delete "delete marked files")
    ("R" dired-do-rename "rename marked files")
    ("Y" dired-do-relsymlink "relative symlink marked files")
    ("S" dired-do-relsymlink "absolute symlink marked files")
    ("z" diredp-compress-this-file "(un)compress marked files")
    ("F" dired-do-find-marked-files "find marked files"))
   "Single file manipulation"
    (("o" dired-find-file "Find single file")
    ("M" dired-do-chmod "chmod")
    ("G" dired-do-chgrp "chgrp"))
   "Other"
    (("s" dired-sort-toggle-or-edit "sort")
    ("=" diredp-ediff "ediff")
    ("e" dired-ediff-files "pdiff")
    ("g" revert-buffer "refresh buffer")
    ("i" dired-maybe-insert-subdir "dired-insert-subdir")
    ("?" dired-summary "dired-summary"))))

(provide 'hydra-dired)
