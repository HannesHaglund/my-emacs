(require 'pretty-hydra)

(pretty-hydra-define hydra-p4 (:color teal :title "P4" :quit-key "q")
  ("Files"
   (("a"         p4-add              "open file for add")
    ("e"         p4-edit             "open file for edit")
    ("m"         p4-move             "open file for move")
    ("x"         p4-delete           "open file for delete")
    ("r"         p4-revert           "revert file")
    ("o"         p4-opened           "list opened files"))
   "Changes"
   (("s"         p4-status           "status")
    ("v"         p4-annotate         "annotate (=blame)")
    ("f"         p4-filelog          "filelog")
    ("="         p4-diff             "diff")
    ("S"         p4-submit           "submit")
    ("g"         p4-update           "update   (=pull)")
    ("z"         p4-reconcile        "reconcile"))
   "Config"
   (("c"         p4-client           "client (=edit workspace)"))
   "Unusual"
   (("b"         p4-branch           "branch")
    ("B"         p4-branches         "branches")
    ("C"         p4-changes          "changes")
    ("d"         p4-diff2            "diff2")
    ("D"         p4-describe         "describe")
    ("E"         p4-reopen           "reopen")
    ("C-f"       p4-depot-find-file  "depot-find-file")
    ("F"         p4-files            "files")
    ("G"         p4-get-client-name  "get-client-name")
    ("h"         p4-help             "help"))
   ""
   (("I"         p4-integ            "integ")
    ("H"         p4-have             "have")
    ("i"         p4-info             "info")
    ("j"         p4-job              "job")
    ("J"         p4-jobs             "jobs")
    ("l"         p4-label            "label")
    ("L"         p4-labels           "labels")
    ("C-l"       p4-labelsync        "labelsync")
    ("p"         p4-print            "print")
    ("P"         p4-set-p4-port      "set-p4-port"))
   ""
   (("R"         p4-refresh          "refresh")
    ("C-r"       p4-resolve          "resolve")
    ("t"         p4-toggle-vc-mode   "toggle-vc-mode")
    ("u"         p4-user             "user")
    ("U"         p4-users            "users")
    ("V"         p4-version          "version")
    ("w"         p4-where            "where")
    ("X"         p4-fix              "fix")
    ("C-="       p4-diff-all-opened  "diff all opened")
    ("-"         p4-ediff            "ediff"))))

(provide 'hydra-p4)
;;; hydra-p4.el ends here
