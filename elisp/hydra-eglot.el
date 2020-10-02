(defun hydra-eglot-title ()
  (concat "☰ Eglot - "
          (if (eglot-current-server)
              (eglot--project-nickname (eglot-current-server))
            "No server for current project")))

(pretty-hydra-define hydra-eglot (:color amaranth :title (hydra-eglot-title) :quit-key "q" :pre (eglot-ensure))

  ("Errors"
   (("p" flymake-goto-next-error "next")
    ("n" flymake-goto-next-error "previous"))

   "Find"
    (("fd" eglot-find-declaration "declaration")
     ("ft" eglot-find-type-definition "type definition")
     ("fi" eglot-find-implementation "implementation"))

   "Actions"
    (("r" eglot-rename "rename")
     ("c" eglot-code-actions "code actions")
     ("f" eglot-format "format"))

   "Server"
    (("R" eglot-reconnect "reconnect"))))

(provide 'hydra-eglot)
