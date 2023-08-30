(require 'pretty-hydra)

(pretty-hydra-define hydra-macro (:color pink
                                         :title (concat " Macro")
                                         :quit-key "q"
                                         :pre
                                         (when defining-kbd-macro
                                           (kmacro-end-macro 1)))
  ("Create"
   (("j" kmacro-start-macro "start macro" :color blue)
    ("l" kmacro-end-or-call-macro-repeat "end macro")
    ("i" kmacro-cycle-ring-previous "previous macro")
    ("k" kmacro-cycle-ring-next "next macro"))

   "Basic"
   (("e" kmacro-end-or-call-macro-repeat "execute")
    ("d" kmacro-delete-ring-head "delete")
    ("o" kmacro-edit-macro-repeat "edit")
    ("r" apply-macro-to-region-lines "region")
    ("m" kmacro-step-edit-macro "step")
    ("s" kmacro-swap-ring "swap"))

   "Insert"
   (("n" kmacro-insert-counter "insert")
    ("t" kmacro-set-counter "set")
    ("a" kmacro-add-counter "add")
    ("f" kmacro-set-format "format"))

   "Save"
   (("b" kmacro-name-last-macro "name")
    ("K" kmacro-bind-to-key "Key")
    ("x" kmacro-to-register "register")
    ("B" insert-kbd-macro "defun"))

   "Edit"
   (("," kmacro-edit-macro "previous")
    ("." edit-kbd-macro "oldest"))))

(provide 'hydra-macro)
;;; hydra-macro.el ends here
