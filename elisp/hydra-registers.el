(require 'pretty-hydra)
(require 'all-the-icons)

(pretty-hydra-define hydra-registers (:color blue
                                             :title (concat (all-the-icons-faicon "list-ol") " Registers")
                                             :quit-key "q")
  ("Point"
   (("r" point-to-register "point to register")
    ("j" jump-to-register "jump to register"))
   "Text"
   (("c" copy-to-register "copy region")
    ("C" copy-rectangle-to-register "copy rect")
    ("i" insert-register "insert")
    ("p" prepend-to-register "prepend")
    ("a" append-to-register "append"))
   "Macros"
   (("m" kmacro-to-register "store macro")
    ("e" jump-to-register "execute"))
   "Miscellaneous"
   (("v" consult-register "view registers"))))

(provide 'hydra-registers)
;;; hydra-registers.el ends here
