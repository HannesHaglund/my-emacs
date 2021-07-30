(require 'pretty-hydra)

(pretty-hydra-define hydra-swedish (:color pink
                                           :title "⚑ Swedish"
                                           :quit-key "q")
  ("Character map"
   (("["  (insert "å") "å")
    ("{"  (insert "Å"))
    (";"  (insert "ö") "ö")
    (":"  (insert "Ö"))
    ("'"  (insert "ä") "ä")
    ("\"" (insert "Ä")))))

(provide 'hydra-swedish)
;;; hydra-swedish.el ends here
