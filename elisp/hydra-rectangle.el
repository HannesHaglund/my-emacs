(require 'pretty-hydra)

(pretty-hydra-define hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                                :color pink
                                                :hint nil
                                                :post (deactivate-mark)
                                                :title "⬕ Rectangle"
                                                :quit-key "q")
  ("Movement"
   (("p" rectangle-previous-line "↑")
    ("n" rectangle-next-line "↓")
    ("b" rectangle-backward-char "←")
    ("f" rectangle-forward-char "→"))

   "Actions"
   (("w" copy-rectangle-as-kill "copy")
    ("y" yank-rectangle "yank")
    ("k" kill-rectangle "kill")
    ("u" undo nil "undo"))

   ""
   (("o" open-rectangle "open")
    ("t" string-rectangle "type")
    ("c" clear-rectangle "clear"))

   ""
   (("N" rectangle-number-lines "Number-lines")
    ("e" rectangle-exchange-point-and-mark "exchange-point")
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) "reset-region-mark"))))

(provide 'hydra-rectangle)
;;; hydra-rectangle.el ends here
