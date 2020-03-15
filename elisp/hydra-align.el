(pretty-hydra-define hydra-align (:color teal :title "☰ Align" :quit-key "q")
  ("Actions"
   (("a" align        "align")
    ("r" align-regexp "align-regexp")
    ("e" align-each   "align-each"))))

(defun align-each (regexp)
  (interactive "sRegexp: ")
  (align-regexp
   (if (use-region-p) (region-beginning) (point-min))
   (if (use-region-p) (region-end)       (point-max))
   (concat "\\(\\s-*\\)" regexp)
   1
   align-default-spacing
   1))

(provide 'hydra-align)
