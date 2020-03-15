(pretty-hydra-define hydra-zoom (:color red :title "🔍 Zoom" :quit-key "q")
  ("All buffers"
   (("r" (do-in-each-buffer 'text-scale-set 0)      "reset")
    ("i" (do-in-each-buffer 'text-scale-increase 1) "zoom in")
    ("o" (do-in-each-buffer 'text-scale-decrease 1) "zoom out"))
   "This buffer"
   (("R" (text-scale-set 0)      "reset")
    ("I" (text-scale-increase 1) "zoom in")
    ("O" (text-scale-decrease 1) "zoom out"))))

(defun do-in-each-buffer (what &rest args)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (apply 'funcall what args))))

(provide 'hydra-zoom)
