(setq local-init-file-name "~/.emacs.d/local-init.el")

(defun overwrite-local-init ()
  (append-to-file
   ";; ================================================================\n\
;; local-init: Machine-local configuration\n\
;; ================================================================\n\
\n\
;; This file is intended for machine-local configurations\n\
;; It will be automatically loaded after init.el\n\
\n\
;; YOUR CODE GOES HERE" nil local-init-file-name))

(defun initialize-local-init()
  (interactive)
  (when (not (file-exists-p local-init-file-name))
    (overwrite-local-init)))

(defun find-local-init ()
  (interactive)
  (initialize-local-init)
  (find-file local-init-file-name))

(provide 'local-init-lib)
