(require 'projectile)

(setq gp-grep-program "ag")
(setq gp--grep-query "--nocolor --nogroup --only-matching \".*\"")
(setq gp--grep-symbols nil)
(setq gp--grep-proc-id 0)

(defun gp--project-roots ()
  (delq nil (delete-dups
             (mapcar 'projectile-project-root
                     (mapcar 'buffer-name (buffer-list))))))

(defun gp--grep-symbols-filter (proc string)
  (setq  gp--grep-symbols
         (delete-dups (delq nil (append string gp--grep-symbols)))))

(defun gp--background-grep-symbols (root)
  (let ((proc
         (start-process (concat "grep-symbols-" (number-to-string gp--grep-proc-id))
                        "<Test output>"
                        gp-grep-program
                        gp--grep-query)))
    (set-process-filter proc 'gp--grep-symbols-filter)
    (setq gp--grep-proc-id (+ gp--grep-proc-id 1))))

(defun gp--refresh ()
  (let (root)
    (dolist (root (gp--project-roots))
      (gp--background-grep-symbols root))))

(gp--refresh)
(sit-for 3)
(message "Symbols:")
(setq elt nil)
(dolist (elt gp--grep-symbols)
  (message elt))
