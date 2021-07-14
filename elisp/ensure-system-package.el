(require 'system-packages)

(defun windows-find-program-file-dir-recurse (file-name)
  "Return windows-style path to first occurence of folder under Program Files containing file-name"
  (replace-in-string
   "/"
   "\\\\"
   (file-name-directory
    (car
     (split-string
      (shell-command-to-string (concat "dir \"c:\\Program Files\" /s /b | findstr /i " file-name))
      "\n")))))

(defun ensure-system-binary-windows (binary-name install-instructions)
  (when (eq system-type 'windows-nt)
    (when (not (executable-find binary-name))
      (message (concat "Searching Program Files for " binary-name " ..."))
      (let ((exe-win-path (windows-find-program-file-dir-recurse binary-name)))
        (if exe-win-path
            ;; If it can be found in program files, add it to the path
            (progn
              (display-warning :debug
                               (concat "The system executable " binary-name " was not found on exec-path, "
                                       "but was found at " exe-win-path " through a (slow) recursive search. "
                                       "Emacs will work properly, but you may want to add it to PATH or exec-path to speed up startup time."))
              (add-to-list 'exec-path exe-win-path)
              (setenv "PATH"  (concat (getenv "PATH") ";" exe-win-path)))
          ;; It's not even in program files, ask the user to install it
          (display-warning :warning
                           (concat "The system executable " binary-name " was not found on the system. "
                                   "It not being available may cause some packages to not work correctly. "
                                   "You must install it manually, and then restart emacs. "
                                   install-instructions)))))))

(defun ensure-system-package-linux (package-name)
  (when (eq system-type 'gnu/linux)
    (system-packages-ensure linux-src-package)))

(defun ensure-system-package (linux-package-name windows-binary-name windows-install-instructions)
  (ensure-system-package-linux linux-package-name)
  (ensure-system-binary-windows windows-binary-name windows-install-instructions))

(provide 'ensure-system-package)
