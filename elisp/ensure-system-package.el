(require 'system-packages)

(defun windows-find-program-file-dir-recurse (file-name)
  "Return windows-style path to first occurence of folder under Program Files containing FILE-NAME."
  (replace-in-string
   "/"
   "\\\\"
   (file-name-directory
    (car
     (split-string
      (shell-command-to-string (concat "dir \"c:\\Program Files\" /s /b | findstr /i \\\\" file-name))
      "\n")))))

(defun ensure-system-binary-windows (binary-name install-instructions)
  "Given that hosy OS is Windows, ensure that BINARY-NAME exists on 'exec-path', prompting the user with INSTALL-INSTRUCTIONS if it can't be found automatically."
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
  "Given that host OS is gnu/linux, ensure that PACKAGE-NAME is installed."
  (when (eq system-type 'gnu/linux)
    (system-packages-ensure linux-src-package)))

(defun ensure-system-package (linux-package-name windows-binary-name windows-install-instructions)
  "Ensure LINUX-PACKAGE-NAME or WINDOWS-BINARY-NAME is available, installing it or providing WINDOWS-INSTALL-INSTRUCTIONS if not."
  (ensure-system-package-linux linux-package-name)
  (ensure-system-binary-windows windows-binary-name windows-install-instructions))

(defun pip-list ()
  "Return string containing list of installed pip modules."
  (if (eq system-type 'windows-nt)
      ;; python3 is called python on Windows
      (shell-command-to-string "python -m pip list")
    (shell-command-to-string "python3 -m pip list")))

(defun ensure-pip-module (module-name)
  "Ensure that MODULE-NAME is installed in pip, providing instructions for how to do so if not."
  (when (not (string-match-p (regexp-quote module-name) (pip-list)))
    (display-warning :warning
                     (concat "The pip module " module-name " was not found among installed pip modules. "
                             "It not being available may cause some packages to not work correctly. "
                             "Please install it via \"pip install " module-name "\", and then restart emacs."))))

(defun ensure-font-installed (font-name install-instructions)
  "Ensure font FONT-NAME is installed, providing INSTALL-INSTRUCTIONS for how to install it if it isn't"
  (unless (font-available-p font-name)
    (display-warning :warning
                     (concat "The font " font-name " could not be found on your system. " install-instructions))))

(provide 'ensure-system-package)
;;; ensure-system-package.el ends here
