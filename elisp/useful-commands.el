(setq run-shell-background-output-buffer-name "*run-shell-background output*")

(defun cd-command (to-what-dir)
  (concat
   ;; On windows cmd, we first need to change the drive we are operating on
   (when (eq system-type 'windows-nt) (concat (substring to-what-dir 0 2) " && "))
   ;; The actual cd
   "cd " to-what-dir))

(defun run-shell-background (command cwd)
  (interactive "sShell command: \nDCWD: ")
  (let ((full-command (concat (cd-command cwd) " && " command)))
    (write-to-run-shell-background-buffer full-command)
    (let ((proc (start-process-shell-command (replace-in-string "%" "%%" command)
                                             run-shell-background-output-buffer-name
                                             full-command)))
      (set-process-sentinel proc #'run-shell-background-sentinel))))

(defun run-shell-background-sentinel (process msg)
  (when (memq (process-status process) '(exit signal))
    (message (concat (process-name process) " - " msg))
    (write-to-run-shell-background-buffer (concat "Sentinel: " (process-name process) " - " msg))
    (when (not (string= msg "finished\n"))
      (display-buffer run-shell-background-output-buffer-name)
      (with-current-buffer run-shell-background-output-buffer-name (goto-char (point-max))))))

(defun write-to-run-shell-background-buffer (msg)
  (with-current-buffer (get-buffer-create run-shell-background-output-buffer-name)
    (comint-mode)
    ;; Write text
    (save-excursion
      (goto-char (point-max))
      (insert (propertize (concat ">>> " msg "\n")
                          'font-lock-face font-lock-function-name-face)))
    ;; Scroll window to end
    (when (get-buffer-window) (with-selected-window (get-buffer-window)
                                (goto-char (point-max))
                                (recenter -1 t)))))

(defun replace-in-string (what with in)
  (when in (replace-regexp-in-string (regexp-quote what) with in nil 'literal)))

(defun shell-command-to-kill-ring (cmd)
  (interactive "sShell command: ")
  (let ((output (shell-command-to-string cmd)))
    (message (concat "Shell output:\n" output))
    (kill-new output)))

(defun overwrite-emacs-d (repo-dir)
  "Overwrite contents in ~/.emacs.d/ with the contents of the my-emacs repository."
  (interactive (list (read-directory-name "my-emacs directory: "
                                          ;; folder of init.el buffer if it exists, or nil
                                          (let ((init-el-buffer (get-buffer "init.el")))
                                            (if init-el-buffer
                                                (file-name-directory (buffer-file-name init-el-buffer))
                                              nil)))))
  (let ((emacs-d       "~/.emacs.d")
        (init-el       "~/.emacs.d/init.el")
        (elisp         "~/.emacs.d/elisp"))
    (when (file-directory-p elisp)   (delete-directory elisp t t))
    (when (file-exists-p    init-el) (delete-file      init-el))
    (copy-directory (expand-file-name "elisp"   repo-dir) elisp t t nil)
    (copy-file      (expand-file-name "init.el" repo-dir) init-el t t t t)
    (message (concat "Wrote " repo-dir " to " emacs-d))))

;; Source: https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors
(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; source: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))) ;;

;; source: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; source: https://stackoverflow.com/questions/3417438/close-all-buffers-besides-the-current-one-in-emacs
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun shell-here ()
  "Open a shell in current directory. Cd a current shell to there if it exists"
  (interactive)
  (let ((dir default-directory))
    (when (get-buffer "*shell*")
      (with-current-buffer "*shell*"
        (goto-char (point-max))
        (insert (concat "pushd " dir))
        (comint-send-input))))
  (shell))

;; source: https://www.reddit.com/r/emacs/comments/4zmly1/how_to_open_the_directory_of_a_file/
(defun open-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(defun bind-caps-lock-to-ctrl ()
  "Attempt to remap caps lock to CTRL in the OS. Requires emacs to be run with administrator priveleges.
May brick your operative system. Use at your own risk."
  (interactive)
  (when (eq system-type 'windows-nt)
    (run-shell-background
     ;; COMMAND
     ;; Source: https://superuser.com/questions/949385/map-capslock-to-control-in-windows-10
     (concat
      "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe -command \""
      "$hexified = \"\"\"00,00,00,00,00,00,00,00,02,00,00,00,1d,00,3a,00,00,00,00,00\"\"\".Split(',') | % { \"\"\"0x$_\"\"\"}; "
      "$kbLayout = 'HKLM:\\System\\CurrentControlSet\\Control\\Keyboard Layout'; "
      "New-ItemProperty -Path $kbLayout -Name \"\"\"Scancode Map\"\"\" -PropertyType Binary -Value ([byte[]]$hexified); "
      "\"")
     ;; WORKING DIRECTORY
     "c:"))
  (when (eq systep-type 'gnu/linux)
    (message "Not yet implemented. Please do this manually via e.g. gnome-tweak-tool settings")))

(provide 'useful-commands)
