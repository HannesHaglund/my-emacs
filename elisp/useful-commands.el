;; ================================================================
;; Basic keybinds
;; ================================================================
(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (forward-line 20)
  (recenter nil t))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (forward-line -20)
  (recenter nil t))

(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (forward-line 1))

(defun revert-buffer-no-confirm ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer nil t))

;; ================================================================
;; Grep utils
;; ================================================================
(require 'consult)

(defun region-when-active ()
  "Return region when region-active-p and deactivate it."
  (let ((result (if (region-active-p) (buffer-substring (region-beginning) (region-end)) nil)))
    (when (region-active-p) (deactivate-mark))
    result))

(defun consult-ripgrep-default-directory ()
  "Run consult-ripgrep in default-directory"
  (interactive)
  (consult-ripgrep default-directory (region-when-active)))

(defun consult-ripgrep-inherit-region ()
  "Run consult-ripgrep, but inherit region when active"
  (interactive)
  (consult-ripgrep nil (region-when-active)))

(defun consult-ripgrep-dir-inherit-region (dir)
  "Run consult-ripgrep, but inherit region when active"
  (interactive)
  (consult-ripgrep dir (region-when-active)))

(defun consult-line-inherit-region ()
  "Run consult-line, but inherit region if active"
  (interactive)
  (consult-line (region-when-active)))

;; ================================================================
;; Timeclock
;; ================================================================
(require 'timeclock)

(defun timeclock-project-summary-format-time-date (time)
  """Format TIME seconds since 1970 as a date string."""
  (format-time-string "%Y/%m/%d %a" time))

(defun timeclock-project-summary-format-time (time)
  """Format TIME seconds as hours and minutes."""
  ;; For some reason it's off by 1 hour unless I add 23 hours worth of seconds
  ;; Using format-time-string in this way is probably a hack... :(
  (format-time-string "%H:%M" (time-convert (+ (* 23 60 60) time) 'list)))

(defun timeclock-proj-accum-info-element-to-string (proj-accum-element)
  """Convert alist element PROJ-ACCUM-ELEMENT to a human-readable line."""
  (let* ((key      (car proj-accum-element))
         (time-sum (cdr proj-accum-element))
         (project  (nth 0 key))
         (date     (nth 1 key))
         (date-split (split-string date "/"))
         (year  (string-to-number (nth 0 date-split)))
         (month (string-to-number (nth 1 date-split)))
         (day   (string-to-number (nth 2 date-split)))
         (encoded-date     (encode-time (list 0 0 12 day month year nil nil nil))))
    (format "%s - Spent %s on %s"
            (timeclock-project-summary-format-time-date encoded-date)
            (timeclock-project-summary-format-time time-sum)
            project)))

(defun timeclock-project-summary-string ()
  """Generate lines containing project hour sums based on timelog."""
  (string-join (mapcar 'timeclock-proj-accum-info-element-to-string (timeclock-time-per-project-per-day)) "\n"))

(defun timeclock-project-summary ()
  """Open and refresh timeclock project summary buffer."""
  (interactive)
  (with-help-window "*timeclock-project-summary*"
    (princ "Timeclock Project Summary\n")
    (princ (format "Todays date is %s\n" (timeclock-project-summary-format-time-date nil)))
    (princ "\n")
    (princ (timeclock-project-summary-string))))

(defun timeclock-time-per-project-per-day ()
  """Generate alist based on timelog mapping (PROJECT DATE) to hours worked in the project at that day."""
  (when (file-readable-p timeclock-file)
    (let* ((event              nil)
           (event-time         nil)
           (event-time-decoded nil)
           (event-date         nil)
           (event-direction    nil)
           (event-reason       nil)
           (prev-date          nil)
           (prev-in-time       nil)
           (prev-in-reason     nil)
           (project-accum      '()))

      (with-temp-buffer
        (insert-file-contents timeclock-file)
        (goto-char 0)

        (while (setq event (timeclock-read-moment))
          (forward-line 1)

          (setq event-direction (nth 0 event))
          (setq event-time      (nth 1 event))
          (setq event-reason    (nth 2 event))
          (setq event-time-decoded (decode-time event-time))
          (setq event-date (format "%d/%d/%d"
                                   (nth 5 event-time-decoded) ; year
                                   (nth 4 event-time-decoded) ; month
                                   (nth 3 event-time-decoded))) ; day

          ;; (message (concat "Processing "
          ;;                  event-direction " "
          ;;                  (prin1-to-string event-time-decoded) " "
          ;;                  event-reason))

          ;; Handle missing out on previous date. Reset prev-in-time when changing date.
          (when (not (string= event-date prev-date))
            (setq prev-in-time event-time)
            (setq prev-in-reason "ERROR"))
          (setq prev-date event-date)

          ;; Process in
          (when (string= "i" event-direction)
            (setq prev-in-time event-time)
            (setq prev-in-reason event-reason))

          ;; Process out
          (when (or (string= "o" event-direction) (string= "O" event-direction))
            ;; Ignore entry if it's the first one of the day. First should be an "i".
            (when (and prev-in-time prev-in-reason)
              (let* ((time-diff (time-subtract event-time prev-in-time))
                     (cur-val   (alist-get (list prev-in-reason event-date) project-accum nil nil #'equal)))
                ;; Update total for that project
                (setf (alist-get (list prev-in-reason event-date) project-accum nil nil #'equal)
                      (+ time-diff (if cur-val cur-val 0))))))))
      ;; Return value
      project-accum)))

(defun timeclock-out-no-reason ()
  """Run timeclock-out, but without any specified REASON."""
  (interactive)
  (timeclock-out ""))

(defun timeclock-reread-and-status-string ()
  "Reread log and show status."
  (interactive)
  (timeclock-reread-log)
  (message (timeclock-status-string nil t)))

;; ================================================================
;; Align
;; ================================================================
(defun align-each (regexp)
  "Align each occurence of REGEXP in region."
  (interactive "sRegexp: ")
  (align-regexp
   (if (use-region-p) (region-beginning) (point-min))
   (if (use-region-p) (region-end)       (point-max))
   (concat "\\(\\s-*\\)" regexp)
   1
   align-default-spacing
   1))

;; ================================================================
;; Other
;; ================================================================

(defvar run-shell-background-output-buffer-name "*run-shell-background output*")

(defun font-available-p (font-name)
  "Return t if font FONT-NAME is available"
  (find-font (font-spec :name font-name)))

(defun cd-command (to-what-dir)
  "Return shell command used to change working directory to TO-WHAT-DIR."
  (concat
   ;; On windows cmd, we first need to change the drive we are operating on
   (when (eq system-type 'windows-nt) (concat (substring to-what-dir 0 2) " && "))
   ;; The actual cd
   "cd " to-what-dir))

(defun run-shell-background (command cwd)
  "Run shell command COMMAND with working directory CWD asynchronously."
  (interactive "sShell command: \nDCWD: ")
  (let ((full-command (concat (cd-command cwd) " && " command)))
    (write-to-run-shell-background-buffer full-command)
    (let ((proc (start-process-shell-command (replace-in-string "%" "%%" command)
                                             run-shell-background-output-buffer-name
                                             full-command)))
      (set-process-sentinel proc #'run-shell-background-sentinel))))

(defun run-shell-background-sentinel (process msg)
  "Process sentinel for run-shell-background, to be callbacked with PROCESS and MSG through 'set-process-sentinel'."
  (when (memq (process-status process) '(exit signal))
    (message (concat (process-name process) " - " msg))
    (write-to-run-shell-background-buffer (concat "Sentinel: " (process-name process) " - " msg))
    (when (not (string= msg "finished\n"))
      (display-buffer run-shell-background-output-buffer-name)
      (with-current-buffer run-shell-background-output-buffer-name (goto-char (point-max))))))

(defun write-to-run-shell-background-buffer (msg)
  "Write string MSG to shell-background output buffer."
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

(defun clear-run-shell-background-output-buffer ()
  "Clear buffer with name 'run-shell-background-output-buffer-name'."
  (interactive)
  (with-current-buffer (get-buffer-create run-shell-background-output-buffer-name)
    (erase-buffer)))

(defun replace-in-string (what with in)
  "Replace WHAT with WITH in string IN."
  (when in (replace-regexp-in-string (regexp-quote what) with in nil 'literal)))

(defun shell-command-to-kill-ring (cmd)
  "Run shell command CMD and yank the output to 'kill-ring'."
  (interactive "sShell command: ")
  (let ((output (shell-command-to-string cmd)))
    (message (concat "Shell output:\n" output))
    (kill-new output)))

(defun my-emacs-configure-load-on-startup (repo-dir)
  "Add line in .emacs loading REPO-DIR/init.el."
  (interactive (list (read-directory-name "my-emacs directory: "
                                          ;; folder of init.el buffer if it exists, or nil
                                          (let ((init-el-buffer (get-buffer "init.el")))
                                            (if init-el-buffer
                                                (file-name-directory (buffer-file-name init-el-buffer))
                                              nil)))))
  (let ((dot-emacs "~/.emacs")
        (string-to-add (format "(load-file \"%s\")" (concat repo-dir "init.el"))))
    (when (not (file-exists-p dot-emacs)) (write-region "" nil dot-emacs))
    (write-region string-to-add nil dot-emacs 'append)
    (message (concat "Wrote to " dot-emacs))))

;; Source: https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors
(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
  Buffers in modified (not yet saved) state in Emacs will not be reverted.
  They will be reverted though if they were modified outside Emacs.
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

;; source: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
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
          (set-buffer-modified-p nil)))))) ;;

;; source: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
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
  "Kill all buffers except (current-buffer)."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun shell-here ()
  "Open a shell in current directory.  Cd a current shell to there if it exists."
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
      (if (eq system-type 'windows-nt) (browse-url-default-windows-browser (expand-file-name default-directory))
        (browse-url-of-file (concat "file://" (expand-file-name default-directory))))
    (error "No `default-directory' to open")))

(defun bind-caps-lock-to-ctrl ()
  "Attempt to remap caps lock to CTRL in the OS.  Requires Emacs to be run with administrator priveleges.  May brick your operative system, so use at your own risk."
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
  (when (eq system-type 'gnu/linux)
    (message "Not yet implemented. Please do this manually via e.g. gnome-tweak-tool settings")))

(defun indent-buffer ()
  "Indent current buffer."
  (interactive)
  (when (and (derived-mode-p 'prog-mode) (not (derived-mode-p 'python-mode)))
    (indent-region (point-min) (point-max))))

(defun insert-skeleton-editorconfig ()
  "Insert some skeleton .editorconfig file contents."
  (interactive)
  (insert (concat
           "root                     = true # This is the topmost.editorconfig" "\n\n"
           "[*] # On all files..."                                              "\n"
           "insert_final_newline     = false"                                   "\n"
           "charset                  = utf-8"                                   "\n"
           "indent_style             = space"                                   "\n"
           "indent_size              = 4"                                       "\n"
           "trim_trailing_whitespace = true"                                    "\n"
           "indent_size = 4"                                                    "\n")))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'useful-commands)
;;; useful-commands.el ends here