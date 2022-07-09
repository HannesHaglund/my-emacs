(require 'subr-x)

(defun shell-repl-read-input (output-to-display)
  "Prompt user for shell command and return it.  Message OUTPUT-TO-DISPLAY."
  (read-shell-command (if shell-command-prompt-show-cwd
                          (format-message "Shell command in `%s': "
                                          (abbreviate-file-name
                                           default-directory))
                        (format "%s\n%s: "
                                (propertize (format "%s" output-to-display) 'face 'font-lock-builtin-face)
                                default-directory))
                      nil nil
		      (let ((filename
			     (cond
			      (buffer-file-name)
			      ((eq major-mode 'dired-mode)
			       (dired-get-filename nil t)))))
			(and filename (file-relative-name filename)))))


(defun shell-repl-string-last-line (str)
  "Return the last line in STR.  Internal utility."
  (with-temp-buffer
    (insert str)
    (thing-at-point 'line)))


(defun shell-repl-string-all-except-last-line (str)
  "Return all lines in STR except for the last one.  Internal utility."
  (with-temp-buffer
    (insert str)
    (goto-char (point-max))
    (beginning-of-line)
    (delete-region (point) (point-max))
    (buffer-substring (point-min) (point-max))))


(defun shell-repl-command-to-string-and-cwd (input)
  "Run shell command INPUT and return a list containing output and CWD after finishing."
  (let* ((actual-input (format "%s ; pwd -W" input))
         (output (string-trim (shell-command-to-string actual-input)))
         (output-meat (shell-repl-string-all-except-last-line output))
         (output-pwd (shell-repl-string-last-line output)))
    (list output-meat output-pwd)))


(defun shell-repl ()
  "Shell REPL - enter shell commands and get output.  Exit with empty input."
  (interactive)
  (let* ((input nil)
         (last-output "")
         (default-directory default-directory)) ; So we can change it later
    (while (not (string-blank-p (setq input (shell-repl-read-input last-output))))
      (let* ((output (shell-repl-command-to-string-and-cwd input))
             (output-meat (first output))
             (output-cwd (second output)))
        (message output-cwd)
        (setq default-directory output-cwd)
        (setq last-output output-meat)))
    (when last-output
      (kill-new last-output))
    (setq last-output nil)))


(provide 'shell-repl)