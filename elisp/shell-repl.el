(require 'subr-x)
(require 'cl-lib)
(require 'dired)

(defvar shell-repl-output-buffer
  "*shell-repl-log*"
  "Name of buffer to display shell-repl output in.")


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
  "Run shell command INPUT and return a list containing output and CWD."
  (let* ((actual-input (format "%s ; pwd -W" input))
         (output (string-trim (shell-command-to-string actual-input)))
         (output-meat (shell-repl-string-all-except-last-line output))
         (output-pwd (shell-repl-string-last-line output)))
    (list output-meat output-pwd)))


(defun shell-repl-append-to-output-buffer (cwd input output)
  "Append shell-repl output (namely CWD INPUT OUTPUT) to SHELL-REPL-OUTPUT-BUFFER."
  ;; Create the buffer if it doesn't exist.
  (unless (get-buffer shell-repl-output-buffer)
    (with-current-buffer (get-buffer-create shell-repl-output-buffer)
      (special-mode)
      (read-only-mode -1)))
  ;; Append to the buffer.
  (with-current-buffer (get-buffer shell-repl-output-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert (concat
               (propertize ">>> "                           'face 'font-lock-function-name-face)
               (propertize (format-time-string "%H:%M:%S ") 'face 'font-lock-function-name-face)
               (propertize cwd                              'face 'link)
               (propertize ": "                             'face 'font-lock-function-name-face)
               (propertize input                            'face 'font-lock-function-name-face)
               "\n"))
      (insert (format "%s\n" output))))
  ;; Scroll to EOB if it's open in a window.
  (let ((win (get-buffer-window shell-repl-output-buffer)))
    (when win
      (with-selected-window win
        (goto-char (point-max))
        ;; Recenter so that text fills the entire window.
        ;; For some reason, text moves up when the minibuffer expands, kinda ruining the effect.
        ;; Not sure how to get around that.
        (recenter -1)))))


(defun shell-repl (&optional dir)
  "Shell REPL - enter shell commands and get output.  Exit with empty input."
  (interactive)
  (let* ((input nil)
         (last-output "")
         (default-directory (if dir dir default-directory))) ; So we can change it later
    (while (not (string-blank-p (setq input (shell-repl-read-input last-output))))
      (let* ((output (shell-repl-command-to-string-and-cwd input))
             (output-meat (cl-first output))
             (output-cwd  (cl-second output)))
        (shell-repl-append-to-output-buffer default-directory input output-meat)
        (setq default-directory output-cwd)
        (setq last-output output-meat)))
    (when last-output
      (kill-new last-output))
    (setq last-output nil)))


(provide 'shell-repl)