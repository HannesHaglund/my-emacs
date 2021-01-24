(pretty-hydra-define hydra-directory (:color blue :title "⛳ Directory: %(default-directory-fun)" :quit-key "q")
  ("Navigation"
   (("f" helm-find "file")
    ("g" helm-ag "grep")
    ("," helm-ag-pop-stack "grep pop stack" :color red))
   "Other"
   (("s" shell-here "shell here")
    ("d" dired-here "dired")
    ("o" open-file-directory "Open dir in OS"))))

(defun dired-here ()
  "Open dired in current directory"
  (interactive)
  (dired default-directory))

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

(defun default-directory-fun () default-directory)

(provide 'hydra-directory)
