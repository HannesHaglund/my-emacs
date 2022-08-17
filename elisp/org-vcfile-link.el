;; -*- lexical-binding: t -*-

(require 'org)
(require 'vc)

(defvar org-vcfile-quit-with-q t "Bind q to 'quit-window' in opened vcfile links when non-nil.")
(defvar org-vcfile-use-fancy-read-revision-for-git t "Non-nil to use a fancier revision read for git files.")
(defvar org-vcfile-fancy-read-revision-git-cmd
  "git log --date=short --pretty=format:\"%h%x09%ad%x09%an%x09%s\" -- "
  "Shell command used to fetch candidates for completing read on git revisions for a file.")

(define-minor-mode quit-with-q-mode
  "Toggle quit-with-q-mode.
Adds a keybinding to 'quit-window' by pressing q."
  :init-value nil
  :keymap `((,(kbd "q") . (lambda () (interactive) (quit-window t)))))


(defun org-vcfile-git-revision-completion-table (completions)
  "Completion table that force COMPLETIONS to appear in original order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))


(defun org-vcfile-git-read-revision (prompt file)
  "Read a git revision of FILE, with PROMPT.
Git-specific, as vc-read-revision does not show individual SHAs."
  (let* ((default-directory (file-name-directory file))
         (revs-hist (split-string (shell-command-to-string
                                   (concat org-vcfile-fancy-read-revision-git-cmd file))
                                  "\n"))
         (completions (org-vcfile-git-revision-completion-table revs-hist))
         (rev (completing-read prompt completions)))
    (car (split-string rev "	"))))


(defun org-vcfile-read-revision (prompt file)
  "Read a vc revision of FILE, with PROMPT."
  (if (and org-vcfile-use-fancy-read-revision-for-git
           (equal (vc-responsible-backend file) 'Git))
      ;; Then
      (org-vcfile-git-read-revision prompt file)
    ;; Else
    (vc-read-revision prompt file)))


(defun org-vcfile-link (file revision &optional line)
  "Create new vcfile link from FILE, REVISION and optionally LINE."
  (if line
      ;; Then
      (format "%s:%s~%s~%s" "vcfile"
              file
              revision
              (concat "::" (number-to-string line)))
    ;; Else
    (format "%s:%s~%s~" "vcfile" file revision)))


(defun org-vcfile-link-complete ()
  "Complete a vcfile link."
  (let* ((file (read-file-name "File: "))
         (rev (org-vcfile-read-revision "Revision: " file)))
    (org-vcfile-link file rev nil)))


(defun org-vcfile-split-link (link)
  "Return an association list with information contained in LINK."
  (let* ((option (and (string-match "^.*::\\(.*\\)\\'" link)
		      (match-string 1 link)))
         (rev    (and (string-match "^.*\\~\\(.*\\)\\~" link)
	              (match-string 1 link)))
         (file   (and (string-match "^\\(.*\\)\\~.*\\~" link)
                      (match-string 1 link)))
         (option-line (if (and option (string-match-p "\\`[0-9]+\\'" option)) (string-to-number option) nil))
         (option-search (if (equal option-line nil) option nil)))
    `((file          . ,file)
      (revision      . ,rev)
      (option        . ,option)
      (option-line   . ,option-line)
      (option-search . ,option-search))))


(defun org-vcfile-link-follow (link)
  "Follow a vcfile LINK (formatted as vcfile:<sha>,<path><optional search>)."
  (let* ((link-alist (org-vcfile-split-link link))
         (file     (cdr (assoc 'file          link-alist)))
         (revision (cdr (assoc 'revision      link-alist)))
         (line     (cdr (assoc 'option-line   link-alist)))
         (search   (cdr (assoc 'option-search link-alist))))

    (unless file (user-error "Could not deduce file from link"))
    (unless file (user-error "Could not deduce revision from link"))

    (switch-to-buffer-other-window (vc-find-revision file revision))

    (when line (org-goto-line line))
    (when search (org-link-search search))
    (when org-vcfile-quit-with-q (quit-with-q-mode 1))))


(defun org-vcfile-current-pos-args ()
  "Args to provide to 'org-vcfile-link' to get a link to current pos.
Also works in buffers created by 'vc-find-revision', despite not technically being under VC."
  (list (or
         ;; Special handling for vc buffers, with the format <PATH>~<SHA>~
         (and (string-match "^\\(.*\\)\\~\\(.*\\)\\~" (buffer-file-name)) (match-string 1 (buffer-file-name)))
         (buffer-file-name))
        (or
         ;; First try to get revision through vc
         (vc-working-revision (buffer-file-name))
         ;; If that fails, first assume we are in a vc buffer and get it through the buffer name
         (and (string-match "^\\(.*\\)\\~\\(.*\\)\\~" (buffer-file-name)) (match-string 2 (buffer-file-name)))
         ;; Finally, just ask for it
         (org-vcfile-read-revision "Could not find revision automatically. Revision: " (buffer-file-name)))
        (line-number-at-pos (point) t)))


;; TODO, would be nice if this respected org-link-file-path-type, but I don't know if that's possible
(defun org-vcfile-store ()
  "Store a vcfile link to current position."
  (let* ((l (apply 'org-vcfile-link (org-vcfile-current-pos-args))))
    (when l
      (org-link-store-props :type "vcfile"
                            :link l
                            :description nil))))


(org-link-set-parameters "vcfile"
                         :complete 'org-vcfile-link-complete
                         :follow 'org-vcfile-link-follow
                         :store 'org-vcfile-store)


(provide 'org-vcfile-link)