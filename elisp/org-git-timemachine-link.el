;; -*- lexical-binding: t -*-

(require 'org)
(require 'vc)


(defvar org-vcfile-rev-file-sep "," "Sepearator for revision and file portions of link.")
(defvar org-vcfile-use-fancy-read-revision-for-git t "Non-nil to use a fancier revision read for git files.")
(defvar org-vcfile-fancy-read-revision-git-cmd
  "git log --date=short --pretty=format:\"%h%x09%ad%x09%an%x09%s\" -- "
  "Shell command used to fetch candidates for completing read on git revisions for a file.")


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


(defun org-vcfile-link-complete ()
  "Complete a vcfile link."
  (let* ((file (read-file-name "File: "))
         (rev (org-vcfile-read-revision "Revision: " file)))
    (format "%s:%s%s%s" "vcfile" rev org-vcfile-rev-file-sep file)))


(defun org-vcfile-link-follow (link)
  "Follow a vcfile LINK (formatted as vcfile:<sha>,<path><optional search>)."
  (let* ((split (split-string link org-vcfile-rev-file-sep))
         (rev (car split))
         (link (string-join (cdr split) org-vcfile-rev-file-sep))
         (option (and (string-match "::\\(.*\\)\\'" link)
		      (match-string 1 link)))
         (file (if (not option) link
		 (substring link 0 (match-beginning 0))))
         (line (if (and option (string-match-p "\\`[0-9]+\\'" option)) (string-to-number option) nil))
         (search (if line nil option)))
    (switch-to-buffer-other-window (vc-find-revision file rev))
    (when line (org-goto-line line))
    (when search (org-link-search search))))


(org-link-set-parameters "vcfile"
                         :complete 'org-vcfile-link-complete
                         :follow 'org-vcfile-link-follow)


(provide 'org-git-timemachine-link)