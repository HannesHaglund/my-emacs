;; -*- lexical-binding: t -*-

(require 'org)
(require 'vc)


(defun org-vcfile-revision-completion-table (completions)
  "Completion table that force COMPLETIONS to appear in original order."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))


;; TODO use vc-retrieve-revision outside of git
;; With git, it doesn't give enough options, but we want to be able to support links that are not git
(defun org-vcfile-read-revision (prompt file)
  "Read a vc revision of FILE, with PROMPT."
  (let* ((default-directory (file-name-directory file))
         (revs-hist (split-string (shell-command-to-string
                                   (concat "git log --date=relative --pretty=format:\"%h%x09%ad%x09%an%x09%s\" -- " file))
                                  "\n"))
         (completions (org-vcfile-revision-completion-table revs-hist))
         (rev (completing-read prompt completions)))
    (car (split-string rev "	"))))


(defvar org-vcfile-rev-file-sep "," "Sepearator for revision and file portions of link.")


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