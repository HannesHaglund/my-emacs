;; ================================================================
;; Set up path
;; ================================================================
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; ================================================================
;; Fetch packages
;; ================================================================
(require 'package)

;; Setup...
(defun setup-melpa ()
  (add-to-list 'package-archives
               '("melpa-stable" .
                 "http://melpa-stable.milkbox.net/packages/") t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (when (>= emacs-major-version 24)
      (package-initialize)
      (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)))

;; src:
;; https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (progn (package-refresh-contents)
                  (package-install package)
                  package)
         nil)))
   packages))

(package-initialize)
(setup-melpa)

;; Fetch them
(ensure-package-installed 'helm)
(helm-mode t)
(setq helm-buffer-max-length nil)

(ensure-package-installed 'helm-xref)
(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

(ensure-package-installed 'dumb-jump)
(ensure-package-installed 'multiple-cursors)

(ensure-package-installed 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(require 'subr-x) ; Tags generation from projectile crashes otherwise
(setq projectile-use-git-grep t)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(add-to-list 'projectile-globally-ignored-directories "Build")

(ensure-package-installed 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

(ensure-package-installed 'helm-ag)
;; Note: Requires the user to manually install rg
(setq helm-ag-fuzzy-match t)
(setq helm-ag-base-command "rg --no-heading")
(when (eq system-type 'windows-nt)
  (setq helm-ag-base-command "rg --no-heading --vimgrep")
  ;; Note: On Windows, you will need to add the ripgrep executable to this path
  (add-to-list 'exec-path "C:\\Program Files\\ripgrep"))

(ensure-package-installed 'expand-region)

;; ================================================================
;; Key binds
;; ================================================================

(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (let (value)
    (dotimes (i 10 value)
      ;; Dont know why this is reversed
      (scroll-up-line))))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (let (value)
    (dotimes (i 10 value)
      ;; Dont know why this is reversed
      (scroll-down-line))))

(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (next-line))

;; src: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; KEY BIND LIST
    ;; Common
    (define-key map (kbd "M-p")         'scroll-up-bind)
    (define-key map (kbd "M-n")         'scroll-down-bind)
    (define-key map (kbd "C-,")         'other-window)
    (define-key map (kbd "C-.")         'wind-bck)
    (define-key map (kbd "C-;")         'other-frame)
    (define-key map (kbd "C-/")         'dabbrev-completion)
    (define-key map (kbd "M-/")         'complete-tag)
    (define-key map (kbd "C-M-.")       'helm-etags-select)
    (define-key map (kbd "M-g")         'goto-line)
    ;; Helm
    (define-key map (kbd "M-x")         'helm-M-x)
    (define-key map (kbd "C-x b")       'helm-buffers-list)
    (define-key map (kbd "C-x C-b")     'helm-buffers-list)
    (define-key map (kbd "C-x C-f")     'helm-find-files)
    (define-key map (kbd "C-c C-y")     'helm-show-kill-ring)
    ;; Helm Swoop
    (define-key map (kbd "M-i")         'helm-do-ag-this-file)
    (define-key map (kbd "M-I")         'helm-ag-pop-stack)
    (define-key map (kbd "C-c M-i")     'helm-do-ag-buffers)
    (define-key map (kbd "C-c p s s")   'helm-do-ag-project-root)
    ;; Multiple cursors
    (define-key map (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (define-key map (kbd "C->")         'mc/mark-next-like-this)
    (define-key map (kbd "C-<")         'mc/mark-previous-like-this)
    (define-key map (kbd "C-S-c C-<")   'mc/mark-all-like-this)
    ;; Expand region
    (define-key map (kbd "C-=")         'er/expand-region)
    map)
  "my-keys-minor-mode keymap.")

;; Refuses to work inside the key map
;; Doing both cause I don't know which works
(global-set-key [C-tab] 'c-tab-bind)
(global-set-key (kbd "<C-tab>") 'c-tab-bind)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " My-Keys")

(my-keys-minor-mode 1)

;; ================================================================
;; Visuals
;; ================================================================

;; Theme
(load-theme 'adwaita t)

;; Prevent startup screen
(setq inhibit-startup-screen t)

;; linum and column in prog mode
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

;; visual line mode
(global-visual-line-mode t)

;; no tool bar
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; Whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail))
(set-face-background 'whitespace-line "#FFC0C0")
(set-face-foreground 'whitespace-line nil)
(global-whitespace-mode t)

;; ================================================================
;; Languages
;; ================================================================

;; src: https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun apply-tab-settings ()
  (setq-default indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq default-tab-width 4))

;; All modes
(apply-tab-settings)
(infer-indentation-style)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; c-mode
;; Company c mode
(setq c-default-style "linux"
      c-basic-offset 3)

;; Wurst mode
(require 'wurstscript-mode)

;; prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

;; ================================================================
;; Useful interactive functions
;; ================================================================

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

;; ================================================================
;; Misc.
;; ================================================================
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Disable mouse
(require 'mye-no-mouse)
(disable-mouse-mode 1)
(setq large-file-warning-threshold (* 200 1000 1000)) ; 200 megabytes

(setq dabbrev-case-fold-search nil)
(setq dabbrev-upcase-means-case-search t)
