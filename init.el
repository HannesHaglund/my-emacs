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
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))))

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
(ensure-package-installed 'helm-ag)
(helm-mode t)

(ensure-package-installed 'dumb-jump)

(ensure-package-installed 'multiple-cursors)

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
    (define-key map (kbd "M-p") 'scroll-up-bind)
    (define-key map (kbd "M-n") 'scroll-down-bind)
    (define-key map (kbd "C-,") 'other-window)
    (define-key map (kbd "C-.") 'wind-bck)
    (define-key map (kbd "C-;") 'other-frame)
    ;; Repo navigation
    (define-key map (kbd "M-g M-l") 'goto-line)
    (define-key map (kbd "M-g M-g") 'dumb-jump-go)
    (define-key map (kbd "M-g M-h") 'dumb-jump-go-other-window)
    (define-key map (kbd "M-g M-b") 'dumb-jump-back)
    (define-key map (kbd "M-g M-r") 'helm-do-ag-project-root)
    (define-key map (kbd "M-g M-t") 'helm-do-ag)
    ;; Helm override
    (define-key map (kbd "M-x") 'helm-M-x)
    (define-key map (kbd "C-x b") 'helm-buffers-list)
    (define-key map (kbd "C-x C-b") 'helm-buffers-list)
    (define-key map (kbd "C-x C-f") 'helm-find-files)
    ;; Multiple cursors
    (define-key map (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (define-key map (kbd "C->") 'mc/mark-next-like-this)
    (define-key map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-<") 'mc/mark-all-like-this)
    map)
  "my-keys-minor-mode keymap.")

;; Refuses to work inside the key map
;; Doing both cause I don't know which works
(global-set-key [C-tab] 'c-tab-bind)
(global-set-key (kbd "<C-tab>") 'c-tab-bind)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

;; ================================================================
;; Visuals
;; ================================================================

;; Theme
(load-theme 'adwaita t)

;; Prevent startup screen
(setq inhibit-startup-screen t)

;; linum mode in code buffers
(add-hook 'prog-mode-hook 'linum-mode)

;; visual line mode
(global-visual-line-mode t)

;; no tool bar
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; Full screen on startup
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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
;; Misc.
;; ================================================================
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(require 'mye-no-mouse)                 ; Disables mouse
