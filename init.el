;; ================================================================
;; Set up path
;; ================================================================
(message "Loading init.el...")
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; ================================================================
;; Fetch packages
;; ================================================================
(require 'package)

;; Setup...
(defun setup-melpa ()
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))
(package-initialize)
(setup-melpa)

;; ----------------------------------------------------------------
;; use-package
;; ----------------------------------------------------------------
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-verbose t))

;; ----------------------------------------------------------------
;; useful-commands
;; ----------------------------------------------------------------
;;
;; We load this as early as possible because it contains (overwrite-emacs-d)
;;
;; If something later fails after we've overwritten .emacs.d we want to be able
;; to overwrite it again even if we get an error
(use-package useful-commands)

;; ----------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------
(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :ensure t
  :after hydra)

;; ----------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------
(use-package helm
  :ensure t
  :config
  (helm-mode t)
  (setq helm-buffer-max-length nil))

;; ----------------------------------------------------------------
;; helm-xref
;; ----------------------------------------------------------------
(use-package helm-xref
  :ensure t
  :after helm
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

;; ----------------------------------------------------------------
;; helm-ag
;; ----------------------------------------------------------------
(use-package helm-ag
  :ensure t
  :after helm
  :config
  (setq helm-ag-use-temp-buffer t)
  (setq helm-ag-insert-at-point t)
  (setq helm-ag-fuzzy-match t)

  (when (eq system-type 'windows-nt)
    ;; Always use rg
    (setq helm-ag-base-command "rg --no-heading --vimgrep")
    ;; You will need to add the ripgrep executable to this path
    (add-to-list 'exec-path "C:\\Program Files\\ripgrep"))

  (when (eq system-type 'gnu/linux)
    ;; Use rg if available
    (when (string= "" (shell-command-to-string "hash rg"))
      (setq helm-ag-base-command "rg --no-heading"))))

;; ----------------------------------------------------------------
;; projectile
;; ----------------------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (projectile-global-mode)
  (require 'subr-x) ; Tags generation from projectile crashes otherwise
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-use-git-grep t)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien))

(use-package helm-projectile
  :ensure t
  :demand t                             ; Not having this causes some dependenant hydras to not load
  :after (helm projectile)
  :bind
  ("M-x"     . helm-M-x)
  ("C-x b"   . helm-buffers-list)
  ("C-x C-b" . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-c C-y" . helm-show-kill-ring)
  ("C-h h"   . helm-apropos)
  ("C-h f"   . helm-apropos)
  ("C-h y"   . helm-apropos)
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

;; ----------------------------------------------------------------
;; multiple-cursors
;; ----------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :after pretty-hydra
  :bind ("C-c m" . hydra-multiple-cursors/body)
  :config
  (require 'multiple-cursors)

  (defun mc/clear-cmds-to-run ()
    (interactive)
    (setq mc/cmds-to-run-once nil)
    (setq mc/cmds-to-run-for-all nil))

  (pretty-hydra-define hydra-multiple-cursors
    (:title "⤲ Multiple cursors - %(mc/num-cursors) active" :quit-key "q")
    ("Up"
     (("p" mc/mark-previous-like-this "next")
      ("P" mc/skip-to-previous-like-this "skip")
      ("M-p" mc/unmark-previous-like-this "unmark"))

     "Down"
     (("n" mc/mark-next-like-this "next")
      ("N" mc/skip-to-next-like-this "skip")
      ("M-n" mc/unmark-next-like-this "unmark"))

     "Insert"
     (("0" mc/insert-numbers "insert numbers" :exit t)
      ("A" mc/insert-letters "insert letters" :exit t))

     "Miscellaneous"
     (("l" mc/edit-lines "edit lines" :exit t)
      ("a" mc/mark-all-like-this "mark all" :exit t)
      ("s" mc/mark-all-in-region-regexp "search" :exit t)
      ("c" mc/clear-cmds-to-run "clear commands")))))

;; ----------------------------------------------------------------
;; restart-emacs
;; ----------------------------------------------------------------
(use-package restart-emacs
  :ensure t
  :commands (restart-emacs)
  :config
  (setq restart-emacs-restore-frames t))

;; ----------------------------------------------------------------
;; company-mode
;; ----------------------------------------------------------------
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("SPC"      . nil)
              ("RET"      . nil)
              ("<return>" . nil)
              ("<tab>"    . 'company-complete-selection)
              ("<C-tab>"  . 'company-select-next)
              ("C-`"      . 'company-select-previous)
              :map company-search-map
              ("<C-tab>"  . 'company-select-next)
              ("C-`"      . 'company-select-previous))
  :config
  (require 'company)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.1)
  (setq company-eclim-auto-save nil))

(use-package flx
  :ensure t)

(use-package company-fuzzy
  :ensure t
  :after (company flx)
  :config
  (global-company-fuzzy-mode 1)
  (setq company-fuzzy-sorting-function 'flx))

;; ----------------------------------------------------------------
;; wgrep
;; ----------------------------------------------------------------
(use-package wgrep
  :ensure t
  :config
  (require 'wgrep)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "\C-c\C-e"))

(use-package wgrep-helm
  :ensure t
  :after (wgrep helm)
  :config
  (require 'wgrep-helm))

;; ----------------------------------------------------------------
;; eglot
;; ----------------------------------------------------------------
(use-package eglot
  :ensure t
  :config
  (setq eglot-ignored-server-capabilites (quote (:documentHighlightProvider))))

(use-package hydra-eglot
  :after (pretty-hydra eglot)
  :bind ("C-c e" . hydra-eglot/body))

;; ----------------------------------------------------------------
;; region-occurences-highligher
;; ----------------------------------------------------------------
(use-package region-occurrences-highlighter
  :ensure t
  :config
  (set-face-attribute 'region-occurrences-highlighter-face nil
                      :background "thistle2"
                      :inverse-video nil)
  (add-hook 'prog-mode-hook #'region-occurrences-highlighter-mode)
  (add-hook 'org-mode-hook  #'region-occurrences-highlighter-mode)
  (add-hook 'text-mode-hook #'region-occurrences-highlighter-mode))

;; ----------------------------------------------------------------
;; basic-keybinds
;; ----------------------------------------------------------------
(use-package basic-keybinds
  :bind
  ("C-a" . beginning-of-code-line-or-buffer)
  ("C-e" . end-of-code-line-or-buffer)
  ("M-p" . scroll-up-bind)
  ("M-n" . scroll-down-bind)
  ("C-," . other-window)
  ("C-." . wind-bck)
  ("C-;" . other-frame)
  ("M-g" . goto-line))

;; ----------------------------------------------------------------
;; whitespace
;; ----------------------------------------------------------------
(use-package whitespace
  :config
  (setq whitespace-style '(face tabs lines-tail))
  (set-face-background 'whitespace-line "#FFC0C0")
  (set-face-foreground 'whitespace-line nil)
  (setq whitespace-line-column 120)
  (global-whitespace-mode t))

;; ----------------------------------------------------------------
;; expand-region
;; ----------------------------------------------------------------
(use-package expand-region
  :bind ("C-j" . er/expand-region)
  :ensure t
  :init
  ;; Make sure our binding isn't overriden by this binding
  (define-key lisp-interaction-mode-map (kbd "C-j") nil))

;; ----------------------------------------------------------------
;; dumb-jump
;; ----------------------------------------------------------------
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg))

;; ----------------------------------------------------------------
;; version control
;; ----------------------------------------------------------------
(use-package hydra-p4
  :commands (hydra-p4/body)
  :after pretty-hydra)

(use-package hydra-magit
  :after pretty-hydra)

(use-package p4
  :ensure t
  :config
  (setq p4-global-key-prefix nil))

(use-package magit
  :commands (magit-dispatch magit-file-dispatch magit-status)
  :ensure t)

(pretty-hydra-define hydra-vc (:color teal :title "⎆ Choose VC backend..." :quit-key "q")
  (""
   (("g" hydra-magit/body "magit")
    ("p" hydra-p4/body    "p4"))))

(defun appropriate-vc-hydra-body ()
  (interactive)
  (if (string= (vc-backend buffer-file-name) "Git") (hydra-magit/body) (hydra-vc/body)))

(global-set-key (kbd "C-c v") 'appropriate-vc-hydra-body)

;; ----------------------------------------------------------------
;; misc. hydras
;; ----------------------------------------------------------------
(use-package hydra-registers
  :after pretty-hydra
  :bind ("C-x r" . hydra-registers/body))
(use-package hydra-rectangle
  :after pretty-hydra
  :bind ("C-c r" . hydra-rectangle/body))
(use-package hydra-align
  :after pretty-hydra
  :bind ("C-c a" . hydra-align/body))
(use-package hydra-zoom
  :after pretty-hydra
  :bind ("C-c z" . hydra-zoom/body))
(use-package hydra-swedish
  :after pretty-hydra
  :bind ("C-c s" . hydra-swedish/body))
(use-package hydra-eval
  :after pretty-hydra
  :bind ("C-c l" . hydra-eval/body))
(use-package hydra-macro
  :after pretty-hydra
  :bind ("C-c o" . hydra-macro/body))
(use-package hydra-projectile
  :after pretty-hydra
  :bind ("C-c p" . hydra-projectile/body))
(use-package hydra-grep
  :after pretty-hydra
  :bind ("C-c g" . hydra-grep/body))
(use-package hydra-dired
  :after pretty-hydra
  :bind (:map dired-mode-map ("<tab>" . hydra-dired/body)))

;; ----------------------------------------------------------------
;; misc.
;; ----------------------------------------------------------------
(use-package mwim
  :ensure t)
(use-package bash-completion
  :ensure t)

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

;; truncate long lines
(setq-default truncate-lines t)

;; no tool bar
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; No scroll bar
(scroll-bar-mode -1)

;; ================================================================
;; Languages
;; ================================================================

;; src: https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the indent-tabs-mode nil
  (setq indent-tabs-mode nil)
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun apply-tab-settings ()
  (setq-default indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq default-tab-width 4)
  (infer-indentation-style))

(setq show-paren-delay 0)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'apply-tab-settings)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ----------------------------------------------------------------
;; c-mode
;; ----------------------------------------------------------------
(setq c-default-style "linux"
      c-basic-offset 4)

;; ----------------------------------------------------------------
;; octave-mode
;; ----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Src: https://emacs.stackexchange.com/questions/15164/commented-lines-shoot-off-to-column-32-in-octave-mode
;; (slightly modified)
(setq octave-mode-hook
      (lambda () (progn (setq octave-comment-char ?%)
                        (setq comment-start "%")
                        (setq indent-tabs-mode nil)
                        (setq comment-add 0)
                        (setq tab-width 4)
                        (setq tab-stop-list (number-sequence 4 200 4))
                        (setq octave-block-offset 4)
                        (define-key octave-mode-map (kbd "M-.") 'xref-find-definitions)
                        (defun octave-indent-comment ()
                          "A function for `smie-indent-functions' (which see)."
                          (save-excursion
                            (back-to-indentation)
                            (cond
                             ((octave-in-string-or-comment-p) nil)
                             ((looking-at-p "\\(\\s<\\)\\1\\{2,\\}") 0)))))))

;; ----------------------------------------------------------------
;; dockerfile-mode
;; ----------------------------------------------------------------
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  (add-to-list 'auto-mode-alist '("\\.docker\\'"  . dockerfile-mode))
  (add-hook 'dockerfile-mode-hook (lambda () (setq tab-width 4))))

;; ----------------------------------------------------------------
;; groovy-mode
;; ----------------------------------------------------------------
(use-package groovy-mode
  :ensure t)

;; ================================================================
;; Misc.
;; ================================================================

;; Change behavior of incremental search to inherit region
;; source: https://www.reddit.com/r/emacs/comments/b7yjje/isearch_region_search/
(defun isearch-region-to-advice (&optional not-regexp no-recursive-edit)
  "If a region is active, make this the isearch default search pattern."
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
      (setq deactivate-mark t)
      (isearch-yank-string search))))
(advice-add 'isearch-forward-regexp  :after 'isearch-region-to-advice)
(advice-add 'isearch-forward         :after 'isearch-region-to-advice)
(advice-add 'isearch-backward-regexp :after 'isearch-region-to-advice)
(advice-add 'isearch-backward        :after 'isearch-region-to-advice)

;; Dabbrev settings
(setq dabbrev-case-fold-search nil)
(setq dabbrev-upcase-means-case-search t)

;; Shell environment
(setenv "PAGER"  "cat")
(setenv "EDITOR" "emacs")

;; Other
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(auto-revert-mode 1)
(setq tags-add-tables nil)              ; Never ask "keep current list of tags table?"
(setq ring-bell-function 'ignore)       ; Disable warning sounds
(setq large-file-warning-threshold (* 200 1000 1000)) ; 200 megabytes

;; ================================================================
;; local-init
;; ================================================================

;; local-init.el is intended for machine-local configuration. Load it now.
(load-file (expand-file-name "local-init.el" (file-name-directory (or load-file-name (buffer-file-name)))))
