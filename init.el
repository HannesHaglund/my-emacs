;; ================================================================
;; Set up path
;; ================================================================
(message "Loading init.el...")

(defun relative-path (path)
  """Return <path to file being evaluated>/PATH."""
  (concat (file-name-directory (or load-file-name buffer-file-name))
          path))

(add-to-list 'load-path (relative-path "elisp"))

;; ================================================================
;; Fetch packages
;; ================================================================
(require 'package)

;; Setup...
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; ----------------------------------------------------------------
;; use-package
;; ----------------------------------------------------------------
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
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
;; exec-path-from-shell
;; ----------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; ----------------------------------------------------------------
;; diminish
;; ----------------------------------------------------------------
(use-package diminish
  :ensure t
  :commands (diminish))

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
  :demand t
  :defer nil
  :bind
  ("M-x"     . helm-M-x)
  ("C-x b"   . helm-buffers-list)
  ("C-x C-b" . helm-buffers-list)
  ("C-x C-f" . helm-find-files)
  ("C-c C-y" . helm-show-kill-ring)
  ("C-h h"   . helm-apropos)
  ("C-h f"   . helm-apropos)
  ("C-h y"   . helm-apropos)
  :bind (:map helm-grep-map
              ("C-c C-e" . helm-grep-run-save-buffer))
  :config
  (helm-mode t)
  (setq helm-buffer-max-length nil)
  (add-hook 'helm-grep-mode-hook 'wgrep-change-to-wgrep-mode)
  (diminish 'helm-mode))

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
  (setq helm-ag-insert-at-point nil)
  (setq helm-ag-fuzzy-match nil)

  (advice-add 'helm-ag--save-current-context :before 'xref-push-marker-stack)

  (when (eq system-type 'windows-nt)
    ;; Always use rg
    (setq helm-ag-base-command "rg --no-heading --vimgrep"))

  (when (eq system-type 'gnu/linux)
    ;; Use rg if available
    (when (string= "" (shell-command-to-string "hash rg"))
      (setq helm-ag-base-command "rg --no-heading"))))

;; ----------------------------------------------------------------
;; helm-tramp
;; ----------------------------------------------------------------
(use-package helm-tramp
  :ensure t
  :after helm
  :config
  (setq tramp-default-method "ssh")
  (define-key global-map (kbd "C-c t") 'Helm-Tramp))

;; ----------------------------------------------------------------
;; projectile
;; ----------------------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (diminish 'projectile-mode)
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
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

;; ----------------------------------------------------------------
;; default-text-scale
;; ----------------------------------------------------------------
(use-package default-text-scale
  :ensure t
  :after pretty-hydra
  :bind ("C-c z" . hydra-text-scale/body)
  :config
  (pretty-hydra-define hydra-text-scale (:color red :title "🔍 Zoom" quit-key "q" :pre (default-text-scale-mode))
    ("Actions"
     (("i" default-text-scale-increase "zoom in")
      ("o" default-text-scale-decrease "zoom out")))))

;; ----------------------------------------------------------------
;; multiple-cursors
;; ----------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :after pretty-hydra
  :bind ("C-c m"   . hydra-multiple-cursors/body)
  :config
  (require 'multiple-cursors)

  (defun mc/add-default-cmds-to-run-once ()
    (dolist (cmd '('hydra-multiple-cursors/mc/mark-previous-like-this
                   'hydra-multiple-cursors/mc/skip-to-previous-like-this
                   'hydra-multiple-cursors/mc/unmark-previous-like-this
                   'hydra-multiple-cursors/mc/mark-next-like-this
                   'hydra-multiple-cursors/mc/skip-to-next-like-this
                   'hydra-multiple-cursors/mc/unmark-next-like-this
                   'hydra-multiple-cursors/mc/insert-numbers
                   'hydra-multiple-cursors/mc/insert-numbers-and-exit
                   'hydra-multiple-cursors/mc/insert-letters
                   'hydra-multiple-cursors/mc/insert-letters-and-exit
                   'hydra-multiple-cursors/mc/edit-lines
                   'hydra-multiple-cursors/mc/mark-all-like-this
                   'hydra-multiple-cursors/mc/mark-all-in-region-regexp
                   'hydra-multiple-cursors/mc/clear-cmds-to-run
                   'beginning-of-code-line-or-buffer
                   'end-of-code-line-or-buffer)
                 (add-to-list 'mc/cmds-to-run-once cmd))))

  (mc/add-default-cmds-to-run-once)

  (defun mc/clear-cmds-to-run ()
    (interactive)
    (setq mc/cmds-to-run-once nil)
    (setq mc/cmds-to-run-for-all nil)
    (mc/add-default-cmds-to-run-once))

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
  (diminish 'company-mode)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.05)
  (setq company-eclim-auto-save nil))

;; ----------------------------------------------------------------
;; wgrep
;; ----------------------------------------------------------------

(defun wgrep-abort-changes-and-exit ()
  "Abort wgrep and kill buffer."
  (interactive)
  (wgrep-abort-changes)
  (kill-buffer-and-window))

(defun wgrep-finish-edit-and-exit ()
  "Finish wgrep edit and kill buffer."
  (interactive)
  (wgrep-finish-edit)
  (kill-buffer-and-window))

(defun bind-wgrep-keys ()
  "Bind custom wgrep-mode keys."
  (define-key wgrep-original-mode-map (kbd "C-c C-g") 'wgrep-abort-changes-and-exit)
  (define-key wgrep-original-mode-map (kbd "C-x C-s") 'wgrep-finish-edit-and-exit))

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode)
  :config
  ;; Due to wgrep being weird, keybinds must be set up in a hook...
  (add-hook 'wgrep-setup-hook #'bind-wgrep-keys)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "\C-c \C-e"))

(use-package wgrep-helm
  :ensure t
  :after (wgrep helm))

;; ----------------------------------------------------------------
;; lsp-mode
;; ----------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :defer nil

  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; This is recommended by https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold (* 100 1000 1000))   ; 100mb
  (setq read-process-output-max (* 1024 1024)) ; 1mb

  ;; Unbind key so it doesn't override our basic-keybinds keybind
  :bind (:map lsp-signature-mode-map ("M-p" . nil))

  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp))
  :commands lsp)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (when (eq system-type 'windows-nt)
    ;; Fixes a bug where the linters are not found on Windows
    (setq flycheck-python-pylint-executable "pylint")
    (setq flycheck-python-flake8-executable "flake8")))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :after (lsp-mode helm)
  :commands helm-lsp-workspace-symbol)

(use-package dap-mode
  :ensure t
  :after lsp-mode)

;; ----------------------------------------------------------------
;; basic-keybinds
;; ----------------------------------------------------------------

(use-package helm-swoop
  :ensure t
  :config
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-use-fuzzy-match nil)
  ;; We hijack the xref marker stack
  (add-hook 'helm-swoop-before-goto-line-action-hook 'xref-push-marker-stack)
  ;; Use region for pre-input
  (setq helm-swoop-pre-input-function
        (lambda ()
          (if mark-active
              (buffer-substring-no-properties (mark) (point))
            ""))))

(use-package basic-keybinds
  :after (pretty-hydra helm-swoop)
  :bind
  ("C-a" . beginning-of-code-line-or-buffer)
  ("C-e" . end-of-code-line-or-buffer)
  ("C-k" . kill-whole-line)
  ("M-p" . scroll-up-bind)
  ("M-n" . scroll-down-bind)
  ("C-," . other-window)
  ("C-." . wind-bck)
  ("C-;" . other-frame)
  ("M-g" . goto-line)
  ("C-z" . undo)
  ("C-o" . helm-swoop)
  ("C-x k" . kill-buffer-and-window))

;; ----------------------------------------------------------------
;; whitespace
;; ----------------------------------------------------------------
(use-package whitespace
  :config
  (setq whitespace-style '(face tabs lines-tail))
  (set-face-foreground 'whitespace-line nil)
  (setq whitespace-line-column 120)
  (global-whitespace-mode t)
  (diminish 'global-whitespace-mode))

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
;; avy
;; ----------------------------------------------------------------
(use-package avy
  :ensure t
  :bind ("C-v" . avy-goto-char-timer))

;; ----------------------------------------------------------------
;; persistent-scratch
;; ----------------------------------------------------------------
(use-package persistent-scratch
  :ensure t
  :init
  (persistent-scratch-setup-default))

;; ----------------------------------------------------------------
;; dumb-jump
;; ----------------------------------------------------------------
(use-package dumb-jump
  :ensure t
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)
  (setq dumb-jump-prefer-searcher 'rg))

;; ----------------------------------------------------------------
;; version control
;; ----------------------------------------------------------------

;; Display whether a line is changed in VC on fringe
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

(use-package hydra-p4
  :commands (hydra-p4/body)
  :after pretty-hydra)

(use-package p4
  :ensure t
  :commands (
             p4-add p4-edit p4-move p4-delete p4-revert p4-opened p4-status p4-annotate
             p4-filelog p4-diff p4-submit p4-update p4-reconcile p4-client p4-branch p4-branches
             p4-changes p4-diff2 p4-describe p4-reopen p4-depot-find-file p4-files p4-get-client-name p4-help
             p4-integ p4-have p4-info p4-job p4-jobs p4-label p4-labels p4-labelsync
             p4-print p4-set-p4-port p4-refresh p4-resolve p4-toggle-vc-mode p4-user p4-users p4-version
             p4-where p4-fix p4-diff-all-opened p4-ediff)
  :config
  (setq p4-global-key-prefix nil))

(use-package magit
  :commands (magit-dispatch magit-file-dispatch magit-status magit-toplevel)
  :ensure t)

(pretty-hydra-define hydra-vc (:color teal :title "⎆ Choose VC frontend..." :quit-key "q")
  ("Available frontends"
   (("g" magit-status  "magit")
    ("p" hydra-p4/body "p4"))))

(defun appropriate-vc-hydra-body ()
  "Try to open UI for the version control used in 'default-directory'.  Prompt the user with a hydra if this is not possible."
  (interactive)
  (if (or (string= (vc-backend buffer-file-name) "Git") (magit-toplevel default-directory))
      ;; We are in a git project...
      (magit-status)
    ;; We are not. Prompt the user.
    (hydra-vc/body)))

(global-set-key (kbd "C-c v")   'appropriate-vc-hydra-body)

;; ----------------------------------------------------------------
;; misc. hydras
;; ----------------------------------------------------------------
(use-package hydra-registers
  :after pretty-hydra
  :bind
  ("C-x r" . hydra-registers/body))
(use-package hydra-rectangle
  :after pretty-hydra
  :bind
  ("C-c r" . hydra-rectangle/body))
(use-package hydra-align
  :after pretty-hydra
  :bind
  ("C-c a" . hydra-align/body))
(use-package hydra-swedish
  :after pretty-hydra
  :bind
  ("C-c ;" . hydra-swedish/body))      ; C-c ö, kind of
(use-package hydra-macro
  :after pretty-hydra
  :bind
  ("C-c o" . hydra-macro/body))
(use-package hydra-projectile
  :after pretty-hydra
  :bind
  ("C-c p" . hydra-projectile/body))
(use-package hydra-timeclock
  :after pretty-hydra
  :bind
  ("C-c c" . hydra-timeclock/body))


;; ----------------------------------------------------------------
;; dired-mode
;; ----------------------------------------------------------------
(use-package dired
  :config
  (setq dired-listing-switches "-alh")    ; List file sizes in a human-readable format
  (defun dired-here () (interactive) (dired default-directory))
  (bind-key "C-x d" 'dired-here)
  (bind-key "C-d" 'open-file-directory dired-mode-map) ; d and D is taken
  (bind-key "b" 'shell-here            dired-mode-map) ; mnemonic: b for bash; s and S is taken
  (bind-key "G" 'helm-ag               dired-mode-map)
  (bind-key "C-o" 'helm-swoop          dired-mode-map)
  (bind-key "o" 'dired-display-file    dired-mode-map))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (defun dired-subtree-expand-recursive ()
    ;; Insert dired-subtree at point and recursively in all dirs that show up
    (interactive)
    (dired-subtree-cycle 999))          ; Arbitrary large number
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<C-tab>" #'dired-subtree-expand-recursive))

;; ----------------------------------------------------------------
;; misc.
;; ----------------------------------------------------------------
(use-package avy
  :ensure t)
(use-package mwim
  :ensure t)

;; ================================================================
;; Visuals
;; ================================================================

;; Theme
(load-theme 'wombat t)
;; Improve helm selection face
(set-face-attribute 'helm-selection nil
                    :background "purple4"
                    :foreground "snow")

;; Keep highlighted window at appropriate size
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1)
  (diminish 'golden-ratio-mode))

;; Prevent startup screen
(setq inhibit-startup-screen t)

;; nlinum is a faster linum-mode
(use-package nlinum
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode))

;; Show column number in modeline
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
  "Set 'indent-tabs-mode' intelligently based on the contents of the current buffer."
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the indent-tabs-mode nil
  (setq indent-tabs-mode nil)
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun apply-tab-settings ()
  "Apply custom tab settings."
  (setq-default indent-tabs-mode nil)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq default-tab-width 4)
  (infer-indentation-style))

(defun indent-buffer ()
  "Indent current buffer."
  (when (and (derived-mode-p 'prog-mode) (not (derived-mode-p 'python-mode)))
    (indent-region (point-min) (point-max))))

(setq show-paren-delay 0)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'apply-tab-settings)
(add-hook 'before-save-hook 'indent-buffer)

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (diminish 'aggressive-indent-mode))

;; Delete trailing whitespace on save
(use-package ws-butler
  :ensure t
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; Have emacs respect .editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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
;; yaml-mode
;; ----------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; ----------------------------------------------------------------
;; dockerfile-mode
;; ----------------------------------------------------------------
(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode)
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
  "If a region is active, make this the isearch default search pattern (arguments NOT-REGEXP and NO-RECURSIVE-EDIT are not used)."
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

;; No backup/lock files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Auto-revert image buffers
(add-hook 'image-mode-hook (lambda () (auto-revert-mode) (auto-image-file-mode)))

;; Other
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(auto-revert-mode 1)
(setq kill-buffer-query-functions nil)  ; Do not ask for confirmation when killing process buffers
(setq tags-add-tables nil)              ; Never ask "keep current list of tags table?"
(setq ring-bell-function 'ignore)       ; Disable warning sounds
(setq large-file-warning-threshold (* 200 1000 1000)) ; 200 megabytes

;; ================================================================
;; Verify needed system packages are installed
;; ================================================================

(use-package system-packages
  :ensure t)

(use-package ensure-system-package
  :demand t
  :after (exec-path-from-shell system-packages)
  :config
  (ensure-system-package "python3" "python.exe" "Please download and install it via https://www.python.org/downloads/ .")
  (ensure-system-package "git" "git.exe" "Please download and install it via https://git-scm.com/downloads .")
  (ensure-system-package "sh" "sh.exe" "Please download and install it via https://git-scm.com/downloads .")
  (ensure-system-package "universal-ctags" "ctags.exe"
                         "Please download a binary from https://github.com/universal-ctags/ctags-win32/releases and copy it to somewhere under C:\\Program Files .")
  (ensure-system-package "ripgrep" "rg.exe"
                         "Please download a binary from https://github.com/BurntSushi/ripgrep/releases and copy it to somewhere under C:\\Program Files .")
  (ensure-system-package "imagemagick" "convert.exe" "Please download and install it via https://legacy.imagemagick.org/script/download.php .")
  (ensure-pip-module "python-lsp-server"))

(provide 'init)
;;; init.el ends here
