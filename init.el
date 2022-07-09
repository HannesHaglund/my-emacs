;; ================================================================
;; Set up path
;; ================================================================
(message "Loading init.el...")

(defun relative-path (path)
  "Return <path to file being evaluated>/PATH."
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

;; Also useful :)
(use-package grep-toolbox)

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
;; which-key
;; ----------------------------------------------------------------
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (diminish 'which-key-mode)
  (which-key-setup-side-window-right))

(use-package which-key-posframe
  :ensure t
  :after which-key
  :config
  (which-key-posframe-mode)
  (diminish 'which-key-posframe))

;; ----------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------
(use-package hydra
  :ensure t)

(use-package pretty-hydra
  :ensure t
  :after hydra)

;; ----------------------------------------------------------------
;; vertico
;; ----------------------------------------------------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (diminish 'vertico-mode)
  :config
  (setq vertico-count 24))

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (vertico-posframe-mode 1)
  (diminish 'vertico-posframe-mode))

;; Keeps all history, especially useful for vertico,
;; since it ranks by history
(use-package savehist
  :init
  (savehist-mode)
  (diminish 'savehist-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ----------------------------------------------------------------
;; consult
;; ----------------------------------------------------------------
(use-package consult
  :ensure t
  :config
  ;; Bind live preview to a key for some modes.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "C-j")))

;; ----------------------------------------------------------------
;; marginalia
;; ----------------------------------------------------------------
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  ;; Disable file annotators in project find (I find the permissions distracting)
  (setq marginalia-annotator-registry
        (assq-delete-all 'project-file marginalia-annotator-registry))
  (diminish 'marginalia-mode))

;; ----------------------------------------------------------------
;; embark
;; ----------------------------------------------------------------

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-M-." . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

  (defun mc/add-default-cmds-to-run ()
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-previous-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/skip-to-previous-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/unmark-previous-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-next-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/skip-to-next-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/unmark-next-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/insert-numbers)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/insert-numbers-and-exit)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/insert-letters)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/insert-letters-and-exit)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/edit-lines)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-all-like-this)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/mark-all-in-region-regexp)
    (add-to-list 'mc/cmds-to-run-once 'hydra-multiple-cursors/mc/clear-cmds-to-run)
    (add-to-list 'mc/cmds-to-run-for-all 'mwim-beginning)
    (add-to-list 'mc/cmds-to-run-for-all 'mwim-end)
    (add-to-list 'mc/cmds-to-run-for-all 'er/expand-region))

  (mc/add-default-cmds-to-run)

  (defun mc/clear-cmds-to-run ()
    (interactive)
    (setq mc/cmds-to-run-once nil)
    (setq mc/cmds-to-run-for-all nil)
    (mc/add-default-cmds-to-run))

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
  :bind (:map lsp-signature-mode-map ("M-n" . nil))

  :hook ((python-mode . lsp))
  :commands lsp)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold (* 10 1000))
  (when (eq system-type 'windows-nt)
    ;; Fixes a bug where the linters are not found on Windows
    (setq flycheck-python-pylint-executable "pylint")
    (setq flycheck-python-flake8-executable "flake8")))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package dap-mode
  :ensure t
  :after lsp-mode)

;; ----------------------------------------------------------------
;; basic-keybinds
;; ----------------------------------------------------------------
(use-package joined-mark-ring-navigation
  :demand t                             ; We want this to load regardless of binds
  :bind
  ("M-,"   .   pop-joined-mark-ring)
  ("C-M-," . unpop-joined-mark-ring)
  :config
  (advice-add 'xref-push-marker-stack :before 'joined-mark-ring-push-point))

(use-package avy
  :ensure t)

(use-package mwim
  :ensure t)

(use-package basic-keybinds
  :after (pretty-hydra consult grep-toolbox avy mwim)
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end)
  ("C-k" . kill-whole-line)
  ("M-p" . scroll-up-bind)
  ("M-n" . scroll-down-bind)
  ("C-," . other-window)
  ("C-;" . other-frame)
  ("M-g" . goto-line)
  ("C-o" . grep-toolbox-consult-line)
  ("C-z" . undo)
  ("C-x K" . kill-buffer-and-window)
  ("C-c C-f" . revert-buffer-no-confirm))

;; ----------------------------------------------------------------
;; shell-repl
;; ----------------------------------------------------------------
(use-package shell-repl
  :commands shell-repl
  :bind ("M-!" . shell-repl))

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
;; projectile
;; ----------------------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (diminish 'projectile-mode))

;; ----------------------------------------------------------------
;; avy
;; ----------------------------------------------------------------
(use-package avy
  :ensure t
  :bind ("C-v" . avy-goto-char-timer))

;; ----------------------------------------------------------------
;; good-scroll
;; ----------------------------------------------------------------

;; Enable pixel scrolling that works better than the built-in one
(use-package good-scroll
  :ensure t
  :config
  (good-scroll-mode 1))

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
  :commands (p4-add p4-edit p4-move p4-delete p4-revert p4-opened p4-status p4-annotate
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
  :after consult
  :config
  (setq dired-listing-switches "-alh")    ; List file sizes in a human-readable format
  (defun dired-here () (interactive) (dired default-directory))
  (bind-key "C-x d" 'dired-here)
  (bind-key "C-d" 'open-file-directory dired-mode-map) ; d and D is taken
  (bind-key "b" 'shell-here            dired-mode-map) ; mnemonic: b for bash; s and S is taken
  (bind-key "i" 'dired-display-file    dired-mode-map)
  (bind-key "o" 'grep-toolbox-consult-line dired-mode-map)
  (bind-key "G" 'grep-toolbox-consult-ripgrep-default-directory dired-mode-map))

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

(use-package all-the-icons-dired
  :ensure t
  :after (dired all-the-icons)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (diminish 'all-the-icons-dired-mode))

;; ----------------------------------------------------------------
;; context-menu
;; ----------------------------------------------------------------
(use-package mouse
  :demand t
  :bind ("<mouse-3>" . context-menu-open)
  :config
  (setq context-menu-functions
        '(context-menu-ffap
          context-menu-region
          context-menu-undo
          context-menu-buffers
          context-menu-local
          context-menu-minor))
  (context-menu-mode +1))

;; ----------------------------------------------------------------
;; project
;; ----------------------------------------------------------------
(use-package project
  :demand t
  :bind ("C-x p p" . project-find-project-then-find)
  :bind ("C-x p g" . consult-ripgrep-inherit-region)
  :init
  (defun project-find-project-then-find (dir)
    "\"Switch\" to another project by running an Emacs command.
When called in a program, it will use the project corresponding to directory DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively 'project-find-file))))

;; ----------------------------------------------------------------
;; git bash shell setup
;; ----------------------------------------------------------------

(defun file-has-str-p (filepath str)
  "Return t if STR exists in file on FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (search-forward str nil t)))

;; Set up git bash terminal on windows systems
(when (eq system-type 'windows-nt)

  (setq shell-file-name "c:/Program Files/Git/bin/bash.exe")

  (unless (boundp 'explicit-shell-file-name)
    ;; Somewhat sane default
    (setq explicit-shell-file-name shell-file-name))

  (unless (file-exists-p explicit-shell-file-name)
    (display-warning :warning
                     (concat "explicit-shell-file-name points to non-existant file. "
                             "It is recommended to point it to bash.exe "
                             "inside your git for windows installation.")))

  (let ((new-ps1-l "if [ -n \"$INSIDE_EMACS\" ]; then export PS1='\\[\\033[32m\\]\\u@\\h \\[\\033[33m\\]\\w\\[\\033[36m\\]`__git_ps1`\\[\\033[0m\\]\\n$ '; fi"))
    (setq explicit-bash.exe-args '("--login" "-i")) ; Is this needed?
    (setq explicit-bash-args '("--login" "-i"))
    (prefer-coding-system 'utf-8)

    ;; Set up bash_profile that avoids a bunch of junk in shell output
    (unless (file-exists-p "~/.bash_profile")
      ;; Create empty file
      (write-region "" nil "~/.bash_profile"))
    ;; Append command to it unless it's already in file

    (unless (file-has-str-p "~/.bash_profile" new-ps1-l)
      (write-region (concat "\n" new-ps1-l) nil "~/.bash_profile" 'append))))


;; ================================================================
;; Visuals
;; ================================================================

;; Keep highlighted window at appropriate size
(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618))     ; Golden ratio
  (zoom-mode t)
  (diminish 'zoom-mode))

;; Apply hack font
(when (font-available-p "hack") (set-frame-font "hack"))

;; Prevent startup screen
(setq inhibit-startup-screen t)

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

;; Show paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Relative line numbers
(add-hook 'prog-mode-hook '(lambda ()
                             (display-line-numbers-mode 1)
                             (setq display-line-numbers 'relative)))

;; Nyan mode to highlight buffer scroll
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1)
  (diminish 'nyan-mode))

;; ----------------------------------------------------------------
;; Themes
;; ----------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (setq doom-vibrant-brighter-comments t)
  (setq doom-vibrant-brighter-modeline t)
  (load-theme 'doom-vibrant t))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (diminish 'doom-modeline-mode))

;; ----------------------------------------------------------------
;; All-the-icons
;; ----------------------------------------------------------------
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;; ================================================================
;; Languages
;; ================================================================
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
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (diminish 'ws-butler-mode))

;; Have emacs respect .editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  (diminish 'editorconfig-mode))

;; ----------------------------------------------------------------
;; c-mode
;; ----------------------------------------------------------------
(setq c-default-style "linux")

;; ----------------------------------------------------------------
;; octave-mode
;; ----------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; Src: https://emacs.stackexchange.com/questions/15164/commented-lines-shoot-off-to-column-32-in-octave-mode
;; (slightly modified)
(setq octave-mode-hook
      (lambda () (progn (setq octave-comment-char ?%)
                        (setq comment-start "%")
                        (setq comment-add 0)
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
(setq kill-buffer-query-functions nil)  ; Do not ask for confirmation when killing process buffers
(setq tags-add-tables nil)              ; Never ask "keep current list of tags table?"
(setq large-file-warning-threshold (* 200 1000 1000)) ; 200 megabytes
(diminish 'eldoc-mode)

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
  (ensure-pip-module "python-lsp-server==0.19.0")
  (ensure-font-installed "hack" "Download it from https://github.com/source-foundry/Hack and install it.")
  ;; all-the-icons fonts
  (let ((all-the-icons-msg "Run M-x all-the-icons-install-fonts, and install the fonts downloaded."))
    (ensure-font-installed "all-the-icons" all-the-icons-msg)
    (ensure-font-installed "file-icons" all-the-icons-msg)
    (ensure-font-installed "fontawesome" all-the-icons-msg)
    (ensure-font-installed "Material Icons" all-the-icons-msg)
    (ensure-font-installed "github-octicons" all-the-icons-msg)
    (ensure-font-installed "Weather Icons Regular" all-the-icons-msg)))


(provide 'init)
;;; init.el ends here
