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
;; vertico
;; ----------------------------------------------------------------
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-count 24))

;; Keeps all history, especially useful for vertico,
;; since it ranks by history
(use-package savehist
  :init
  (savehist-mode))

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
  :demand t
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
        (assq-delete-all 'project-file marginalia-annotator-registry)))

;; ----------------------------------------------------------------
;; default-text-scale
;; ----------------------------------------------------------------
(use-package default-text-scale
  :ensure t
  :bind
  ("C-c z i" . default-text-scale-increase)
  ("C-c z o" . default-text-scale-decrease)
  :config
  (setq default-text-scale-amount 40))

;; ----------------------------------------------------------------
;; multiple-cursors
;; ----------------------------------------------------------------
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-M-n" . mc/mark-next-like-this)
  ("C-M-p" . mc/mark-previous-like-this)
  ("C-M-f" . mc/skip-to-next-like-this)
  ("C-M-b" . mc/skip-to-previous-like-this)
  ("C-M-a" . mc/mark-all-like-this)

  :config
  (defun mc/add-default-cmds-to-run ()
    (add-to-list 'mc/cmds-to-run-for-all 'org-delete-char)
    (add-to-list 'mc/cmds-to-run-for-all 'mwim-beginning)
    (add-to-list 'mc/cmds-to-run-for-all 'mwim-end)
    (add-to-list 'mc/cmds-to-run-for-all 'er/expand-region))

  (mc/add-default-cmds-to-run)

  (defun mc/clear-cmds-to-run ()
    (interactive)
    (setq mc/cmds-to-run-once nil)
    (setq mc/cmds-to-run-for-all nil)
    (mc/add-default-cmds-to-run)))

;; ----------------------------------------------------------------
;; company-mode
;; ----------------------------------------------------------------
(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  (org-mode . company-mode)
  (markdown-mode . company-mode)
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
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.05)
  (setq company-eclim-auto-save nil))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-fuzzy
  :ensure t
  :after company
  :config
  (global-company-fuzzy-mode 1))

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
;; basic-keybinds
;; ----------------------------------------------------------------
(use-package xref
  :bind
  ("M-."   .   xref-find-definitions)
  ("M-?"   .   xref-find-references)
  :config
  (defun my-emacs-xref-push-marker-stack (&optional _)
    (xref-push-marker-stack (copy-marker (point-marker))))
  (advice-add 'consult--jump :before 'my-emacs-xref-push-marker-stack))

(use-package mwim
  :ensure t)

(use-package emacs
  :after useful-commands
  :demand t
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end)
  ("C-k" . kill-whole-line)
  ("M-p" . scroll-up-bind)
  ("M-n" . scroll-down-bind)
  ("C-," . other-window)
  ("C-;" . other-frame)
  ("M-g" . goto-line)
  ("C-o" . consult-line-inherit-region)
  ("C-z" . repeat)
  ("C-x C-b" . switch-to-buffer)
  ("C-x K" . kill-buffer-and-window)
  ("C-c C-f" . revert-buffer-no-confirm))

;; ----------------------------------------------------------------
;; shell-repl
;; ----------------------------------------------------------------
(use-package shell-repl
  :commands shell-repl
  :bind (("M-!" . shell-repl)
         ("M-~" . shell-repl-store)
         ("M-`" . shell-repl-run-stored)
         :map dired-mode-map
         ("M-!" . shell-repl)))

;; ----------------------------------------------------------------
;; whitespace
;; ----------------------------------------------------------------
(use-package whitespace
  :config
  (setq whitespace-style '(face tabs lines-tail))
  (set-face-foreground 'whitespace-line nil)
  (setq whitespace-line-column 120))

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
;; persistent-scratch
;; ----------------------------------------------------------------
(use-package persistent-scratch
  :ensure t
  :demand t
  :init
  (persistent-scratch-setup-default))

;; ----------------------------------------------------------------
;; back-button
;; ----------------------------------------------------------------
(use-package back-button
  :ensure t
  :demand t
  :after consult
  :bind
  ("M-,"   . back-button-global)
  ("C-M-," . back-button-global-forward)

  :config

  (defun back-button-push-mark-local-and-global-unless-from-back-button ()
    (unless (or (eq this-command 'back-button-global)
                (eq this-command 'back-button-global-forward))
      (back-button-push-mark-local-and-global)))

  (setq back-button-no-wrap t)
  (setq back-button-index-timeout 8)    ; seconds

  (advice-add 'consult--jump           :before 'back-button-push-mark-local-and-global)
  (advice-add 'consult--jump           :after  'back-button-push-mark-local-and-global)
  ;; (advice-add 'pop-to-buffer-same-window           :before 'back-button-push-mark-local-and-global)
  ;; (advice-add 'pop-to-buffer-same-window           :after 'back-button-push-mark-local-and-global)
  (advice-add 'find-generic-definition :before 'back-button-push-mark-local-and-global)
  (advice-add 'find-generic-definition :after  'back-button-push-mark-local-and-global)

  (back-button-mode 1))

;; ----------------------------------------------------------------
;; dumb-jump
;; ----------------------------------------------------------------
(use-package dumb-jump
  :ensure t
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg))

;; ----------------------------------------------------------------
;; version control
;; ----------------------------------------------------------------

;; Display whether a line is changed in VC on fringe
(use-package diff-hl
  :ensure t
  :demand t
  :config
  (global-diff-hl-mode))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

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
  :ensure t
  :config
  (setq magit-refresh-verbose t)
  ;; Magit is slow on windows. Remove sections to speed it up.
  (when (eq system-type 'windows-nt)
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
    (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)))

(advice-add 'diff-apply-hunk :after #'(lambda () (interactive) (save-some-buffers t nil)))
(bind-key "k" 'diff-apply-hunk diff-mode-shared-map)

;; ----------------------------------------------------------------
;; Timeclock
;; ----------------------------------------------------------------
(use-package timeclock
  :after useful-commands
  :bind
  ("C-c c i" . timeclock-in)
  ("C-c c o" . timeclock-out-no-reason)
  ("C-c c O" . timeclock-out)
  ("C-c c s" . timeclock-change)
  ("C-c c f" . timeclock-visit-timelog)
  ("C-c c c" . timeclock-reread-and-status-string)
  ("C-c c d" . timeclock-project-summary)
  :config
  )

;; ----------------------------------------------------------------
;; Macro
;; ----------------------------------------------------------------
(use-package macro
  :bind
  ("C-c o a" . kmacro-start-macro)
  ("C-c o e" . kmacro-end-or-call-macro-repeat)
  ("C-c o x s" . insert-kbd-macro))

;; ----------------------------------------------------------------
;; dired-mode
;; ----------------------------------------------------------------
(use-package dired
  :demand t
  :config
  (setq dired-listing-switches "-alh")    ; List file sizes in a human-readable format
  (defun dired-here () (interactive) (dired default-directory))
  (bind-key "C-x d" 'dired-here)
  (bind-key "C-d" 'open-file-directory dired-mode-map) ; d and D is taken
  (bind-key "b" 'shell-here            dired-mode-map) ; mnemonic: b for bash; s and S is taken
  (bind-key "i" 'dired-display-file    dired-mode-map)
  (bind-key "o" 'consult-line-inherit-region dired-mode-map)
  (bind-key "G" 'consult-ripgrep-default-directory dired-mode-map))

(use-package dired-subtree
  :ensure t
  :demand t
  :after dired
  :config
  (defun dired-subtree-expand-recursive ()
    ;; Insert dired-subtree at point and recursively in all dirs that show up
    (interactive)
    (dired-subtree-cycle 999))          ; Arbitrary large number
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<M-tab>" #'dired-subtree-expand-recursive))

;; ----------------------------------------------------------------
;; context-menu
;; ----------------------------------------------------------------
(use-package mouse
  :bind ("<mouse-3>" . context-menu-open)
  :init
  (setq context-menu-functions
        '(context-menu-ffap
          context-menu-region
          context-menu-undo
          context-menu-buffers
          context-menu-local
          context-menu-minor))
  :config
  (context-menu-mode +1))

;; ----------------------------------------------------------------
;; project
;; ----------------------------------------------------------------
(use-package project
  :demand t
  :bind
  ("C-x p g" . consult-ripgrep-inherit-region))

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

  (setq explicit-bash.exe-args '(""))
  (setq explicit-bash-args     '(""))

  (prefer-coding-system 'utf-8)

  (setenv "PAGER"  "cat")
  (setenv "EDITOR" "emacs")

  (defface my-emacs-shell-plus-face
    `((((class color) (background light))
       ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "#ddffdd"
       :foreground "#22aa22")
      (((class color) (background dark))
       ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "#335533"
       :foreground "#ddffdd"))
    "Face for lines that starts with plus in shell-mode."
    :group 'magit-faces)

  (defface my-emacs-shell-minus-face
    `((((class color) (background light))
       ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "#ffdddd"
       :foreground "#aa2222")
      (((class color) (background dark))
       ,@(and (>= emacs-major-version 27) '(:extend t))
       :background "#553333"
       :foreground "#ffdddd"))
    "Face for lines that starts with minus in shell-mode.")

  (font-lock-add-keywords 'shell-mode '(("^\\+.*" . 'my-emacs-shell-plus-face)
                                        ("^-.*"   . 'my-emacs-shell-minus-face)))

  (defun windows-shell-advice (&optional a b c)
    (when (get-buffer "*shell*")
      (with-current-buffer "*shell*"
        (goto-char (point-max))
        (when (< (point) 16)          ; When the buffer is very short, i.e newly created

          (insert "export PS1=\"\\\\n┃ \\\\w ➤ \"")
          (comint-send-input)

          (insert "export EDITOR='/c/windows/system32/notepad.exe'")
          (comint-send-input)

          (insert "alias gitlog='git log --pretty=format:\"%h%x09%an%x09%ad%x09%s\" --date=short --reverse | tail -48'")
          (comint-send-input)

          (insert "alias gitbs='git for-each-ref --format=\" %(author) %09 %(authordate:short)   %(objectname:short)   (%(committerdate:relative)) %09 %(refname:short) \" --sort=authordate | grep -i \"hannes haglund\" | cut -f2-'")
          (comint-send-input)
          ))))

  (advice-add 'shell :after 'windows-shell-advice))


;; ================================================================
;; Visuals
;; ================================================================

;; Highlight current line
(global-hl-line-mode 1)

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

;; Scroll bar
(scroll-bar-mode 1)

;; Show paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; ----------------------------------------------------------------
;; Modeline
;; ----------------------------------------------------------------
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   "%*%*%* "
   mode-line-buffer-identification
   "| "
   "%l⬍"
   (:eval (number-to-string (count-lines (point-min) (point-max))))
   "  "
   (:eval (propertize
           (file-name-nondirectory (directory-file-name (file-name-directory (project-root (project-current)))))
           'face 'mode-line-emphasis))
   "/" ((:eval (string-trim-left default-directory (project-root (project-current)))))
   "   ("
   mode-name
   (:eval (when (bound-and-true-p defining-kbd-macro) " kmacro"))
   (:eval (when (bound-and-true-p multiple-cursors-mode)
            (format " mc[%d]" (mc/num-cursors))))
   (:eval (when (bound-and-true-p buffer-read-only) " readonly"))
   (:eval (when (bound-and-true-p debug-on-error) " dbg"))
   (:eval (when (and (fboundp 'timeclock-currently-in-p) (timeclock-currently-in-p))
            (format " t[%s]" (timeclock-when-to-leave-string nil t))))
   ") "
   mode-line-misc-info mode-line-end-spaces))

;; Update all modelines when time changes
(advice-add 'timeclock-in  :after 'timeclock-reread-log)
(advice-add 'timeclock-reread-log :after #'(lambda () (force-mode-line-update t)))

;; ================================================================
;; Languages
;; ================================================================
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'gdscript-mode))

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
;; org-mode
;; ----------------------------------------------------------------
(use-package org
  :mode (("\\.org$" . org-mode))
  :config

  ;; Adjust org heading face attributes
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  (setq org-ellipsis " ◢")
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-link-file-path-type 'relative)

  (setq org-todo-keywords '((sequence "TODO(!)" "WAITING(!)" "|" "DONE(!)")))
  (setq org-todo-keyword-faces '(("WAITING" . warning)))
  (setq org-agenda-skip-deadline-if-done t)

  (bind-key "C-," nil org-mode-map)
  (bind-key "C-j" nil org-mode-map)

  (defun org-fold-done ()
    "Fold top-level headings in current buffer marked DONE."
    (interactive)
    (org-fold-show-all '(headings))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (s-starts-with-p "DONE" (org-get-heading))
          (org-cycle))
        (org-next-visible-heading 1))))

  (defun org-sort-todo-cur-buffer ()
    "Sort entries in todo order in current buffer and go to point-min."
    (interactive)
    (save-mark-and-excursion
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (org-sort-entries nil ?o))
    ;; save-mark-and-excursion seems to still mess up point. Guess I can live with that.
    (goto-char (point-min)))

  ;; Works on find-file, but bugs on revert-buffer. Don't know why, but good enough I guess.
  (defun org-try-todoorg-actions ()
    "Actions on entering todo.org."
    (when (string-equal (buffer-name (current-buffer)) "todo.org")
      (org-sort-todo-cur-buffer)
      (org-fold-done)
      (goto-char (point-min))))

  (add-hook 'find-file-hook #'org-try-todoorg-actions))

(use-package org-bullets
  :after org
  :ensure t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-chef
  :after org
  :ensure t
  :commands (org-chef-insert-recipe org-chef-edit-servings))

;; ----------------------------------------------------------------
;; markdown-mode
;; ----------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map ("C-M-u" . markdown-do))
  :config

  ;; Disbale some keys from overriding global map
  (bind-key "M-n" nil markdown-mode-map)
  (bind-key "M-p" nil markdown-mode-map)

  ;; Misc settings
  (setq markdown-fontify-whole-heading-line t)
  (setq markdown-fontify-code-blocks-natively t)

  ;; Disable indent
  (setq markdown-indent-function #'(lambda () (interactive)))
  (setq markdown-indent-on-enter nil))


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
;; gdscript-mode
;; ----------------------------------------------------------------
(use-package gdscript-mode
  :ensure t
  :commands (gdscript-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-mode)))

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

;; Show num of matches in isearch
(setq isearch-lazy-count t)

;; Dabbrev settings
(setq dabbrev-case-fold-search nil)
(setq dabbrev-upcase-means-case-search t)

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
  (ensure-font-installed "hack" "Download it from https://github.com/source-foundry/Hack and install it."))


(provide 'init)
;;; init.el ends here
