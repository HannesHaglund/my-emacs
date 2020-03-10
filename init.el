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

;; ----------------------------------------------------------------
;; hydra
;; ----------------------------------------------------------------
(ensure-package-installed 'hydra)

;; ----------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------
(ensure-package-installed 'helm)
(helm-mode t)
(setq helm-buffer-max-length nil)

;; ----------------------------------------------------------------
;; helm-xref
;; ----------------------------------------------------------------
(ensure-package-installed 'helm-xref)
(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

;; ----------------------------------------------------------------
;; helm-ag
;; ----------------------------------------------------------------
(ensure-package-installed 'helm-ag)

(setq helm-ag-use-temp-buffer t)
(setq helm-ag-insert-at-point t)
(setq helm-ag-fuzzy-match t)

;; Note: Requires the user to manually install rg
(setq helm-ag-base-command "rg --no-heading")
(when (eq system-type 'windows-nt)
  (setq helm-ag-base-command "rg --no-heading --vimgrep")
  ;; Note: On Windows, you will need to add the ripgrep executable to this path
  (add-to-list 'exec-path "C:\\Program Files\\ripgrep"))

;; ----------------------------------------------------------------
;; helm-projectile
;; ----------------------------------------------------------------
(ensure-package-installed 'projectile)
(projectile-mode +1)
(projectile-global-mode)
(require 'subr-x) ; Tags generation from projectile crashes otherwise
(setq projectile-use-git-grep t)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(add-to-list 'projectile-globally-ignored-directories "Build")

(ensure-package-installed 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
  _f_: file            _a_: ag                _i_: Ibuffer           _c_: clear cache
_s-f_: file dwim       _g_: git grep          _b_: switch to buffer  _x_: remove known project
  _r_: recent file     _t_: update gtags    _s-k_: kill all buffers  _X_: cleanup non-existing
  _d_: dir             _o_: multi-occur                          ^^^^_z_: cache current
"
  ("a"   helm-do-ag-project-root)
  ("g"   helm-projectile-grep)
  ("b"   helm-projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("f"   helm-projectile-find-file)
  ("s-f" helm-projectile-find-file-dwim)
  ("d"   helm-projectile-find-dir)
  ("t"   ggtags-update-tags)
  ("s-t" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" helm-projectile-switch-project "switch project")
  ("p"   helm-projectile-switch-project)
  ("s"   helm-projectile-switch-project)
  ("r"   helm-projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

;; ----------------------------------------------------------------
;; multiple-cursors
;; ----------------------------------------------------------------
(ensure-package-installed 'multiple-cursors)
(require 'multiple-cursors)

(defun mc/clear-cmds-to-run ()
  (interactive)
  (setq mc/cmds-to-run-once nil)
  (setq mc/cmds-to-run-for-all nil))

(defhydra hydra-multiple-cursors (:hint nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_c_] Clear commands
 [Click] Cursor at point       [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("c" mc/clear-cmds-to-run)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))

;; ----------------------------------------------------------------
;; restart-emacs
;; ----------------------------------------------------------------
(ensure-package-installed 'restart-emacs)
(setq restart-emacs-restore-frames nil)

(defun restart-emacs-keep-frames ()
  "Same as (restart-emacs) but restore the frame to current context"
  (interactive)
  (setq restart-emacs-restore-frames t)
  (restart-emacs)
  (setq restart-emacs-restore-frames nil))

;; ----------------------------------------------------------------
;; company-mode
;; ----------------------------------------------------------------
(ensure-package-installed 'company)
(ensure-package-installed 'company-fuzzy)
(require 'company)

(define-key company-active-map (kbd "SPC")                 nil)
(define-key company-active-map (kbd "<return>")            nil)
(define-key company-active-map (kbd "RET")                 nil)
(define-key company-active-map (kbd "<tab>")              'company-complete-selection)
(define-key company-active-map (read-kbd-macro "<C-tab>") 'company-select-next)
(define-key company-active-map (read-kbd-macro "<S-tab>") 'company-select-previous)
(define-key company-search-map (read-kbd-macro "<C-tab>") 'company-select-next)
(define-key company-search-map (read-kbd-macro "<S-tab>") 'company-select-previous)

(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.1)
(setq company-eclim-auto-save nil)

(global-company-fuzzy-mode 1)
(setq company-fuzzy-prefix-ontop t)

(add-hook 'after-init-hook 'global-company-mode)

;; ----------------------------------------------------------------
;; misc.
;; ----------------------------------------------------------------
(ensure-package-installed 'magit)
(ensure-package-installed 'expand-region)
(ensure-package-installed 'mwim)
(ensure-package-installed 'goto-chg)

;; ================================================================
;; Key binds
;; ================================================================

(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (scroll-up-line 10))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (scroll-down-line 10))

(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (next-line))

(setq beginning-of-code-line-or-bugger-original-point nil)
(setq beginning-of-code-line-or-buffer-times-pressed 0)
(defun beginning-of-code-line-or-buffer ()
  "Move point to beginning of code, line or buffer depending on times pressed."
  (interactive)
  (when (not (eq last-command this-command))
    (setq beginning-of-code-line-or-bugger-original-point nil)
    (setq beginning-of-code-line-or-buffer-times-pressed 0))
  (when beginning-of-code-line-or-bugger-original-point
    (goto-char beginning-of-code-line-or-bugger-original-point)
    (setq beginning-of-code-line-or-bugger-original-point nil))
  (setq beginning-of-code-line-or-buffer-times-pressed
        (+ beginning-of-code-line-or-buffer-times-pressed 1))
  (if (eq beginning-of-code-line-or-buffer-times-pressed 1)
      (beginning-of-line-text)
    (if (eq beginning-of-code-line-or-buffer-times-pressed 2)
        (beginning-of-line)
      (when (eq beginning-of-code-line-or-buffer-times-pressed 3)
        (setq beginning-of-code-line-or-bugger-original-point (point))
        (goto-char (point-min))
        (setq beginning-of-code-line-or-buffer-times-pressed 0)))))

(setq end-of-code-line-or-bugger-original-point nil)
(setq end-of-code-line-or-buffer-times-pressed 0)
(defun end-of-code-line-or-buffer ()
  "Move point to end of code, line or buffer depending on times pressed."
  (interactive)
  (when (not (eq last-command this-command))
    (setq end-of-code-line-or-bugger-original-point nil)
    (setq end-of-code-line-or-buffer-times-pressed 0))
  (when end-of-code-line-or-bugger-original-point
    (goto-char end-of-code-line-or-bugger-original-point)
    (setq end-of-code-line-or-bugger-original-point nil))
  (setq end-of-code-line-or-buffer-times-pressed
        (+ end-of-code-line-or-buffer-times-pressed 1))
  (if (eq end-of-code-line-or-buffer-times-pressed 1)
      (mwim-end)
    (if (eq end-of-code-line-or-buffer-times-pressed 2)
        (end-of-line)
      (when (eq end-of-code-line-or-buffer-times-pressed 3)
        (setq end-of-code-line-or-bugger-original-point (point))
        (goto-char (point-max))
        (setq end-of-code-line-or-buffer-times-pressed 0)))))

(defun align-each (regexp)
  (interactive "sRegexp: ")
  (align-regexp
   (if (use-region-p) (region-beginning) (point-min))
   (if (use-region-p) (region-end)       (point-max))
   (concat "\\(\\s-*\\)" regexp)
   1
   align-default-spacing
   1))

(defun helm-do-ag-this-saved-file ()
  (interactive)
  (save-buffer)
  (helm-do-ag-this-file))

;; src: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

    ;; Common
    (define-key map (kbd "C-a")         'beginning-of-code-line-or-buffer)
    (define-key map (kbd "C-e")         'end-of-code-line-or-buffer)
    (define-key map (kbd "M-p")         'scroll-up-bind)
    (define-key map (kbd "M-n")         'scroll-down-bind)
    (define-key map (kbd "C-,")         'other-window)
    (define-key map (kbd "C-.")         'wind-bck)
    (define-key map (kbd "C-;")         'other-frame)
    (define-key map (kbd "M-g")         'goto-line)
    (define-key map (kbd "C-j")         'er/expand-region)
    (define-key map (kbd "C-c c")       'goto-last-change)

    ;; Align
    (define-key map (kbd "C-c a a")     'align)
    (define-key map (kbd "C-c a r")     'align-regexp)
    (define-key map (kbd "C-c a e")     'align-each)

    ;; Company
    (define-key map (kbd "C-/")         'company-manual-begin)

    ;; Helm
    (define-key map (kbd "M-x")         'helm-M-x)
    (define-key map (kbd "C-x b")       'helm-buffers-list)
    (define-key map (kbd "C-x C-b")     'helm-buffers-list)
    (define-key map (kbd "C-x C-f")     'helm-find-files)
    (define-key map (kbd "C-c C-y")     'helm-show-kill-ring)
    (define-key map (kbd "C-h h")       'helm-apropos)
    (define-key map (kbd "C-h f")       'helm-apropos)
    (define-key map (kbd "C-h v")       'helm-apropos)

    ;; Helm Swoop
    (define-key map (kbd "M-I")         'helm-ag-pop-stack)
    (define-key map (kbd "M-i")         'helm-do-ag-this-saved-file)
    (define-key map (kbd "C-c M-i")     'helm-do-ag-buffers)
    (define-key map (kbd "C-c p s s")   'helm-do-ag-project-root)

    ;; Multiple cursors
    (define-key map (kbd "C-c m")       'hydra-multiple-cursors/body)

    ;; Projectile
    (define-key map (kbd "C-c p")       'hydra-projectile/body)

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
(setq whitespace-line-column 120)
(global-whitespace-mode t)


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
  (setq default-tab-width 4))

(apply-tab-settings)
(setq show-paren-delay 0)
(show-paren-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'infer-indentation-style)

;; ----------------------------------------------------------------
;; c-mode
;; ----------------------------------------------------------------
(setq c-default-style "linux"
      c-basic-offset 3)

;; ----------------------------------------------------------------
;; Wurst mode
;; ----------------------------------------------------------------
(require 'wurstscript-mode)

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


;; ================================================================
;; Useful interactive functions
;; ================================================================

(defun shell-command-to-kill-ring (cmd)
  (interactive "sShell command: ")
  (let ((output (shell-command-to-string cmd)))
    (message (concat "Shell output:\n" output))
    (kill-new output)))

(defun overwrite-emacs-d (repo-dir)
  "Overwrite contents in ~/.emacs.d/ with the contents of the my-emacs repository."
  (interactive (list (read-directory-name "my-emacs directory: "
                                          ;; folder of init.el buffer if it exists, or nil
                                          (let ((init-el-buffer (get-buffer "init.el")))
                                            (if init-el-buffer
                                                (file-name-directory (buffer-file-name init-el-buffer))
                                              nil)))))
  (let ((emacs-d "~/.emacs.d")
        (init-el "~/.emacs.d/init.el")
        (elisp "~/.emacs.d/elisp"))
    (when (file-directory-p elisp)   (delete-directory elisp t t))
    (when (file-exists-p    init-el) (delete-file      init-el))
    (copy-directory (expand-file-name "elisp"   repo-dir) elisp t t nil)
    (copy-file      (expand-file-name "init.el" repo-dir) init-el t t t t)
    (message (concat "Wrote " repo-dir " to " emacs-d))))

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

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; source: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))) ;;

;; source: https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; source: https://stackoverflow.com/questions/3417438/close-all-buffers-besides-the-current-one-in-emacs
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))


;; ================================================================
;; Misc.
;; ================================================================

;; Dabbrev settings
(setq dabbrev-case-fold-search nil)
(setq dabbrev-upcase-means-case-search t)

;; Disable mouse
(require 'mye-no-mouse)
(disable-mouse-mode 1)

;; Other
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ring-bell-function 'ignore)                     ; Disable warning sounds
(setq large-file-warning-threshold (* 200 1000 1000)) ; 200 megabytes
