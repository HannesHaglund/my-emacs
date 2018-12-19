
;; ================================================================
;; Syntax
;; ================================================================

(defconst wurstscript-syntax-table
  (let ((table (make-syntax-table)))
    ;; " is string delimiter
    (modify-syntax-entry ?\" "\"" table)
    ;; ' is not technically a string delimiter, but close enough
    (modify-syntax-entry ?' "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(setq wurstscript-font-lock-keywords
      (let* (
             (x-keywords '("package" "import" "break" "continue" "class" "extends"
                           "public" "private" "protected" "constant" "function" "override"
                           "returns" "return" "switch" "case" "default" "for" "init"
                           "construct" "destroy" "new" "while" "begin" "array" "skip"
                           "enum" "abstract" "interface" "implements" "module" "static"
                           "tuple" "castTo" "instanceof" "this" "initlater"
                           "ondestroy" "not" "and" "or" "mod" "div" "else"
                           "end" "let" "var" "if"
                           "step" "from" "downto" "to" "in"))
             (x-types '("bool" "char" "integer" "int" "real" "string"
                        "angle" "vec2" "vec3" "thistype"
                        "boolexpr" "camera" "destructable" "effect" "force"
                        "group" "hashtable" "image" "item" "multiboard"
                        "player" "rect" "region" "sound" "texttag" "timer"
                        "trackable" "trigger" "unit" "unittype" "widget"))
             (x-constants '("false" "true" "null"))
             (x-events '(""))
             (x-functions '("super"))

        ;; generate regex string for each category of keywords
        (x-types-regexp (regexp-opt x-types 'words))
        (x-keywords-regexp (regexp-opt x-keywords 'words))
        (x-events-regexp (regexp-opt x-events 'words))
        (x-functions-regexp (regexp-opt x-functions 'words))
        (x-constants-regexp (regexp-opt x-constants 'words))
        (x-warnings-regexp (concat
                            "\\("
                            ;; elseif and elif is not legal syntax. Easy to forget.
                            "elseif\\|elif"
                            "\\|"
                            ;; vec2 should always be used instead
                            "location"
                            "\\|"
                            ;; Matches UpperCamelCase followed by (
                            ;; For showing warnings on JASS function calls
                            ;; Sadly catches class construction to, but no good way around it
                            "\\(^\\|[ 	,(]\\)\\(\\([A-Z]\\([a-z]\\)*\\)+\\)("
                            "\\)")))
      `(
        (,x-types-regexp . font-lock-type-face)
        (,x-constants-regexp . font-lock-constant-face)
        (,x-events-regexp . font-lock-builtin-face)
        (,x-functions-regexp . font-lock-function-name-face)
        (,x-keywords-regexp . font-lock-keyword-face)
        (,x-warnings-regexp . font-lock-warning-face)
        )))

;; ================================================================
;; Indentation
;; ================================================================

(defun wurstscriptp-additional-number-of-indents ()
  ;; Are we on the first line in the file? If so, return false
  (if (= (line-number-at-pos) 1) 0

    ;; Otherwise...
    (save-excursion
      (previous-line)
      (beginning-of-line)
      ;; Am I looking at a line with either just whitespace or line continutation
      ;; possibly ending with a comment?
      (if (looking-at (concat
                       ;; whitespace....comment.............ending.
                       "\\(^[[:space:]]*\\(//.*\\|/\\*.*\\)?$\\)\\|"
                       ;; whitespace....cont....*.comment.............ending.
                       "\\(^[[:space:]]*[.,+-/%].*\\(//.*\\|/\\*.*\\)?$\\)\\|"
                       ;; whitespace....*.cont....comment............ending
                       "\\(^[[:space:]]*.*[,+-/%(]\\(//.*\\|/\\*.*\\)?$\\)"))

          ;; If so, check the previous line instead
          (wurstscriptp-additional-number-of-indents)
        ;; Otherwise, check if this line is syntactically relevaant
        (if (looking-at (concat
                         ;; Class declarations
                         "\\(^[[:space:]]*\\(public \\|private \\)?class\\)\\|"
                         ;; Enum declarations
                         "\\(^[[:space:]]*\\(public \\|private \\)?enum\\)\\|"
                         ;; Function declarations
                         "\\(^[[:space:]]*\\"
                         "(public \\|private \\|"
                         "protected \\|override \\|static \\)*"
                         "function [[:word:]]*(.*)\\)\\|"
                         ;; If
                         "\\(^[[:space:]]*if \\)\\|"
                         ;; Else
                         "\\(^[[:space:]]*else \\)\\|"
                         ;; For
                         "\\(^[[:space:]]*for \\)\\|"
                         ;; While
                         "\\(^[[:space:]]*while \\)\\|"
                         ;; Construct
                         "\\(^[[:space:]]*construct(.*)\\)\\|"
                         ;; Ondestroy
                         "\\(^[[:space:]]*ondestroy\\)\\|"
                         ;; Switch
                         "\\(^[[:space:]]*switch \\)\\|"
                         ;; Case
                         "\\(^[[:space:]]*case \\)\\|"
                         ;; Init
                         "\\(^[[:space:]]*init\\([[:space:]]*\\(//\\|/\\*\\).*\\)?$\\)\\|"
                         ;; Initlater
                         "\\(^[[:space:]]*initlater\\([[:space:]]*\\(//\\|/\\*\\).*\\)?$\\)\\|"
                         ;; Begin
                         "\\(->[[:space:]]*begin\\([[:space:]]*\\(//\\|/\\*\\).*\\)?$\\)"
                         ))
            tab-width 0)))))

(defun wurstscriptp-previous-line-indent ()
  (save-excursion
    (previous-line)
    (beginning-of-line)
    (if (= (line-number-at-pos) 1)
        ;; Then
        0                               ; Beginning of file
      ;; Else
      (if (looking-at
           ;; whitespace...comment.............ending.
           "^[[:space:]]*\\(//.*\\|/\\*.*\\)?$")
          ;; Then
          (wurstscriptp-previous-line-indent)
        ;; Else
        (current-indentation)))))

(defun wurstscriptp-calculate-indentation ()
  (if (and
       (eq this-command last-command)
       (>= (current-indentation) tab-width))
      ;; Then
      (- (current-indentation) tab-width)
    ;; Else
    (+ (wurstscriptp-previous-line-indent)
       (wurstscriptp-additional-number-of-indents))))

(defun wurstscriptp-indent-line ()
  (interactive)
  (indent-line-to (wurstscriptp-calculate-indentation)))

(defun wurstscriptp-new-line ()
  (interactive)
  (newline)
  (indent-relative t))

;; ================================================================
;; Compilation
;; ================================================================

(setq wurstscriptp-build-last-args
      "-stacktraces -runcompiletimefunctions -injectobjects ")
(setq wurstscriptp-build-last-project nil)

(defun wurstscriptp-project-root (project-root-or-child-path)
  (locate-dominating-file
   project-root-or-child-path
   "map.w3x"))
   ;; (lambda (dir)
   ;;   (directory-files dir nil ".*\\.w3x")) ))

(defun wurstscriptp-build-dir (project-root-or-child-path)
  (concat (wurstscriptp-project-root project-root-or-child-path) "_build/"))

;; (defun wurstscriptp-map-file (project-root-or-child-path)
;;   (car (directory-files
;;         (wurstscriptp-project-root project-root-or-child-path)
;;         t
;;         ".*\.w3x")))

(defun wurstscriptp-vscode-settings-content (project-root-or-child-path)
  (with-temp-buffer
    (insert-file-contents (concat
                           (wurstscriptp-project-root project-root-or-child-path)
                           ".vscode/settings.json"))
    (buffer-string)))

(defun wurstscriptp-value-from-vscode-settings (value project-root-or-child-path)
  (let ((contents (wurstscriptp-vscode-settings-content project-root-or-child-path)))
    (let ((prefix (concat "\"" value "\": \"")))
      (substring contents
                 (+ (length prefix)
                    (string-match-p prefix contents))
                 ;; Find first endline after prefix match
                 (string-match-p "\"," contents (string-match-p prefix contents))))))

(defun wurstscriptp-wurstjar-path (project-root-or-child-path)
  (wurstscriptp-value-from-vscode-settings
   "wurst.wurstJar"
   project-root-or-child-path))

(defun wurstscriptp-commonj-path (project-root-or-child-path)
  (concat (file-name-directory
           (wurstscriptp-wurstjar-path project-root-or-child-path))
          "common.j"))

(defun wurstscriptp-blizzardj-path (project-root-or-child-path)
  (concat (file-name-directory
           (wurstscriptp-wurstjar-path project-root-or-child-path))
          "blizzard.j"))

(defun wurstscriptp-wc3-path (project-root-or-child-path)
  (wurstscriptp-value-from-vscode-settings
   "wurst.wc3path"
   project-root-or-child-path))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun wurstscriptp-do-build-command (cmd project-root)
  (let ((cmd-fixed (replace-in-string "/" "\\" (concat cmd " &"))))
   (message cmd-fixed)
    (async-shell-command cmd-fixed "*Wurstscript Build Output*")
    (with-current-buffer "*Wurstscript Build Output*"
      (insert (concat "\n(output from: '" cmd-fixed "')\n\n"))
      (goto-char (point-max))))
  (let ((proc (get-buffer-process "*Wurstscript Build Output*")))
    (when (process-live-p proc)
      (set-process-sentinel proc #'wurstscriptp-finalize-sentinel))))

(defun wurstscriptp-finalize-sentinel (process signal)
  (when (memq (process-status process) '(exit signal))
    (wurstscriptp-finalize-build wurstscriptp-build-last-project)
    (shell-command-sentinel process signal)))

(defun wurstscriptp-finalize-build (project-root-or-child-path)
  (rename-file
   (concat (wurstscriptp-project-root project-root-or-child-path) "build-map.w3x")
   (concat (wurstscriptp-build-dir    project-root-or-child-path) "build-map.w3x") t)
  (let ((obj-dir (concat
                  (wurstscriptp-project-root project-root-or-child-path)
                  "objectEditingOutput/")))
    (when (file-directory-p obj-dir)
      (copy-directory obj-dir (concat
                               (wurstscriptp-build-dir project-root-or-child-path)
                               "objectEditingOutput/"))
      (delete-directory obj-dir t nil))))

(defun wurstscriptp-prepare-build-dir (project-root-or-child-path)
  (let ((build-dir (wurstscriptp-build-dir project-root-or-child-path)))
    (when (not (file-directory-p build-dir))
      (mkdir build-dir t))
    (copy-file
     (concat (wurstscriptp-project-root project-root-or-child-path) "map.w3x")
     (concat (wurstscriptp-project-root project-root-or-child-path) "build-map.w3x") t)
    (copy-file
     (wurstscriptp-blizzardj-path project-root-or-child-path)
     (concat build-dir "blizzard.j") t)
    (copy-file
     (wurstscriptp-commonj-path project-root-or-child-path)
     (concat build-dir "common.j") t)
    build-dir))

(defun wurstscriptp-build (project-dir arguments)
  (setq wurstscriptp-build-last-project project-dir)
  (setq wurstscriptp-build-last-args arguments)
  (let ((build-dir (wurstscriptp-prepare-build-dir project-dir)))
    (let ((cmd (concat
                "java -jar "
                (wurstscriptp-wurstjar-path project-dir)
                " "
                arguments
                " "
                build-dir "blizzard.j"
                " "
                build-dir "common.j"
                " "
                (wurstscriptp-project-root project-dir) "build-map.w3x"
                ;build-dir "map.w3x"
                )))
      (wurstscriptp-do-build-command cmd project-dir))))

(defun wurstscript-build (project-dir arguments)
  (interactive (list
                (read-directory-name "Build project: "
                                     (or (wurstscriptp-project-root (buffer-file-name))
                                         wurstscriptp-build-last-project
                                         default-directory))
                (read-string "Command line arguments: "
                             wurstscriptp-build-last-args)))
  (wurstscriptp-build project-dir arguments))

;; ================================================================
;; Finalization
;; ================================================================

(define-derived-mode wurstscript-mode prog-mode "wurstscript mode"
  :syntax-table wurstscript-syntax-table
  (setq font-lock-defaults '((wurstscript-font-lock-keywords)))
  (setq-local indent-line-function #'wurstscriptp-indent-line)
  (local-set-key (kbd "RET") 'wurstscriptp-new-line)
  (local-set-key (kbd "C-j") 'wurstscriptp-new-line)
  (local-set-key (kbd "C-c C-b") 'wurstscript-build))

(add-to-list 'auto-mode-alist '("\\.wurst\\'" . wurstscript-mode))

(provide 'wurstscript-mode)
