
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
                           "destroy" "new" "while" "begin" "array" "skip"
                           "enum" "abstract" "interface" "implements" "module"
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

(defun wurstscript-calculate-indentation ()
  (let ((cur-ind (current-indentation)))
  (save-excursion
    (previous-line)
    (if (<= (current-indentation) (- cur-ind tab-width))
        0
      (+ cur-ind tab-width)))))

(defun wurstscript-indent-line ()
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (wurstscript-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun wurstscript-new-line ()
  (interactive)
  (newline)
  (indent-relative t))

(define-derived-mode wurstscript-mode prog-mode "wurstscript mode"
:syntax-table wurstscript-syntax-table
(setq font-lock-defaults '((wurstscript-font-lock-keywords)))
(setq-local indent-line-function #'wurstscript-indent-line)
(local-set-key (kbd "RET") 'wurstscript-new-line)
(local-set-key (kbd "C-j") 'wurstscript-new-line))

(add-to-list 'auto-mode-alist '("\\.wurst\\'" . wurstscript-mode))

(provide 'wurstscript-mode)
