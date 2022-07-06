
(require 'consult)

(defvar grep-toolbox--consult-ripgrep-builder-cmd nil)

(defun grep-toolbox--consult-ripgrep-builder (input)
  "Build command line given INPUT."
  (pcase-let* ((cmd (split-string-and-unquote grep-toolbox--consult-ripgrep-builder-cmd))
               (type (consult--ripgrep-regexp-type (car cmd)))
               (`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg type
                                      (if (member "--smart-case" cmd)
                                          (let ((case-fold-search nil))
                                            ;; Case insensitive if there are no uppercase letters
                                            (not (string-match-p "[[:upper:]]" input)))
                                        (member "--ignore-case" cmd))))
               (output-cmd (append
                            cmd
                            (and (eq type 'pcre) '("-P"))
                            (list  "-e" (consult--join-regexps re type)) opts)))
    (when re
      (list :command output-cmd
            :highlight hl))))

(defun grep-toolbox-consult-ripgrep-cmd (cmd initial)
  "Run consult-ripgrep but with a specific CMD (and INITIAL input)."
  (let ((grep-toolbox--consult-ripgrep-builder-cmd cmd))
    (consult--grep "Ripgrep" #'grep-toolbox--consult-ripgrep-builder "" initial)))

(defun consult-ripgrep-multidir (dirs flags initial)
  "Like consult-grep, but support multiple directories DIRS, a list.  Also FLAGS and INITIAL input."
  (grep-toolbox-consult-ripgrep-cmd
   (concat "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / "
           "--smart-case --no-heading --line-number "
           flags " "
           ;; The order of dirs has to be reversed for some reason.
           ;; The last dir in the ripgrep command gets its output first.
           (string-join (reverse dirs) " ") " ")
   initial))

(defmacro without-vertico-posframe (&rest body)
  "Execute the forms in BODY with vertico-posframe-mode disabled."
  `(let ((posframe-p (bound-and-true-p vertico-posframe-mode)))
     (unwind-protect
         ;; Action
         (progn
           (when posframe-p (vertico-posframe-mode -1))
           ,@body)
       ;; Unwind
       (when posframe-p (vertico-posframe-mode 1)))))

(defun region-when-active ()
  "Return region when region-active-p"
  (if (region-active-p) (buffer-substring (region-beginning) (region-end)) nil))

(defun grep-toolbox-consult-ripgrep-default-directory ()
  "Run consult-ripgrep in default-directory"
  (interactive)
  (without-vertico-posframe
   (consult-ripgrep default-directory (region-when-active))))

(defun grep-toolbox-consult-ripgrep ()
  "Run consult-ripgrep, but inherit region when active"
  (interactive)
  (without-vertico-posframe
   (consult-ripgrep nil (region-when-active))))

(defun grep-toolbox-consult-line ()
  "Run consult-line, but inherit region if active"
  (interactive)
  (without-vertico-posframe
   (consult-line (region-when-active))))

(provide 'grep-toolbox)
;;; grep-toolbox.el ends here
