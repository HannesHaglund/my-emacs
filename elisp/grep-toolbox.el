
(require 'consult)

(defvar grep-toolbox--consult-ripgrep-builder-cmd nil)
(defvar grep-toolbox--replacement-str "GREP_TOOLBOX_REPLACEMENT_STR")

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
               (output-cmd (string-replace
                            grep-toolbox--replacement-str
                            (string-join (append (and (eq type 'pcre) '("-P")) (list  "-e" (consult--join-regexps re type)) opts) " ")
                            grep-toolbox--consult-ripgrep-builder-cmd)))
    (when re
      (message output-cmd)
      (list :command (split-string-and-unquote output-cmd)
            :highlight hl))))

(defun grep-toolbox-consult-ripgrep-cmd (cmd)
  (let ((grep-toolbox--consult-ripgrep-builder-cmd cmd))
    (consult--grep "Ripgrep" #'grep-toolbox--consult-ripgrep-builder "" "")))

(defun grep-toolbox-make-cmd (dirs extra-flags)
  (string-join
   (mapcar #'(lambda (dir) (concat "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number " extra-flags " " dir " " grep-toolbox--replacement-str))
           dirs)
   " && "))

(defun grep-toolbox-test ()
  (interactive)
  (grep-toolbox-consult-ripgrep-cmd
   (grep-toolbox-make-cmd '("d:/Documents/code/my-emacs/elisp/" "d:/Documents/code/cv") "")))


(provide 'grep-toolbox)
;;; grep-toolbox.el ends here
