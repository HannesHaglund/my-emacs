
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
                                        (member "--ignore-case" cmd)))))
    (when re
      (list :command
            (append cmd
                    (and (eq type 'pcre) '("-P"))
                    (list  "-e" (consult--join-regexps re type))
                    opts)
            :highlight hl))))


(defun grep-toolbox-consult-ripgrep-cmd (cmd)
  (let ((grep-toolbox--consult-ripgrep-builder-cmd cmd))
    (consult--grep "Ripgrep" #'grep-toolbox--consult-ripgrep-builder "" "")))

(defun grep-toolbox-make-cmd (dirs extra-flags)
  (string-join
   (mapcar #'(lambda (dir) (concat "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number " extra-flags " " dir))
           dirs)
   " && "))

(defun grep-toolbox-test ()
  (interactive)
  (grep-toolbox-consult-ripgrep-cmd
   (grep-toolbox-make-cmd '("d:/Documents/code/my-emacs/elisp/" "d:/Documents/code/cv") "-g*aux")))


(provide 'grep-toolbox)
;;; grep-toolbox.el ends here
