(require 'package)

(defun setup-melpa ()
  (add-to-list 'package-archives
               '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))))
  ;; ELPA
  ;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
  ;;                          ("marmalade" . "https://marmalade-repo.org/packages/")
  ;;                          ("melpa" . "https://melpa.org/packages/"))))

;; src: https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;           (progn (call-interactively 'package-install t (vector package)) (package))
           (progn (package-install package) package)
         nil)))
   packages))

(package-initialize)

(setup-melpa)

(unless package-archive-contents
  (package-refresh-contents))
(or (file-exists-p package-user-dir)
    (package-refresh-contents))


;; =====================================
;; PACKAGES
;; =====================================

(ensure-package-installed 'helm)
(ensure-package-installed 'helm-ag)
(ensure-package-installed 'dumb-jump)

(helm-mode t)

(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail))
(global-whitespace-mode t)

(provide 'mye-external-packages)
