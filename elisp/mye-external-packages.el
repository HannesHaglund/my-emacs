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
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(setup-melpa)

;; =====================================
;; PACKAGES
;; =====================================

(if (>= emacs-major-version 24.4)
    (ensure-package-installed 'magit))

(require 'whitespace)
(setq whitespace-style '(face tabs lines-tail))
(global-whitespace-mode t)

(provide 'mye-external-packages)
