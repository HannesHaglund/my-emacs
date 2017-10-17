;; Setup load path
(add-to-list 'load-path "./elisp/")

;; Load stuff
(load "mye-visuals")
(load "mye-keys")
(load "mye-modes")
(load "mye-external-packages")
(package-initialize)

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)
