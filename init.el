;; Load stuff
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'mye-visuals)
(require 'mye-keys)
(require 'mye-modes)
(require 'mye-external-packages)
(require 'mye-no-mouse)
(package-initialize)

;; Misc
(defalias 'yes-or-no-p 'y-or-n-p)
(setenv "PAGER" "cat") ;; Hack to get git log and the like to work
