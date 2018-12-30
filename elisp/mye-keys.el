(defun scroll-down-bind ()
  "Scroll 10 lines down."
  (interactive)
  (let (value)
    (dotimes (i 10 value)
      ;; Dont know why this is reversed
      (scroll-up-line))))

(defun scroll-up-bind ()
  "Scroll 10 lines up."
  (interactive)
  (let (value)
    (dotimes (i 10 value)
      ;; Dont know why this is reversed
      (scroll-down-line))))


(defun wind-bck ()
  "Change window."
  (interactive)
  (other-window -1))

(defun c-tab-bind ()
  "Indent line and move to next."
  (interactive)
  (indent-according-to-mode)
  (next-line))

;; src: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; KEY BIND LIST
    ;; Common
    (define-key map (kbd "M-p") 'scroll-up-bind)
    (define-key map (kbd "M-n") 'scroll-down-bind)
    (define-key map (kbd "C-,") 'other-window)
    (define-key map (kbd "C-.") 'wind-bck)
    (define-key map (kbd "C-;") 'other-frame)
    ;; Repo navigation
    (define-key map (kbd "M-g M-l") 'goto-line)
    (define-key map (kbd "M-g M-g") 'dumb-jump-go)
    (define-key map (kbd "M-g M-h") 'dumb-jump-go-other-window)
    (define-key map (kbd "M-g M-b") 'dumb-jump-back)
    (define-key map (kbd "M-g M-r") 'helm-do-ag-project-root)
    (define-key map (kbd "M-g M-t") 'helm-do-ag)
    ;; Helm override
    (define-key map (kbd "M-x") 'helm-M-x)
    (define-key map (kbd "C-x b") 'helm-buffers-list)
    (define-key map (kbd "C-x C-b") 'helm-buffers-list)
    (define-key map (kbd "C-x C-f") 'helm-find-files)
    map)
  "my-keys-minor-mode keymap.")

;; Refuses to work inside the key map
;; Doing both cause I don't know which works
(global-set-key [C-tab] 'c-tab-bind)
(global-set-key (kbd "<C-tab>") 'c-tab-bind)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(provide 'mye-keys)
