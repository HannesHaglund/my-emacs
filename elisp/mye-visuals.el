;; Theme
(load-theme 'wombat t)

;; Brighter cursor
(set-cursor-color "#eeeeee")

;; Prevent startup screen
(setq inhibit-startup-screen t)

;; linum mode in code buffers
(add-hook 'prog-mode-hook 'linum-mode)

;; visual line mode
(global-visual-line-mode t)

;; no tool bar
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; Full screen on startup
(custom-set-variables
	'(initial-frame-alist (quote ((fullscreen . maximized)))))

(provide 'mye-visuals)
