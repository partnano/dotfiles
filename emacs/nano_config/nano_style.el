;; styling!

(setq-default cursor-type '(bar . 2))

(global-linum-mode t)
(setq-default line-number-mode 't)
(setq-default column-number-mode 't)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(scroll-bar-mode -1)

;; Use font-lock everywhere.
(global-font-lock-mode t)

;; We have CPU to spare; highlight all syntax categories.
(setq font-lock-maximum-decoration t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%b" ("%b"))))

(setq blink-cursor-blinks -1)

(load "~/nano_config/heartbeat.el")
;; (add-hook 'after-init-hook 'heartbeat-cursor-mode)
