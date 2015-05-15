;;;;;;;;;;;;;;;;;;;;;;
;; Userdata
;;;;;;;;;;;;;;;;;;;;;;

(setq user-name "part.nano-")
(setq user-full-name "Bernhard Wick")
(setq user-mail "bernhardcwick@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;
;; Package Managing
;;;;;;;;;;;;;;;;;;;;;;

;; package init
(require 'package)
(require 'cl)
(package-initialize)

;; required sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; required collection of packages
(defvar required-packages
  '(csharp-mode
    markdown-mode
    lua-mode
    json-mode
    ace-jump-mode
    js2-mode
    love-minor-mode
    vala-mode
    vala-snippets
    move-text
    powerline
    rainbow-delimiters
    php-mode
    web-mode
    neotree
    auctex)
  "A list of packages to ensure are installed at launch.")

;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;
;; Package Settings
;;;;;;;;;;;;;;;;;;;;;;

;; c# mode
(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-mode 1))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

;; powerline
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'butt)

;; move text (up / down)
(require 'move-text)
(move-text-default-bindings)

(require 'ace-jump-mode)

(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;

;; keybinds (in form of minor mode)

(defvar nano-mode-keymap (make-keymap) "nano-mode keymap.")
(define-key nano-mode-keymap (kbd "C-,") ctl-x-map)
(define-key nano-mode-keymap (kbd "C-/") 'comment-or-uncomment-region)
(define-key nano-mode-keymap (kbd "C-+") 'text-scale-increase)
(define-key nano-mode-keymap (kbd "C--") 'text-scale-decrease)
(define-key nano-mode-keymap [(shift return)] 'smart-open-line)
(define-key nano-mode-keymap [(control shift return)] 'smart-open-line-above)
(define-key nano-mode-keymap [(control shift backspace)] 'smart-kill-line)
(define-key nano-mode-keymap (kbd "C-c SPC") 'ace-jump-mode)
(define-key nano-mode-keymap (kbd "M-n") (lambda () (interactive) (next-line 5)))
(define-key nano-mode-keymap (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(define-key nano-mode-keymap (kbd "S-M-<left>") 'windmove-left)
(define-key nano-mode-keymap (kbd "S-M-<right>") 'windmove-right)
(define-key nano-mode-keymap (kbd "S-M-<down>") 'windmove-down)
(define-key nano-mode-keymap (kbd "S-M-<up>") 'windmove-up)
(define-key nano-mode-keymap (kbd "C-M-q") 'neotree)
(define-key nano-mode-keymap (kbd "S-SPC") 'dabbrev-expand)

;; can't use newline-and-indent with ido currently :(
;;(define-key nano-mode-keymap (kbd "RET") 'newline-and-indent)

(define-minor-mode nano-mode
  "nano customized emacs binds. These are my preferred binds. 
Sorry for your inconvinience. Have fun."
  :lighter " 나노"
  :keymap nano-mode-keymap
  :global 1
  )

(add-hook 'after-init-hook 'nano-mode)
(provide 'nano-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; Behavior
;;;;;;;;;;;;;;;;;;;;;;

;; Standard Mode for cut, copy & paste. Also for overwriting marked areas.
(custom-set-variables
 '(cua-mode t nil (cua-base)))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(require 'recentf)
(recentf-mode 1) ; keep a list of recently opened files
(setq recentf-max-menu-items 25)

(setq inhibit-startup-message t)
(switch-to-buffer "*scratch*")

;; get the scrolling under control
(setq redisplay-dont-pause t
  scroll-margin 6
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

(setq mouse-wheel-scroll-amount '(6 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p) ;; y or n instead of yes or no
(defalias 'list-buffers 'ibuffer) ;; ibuffer instead of standard buffer

;; ido
(ido-mode 1)
(setq ido-case-fold t)
(setq ido-auto-merge-work-directories-length -1) ;; disable auto-merge
(setq ido-ignore-files '(".meta" "desktop.ini" "~" ".bin" ".ln" ".git" ".gitignore"
                         ".svn" ".db" ".sln" ".lib" ".log")) ;; ignore some files

;; rainbow
(add-hook 'nano-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'nano-mode-hook #'rainbow-identifiers-mode)

;; highlight certain keywords
(add-hook 'nano-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\|NOTE\\):" 1 font-lock-warning-face t)))))

;; org mode
(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "RoyalBlue2" :weight bold))))

;; other stuff that will be categorized at some point
(transient-mark-mode 1)
(setq tab-width 2 indent-tabs-mode nil)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
(setq redisplay-dont-pause t)

;; web-mode for php, html and js files
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Other Functions
;;;;;;;;;;;;;;;;;;;;;;

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-kill-line ()
  "Delete whole line and place curor to the end of the previous line."
  (interactive)
  (kill-whole-line)
  (previous-line 1)
  (move-end-of-line nil))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;;;;;;;;;;;;;;;;;;;;
;; Themes & Style
;;;;;;;;;;;;;;;;;;;;;;

(setq-default cursor-type '(bar . 2))
(global-linum-mode t)
(setq-default line-number-mode 't)
(setq-default column-number-mode 't)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(scroll-bar-mode -1)

;; Use font-lock everywhere
(global-font-lock-mode t)

;; We have CPU to spare; highlight all syntax categories.
(setq font-lock-maximum-decoration t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%b" ("%b"))))

(setq blink-cursor-blinks -1)

(custom-set-faces
 '(default ((t (:family "Source Code Pro"
			:foundry "outline"
			:slant normal
			:weight normal
			:height 120
			:width normal)))))

(push '"/media/partnano/Data/Drive/NanoNanoDev/nano_macs/run/nano_config/themes" custom-theme-load-path)

(load-theme 'jazz t)
(set-face-attribute 'region nil :background "#1E253B")
(set-face-attribute 'default nil :foreground "#BBB")

;; set window size & position
(when window-system (set-frame-size (selected-frame) 150 40))
(setq initial-frame-alist '((left . 50) (top . 50)))
