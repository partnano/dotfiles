;; behavior!

;; Standard Mode for cut, copy & paste. Also for overwriting marked areas.
(custom-set-variables
 '(cua-mode t nil (cua-base)))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(recentf-mode 1) ; keep a list of recently opened files
(desktop-save-mode 1) ; save/restore opened files

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; personal functions
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
