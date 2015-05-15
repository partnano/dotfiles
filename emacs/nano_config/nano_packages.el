;; packages!

;; package init
(require 'package)
(require 'cl)
(package-initialize)

;; required sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; required collection of packages
(defvar my-packages
  '(csharp-mode
    markdown-mode
    lua-mode
    json-mode
    ace-jump-mode
    js2-mode
    move-text
    powerline
    rainbow-delimiters
    php-mode
    web-mode
    neotree)
  "A list of packages to ensure are installed at launch.")

;; if a package is missing, download and install it
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
 
(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; package config

;; c# mode
(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-mode 1))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

;; powerline
(require 'powerline)
(powerline-default-theme)
(setq powerline-inactive1)
(setq powerline-default-separator 'contour)

;; move text (up / down)
(require 'move-text)
(move-text-default-bindings)

(require 'ace-jump-mode)

;; eclim for java dev
;; NEEDS MORE TINKERING ... CURRENTLY USING JETBRAINS IDE

;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)

;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("C:/Users/Bernhard/Documents/eclipse"))
;;  '(eclim-executable "C:/Users/Bernhard/Documents/eclipse/eclim"))

;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)

;; (require 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)
