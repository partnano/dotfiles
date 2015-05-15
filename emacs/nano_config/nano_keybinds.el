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

(define-minor-mode nano-mode
  "nano customized emacs binds. These are my preferred binds. Sorry for your inconvinience. 
Actually not sorry. It's your fault. Not mine."
  :lighter " nano"
  :keymap nano-mode-keymap
  :global 1
  )

(add-hook 'after-init-hook 'nano-mode)
(provide 'nano-mode)

;; functions

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

