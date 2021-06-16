;;; Package --- Summary
;;; Commentary:
;;; Code:

;; duplicate the current line
(defun duplicate-line()
  "Duplicates the current line as it is."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )
(global-set-key (kbd "C-d") 'duplicate-line)
(global-unset-key (kbd "C-z"))
(display-time-mode 1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
;;(require 'hlinum)
;;(hlinum-active)
;;(setq display-line-numbers-type 'relative)

;;
(size-indication-mode t)
(save-place-mode 1)
(show-paren-mode 1)

;; use y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (find-file-in-project diffscuss-mode  vimish-fold web-beautify web-completion-data highlight recently desktop-environment   highlight-symbol aggressive-indent flycheck flylisp highlight-quoted material-theme js2-mode))))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")
(setq suggest-key-bindings 4)
(setq
 inhibit-startup-screen t
 column-number-mode t
 scroll-error-top-bottom t
 use-package-always-ensure t)

(set-language-environment "UTF-8")

;;; Fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 3)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)
(setq-default cursor-type 'hbar)

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)
(setq-default fill-column 80)

;;; Set undo limits
(setq undo-limit (* 16 1024 1024))
(setq undo-strong-limit (* 24 1024 1024))
(setq undo-outer-limit (* 64 1024 1024))

;; Ignore case for completion
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; Search highlight
(setq search-highlight t)
(setq query-replace-highlight t)

;; Newline at end of file
(setq require-final-newline t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(load-theme 'material t)

(set-cursor-color "#ffffff")

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; delete whitespace
(defun my-delete-trailing-whitespace ()
  "Deletes trailing white space throughout the buffer."
  (when (not (string= (file-name-extension buffer-file-name) "http"))
    (delete-trailing-whitespace)))


;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; ido mode
(ido-mode 1)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-enable-flex-matching t)
(define-key (cdr ido-minor-mode-map-entry) [(region-end)map write-file] nil)

;;  tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; smartparens
(show-paren-mode 1)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(setq-default sp-autoskip-closing-pair t)
  
(set-frame-font "Source Code Pro 13")
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")

;; start in fullscreen mode
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;;; highlight-quoted
(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
(add-hook 'lisp-mode-hook #'highlight-quoted-mode)
(add-hook 'scheme-mode-hook #'highlight-quoted-mode)

(defun my-sh-completion-at-point ()
  "Docs comment here."
  (let ((end (point))
        (beg (save-excursion (sh-beginning-of-command))))
    (when (and beg (> end beg))
      (bash-completion-dynamic-complete-nocomint beg end t))))

(defun my-sh-hook ()
  "Doc comment here."
  (add-hook 'completion-at-point-functions #'my-sh-completion-at-point nil t))

(add-hook 'sh-mode-hook #'my-sh-hook)
(when (commandp 'counsel-M-x)
  (global-set-key [remap execute-extended-command] #'counsel-M-x))
```Emacs-lisp
(quelpa '(eaf :fetcher github
              :repo  "manateelazycat/emacs-application-framework"
              :files ("*")))
(require 'ctable)
;;; basic.el ends here
