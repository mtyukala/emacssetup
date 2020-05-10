;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
;;(require 'use-package)

(when (>= emacs-major-version 24)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
  )

(package-initialize)
(package-refresh-contents)
(package-install 'use-package)


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

(display-time-mode 1)
(global-visual-line-mode)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-mode find-file-in-project diffscuss-mode csproj-mode vimish-fold web-beautify web-completion-data highlight recently desktop-environment realgud meghanada hlinum highlight-symbol company
              (quote auto-complete)
              auto-complete aggressive-indent flycheck flylisp highlight-quoted smartparens csharp-mode material-theme js2-mode ahungry-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-beyond-fill-column-face ((t (:foreground "red")))))

;;; Initialize the package manager
(eval-and-compile
  (require 'package)
  (setq package-enable-at-startup nil)
  (defvar init-el-package-archives-refreshed nil)
  (defun init-el-install-package (package-name)
    (unless (package-installed-p package-name)
      (unless init-el-package-archives-refreshed
        (package-refresh-contents)
        (setq init-el-package-archives-refreshed t))
      (package-install package-name)))
  (defmacro init-el-with-eval-after-load (feature &rest body)
    (declare (indent 1) (debug t))
    (require feature)
    `(with-eval-after-load ',feature ,@body))
  (defmacro init-el-require-package (package-name &optional feature-name)
    (init-el-install-package package-name)
    (require (or feature-name package-name))
    `(init-el-install-package ', package-name)))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(blink-cursor-mode -1)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(setq-default fill-column 80)
;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(load-theme 'material t)


(set-cursor-color "#ffffff")
(setq-default cursor-type 'hbar)

;;; Fix scrolling
(setq mouse-wheel-progressive-speed nil)
(setq scroll-margin 3)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)


;; Wrap lines at 80 characters
(setq-default fill-column 80)

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


(require 'auto-complete)
(global-auto-complete-mode t)
(use-package company
  :ensure t
  :config
  (setq  company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  )
;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

;; delete whitespace
(defun my-delete-trailing-whitespace ()
  "Deletes trailing white space throughout the buffer."
  (when (not (string= (file-name-extension buffer-file-name) "http"))
    (delete-trailing-whitespace)))

(setq-default whitespace-style '(face trailing lines empty indentation::space))
(setq-default whitespace-line-column 80)

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

(init-el-require-package auto-complete)
(global-auto-complete-mode t)
(use-package company
  :ensure t
  :config
  (setq  company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  )
;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

(use-package electric
  :ensure t
  :init
  (progn
    (electric-pair-mode +1)
    )
  )

;; configure line numbers
(add-hook 'prog-mode-hook 'global-linum-mode)
(add-hook 'prog-mode-hook 'highlight-beyond-fill-column)

(setq linum-format "%4d \u2502 ")

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(set-frame-font "Source Code Pro Medium-13")
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")

;; start in fullscreen mode
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


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

;; ido mode
(init-el-require-package ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
;; use current pane for newly opened file
(setq ido-default-file-method 'selected-window)
;; use current pane for newly switched buffer
(setq ido-default-buffer-method 'selected-window)
;; show any name that has the chars you typed
(setq ido-enable-flex-matching t)
;; stop ido from suggesting when naming new file
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;; smartparens
(init-el-require-package smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(setq-default sp-autoskip-closing-pair t)
(sp-local-pair '(c-mode c++-mode java-mode css-mode php-mode js-mode perl-mode
                        cperl-mode rust-mode sh-mode)
               "{" nil
               :post-handlers '((init-el-smartparens-create-and-enter-block "RET")))

;;; highlight-quoted
(init-el-require-package highlight-quoted)
(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
(add-hook 'lisp-mode-hook #'highlight-quoted-mode)
(add-hook 'scheme-mode-hook #'highlight-quoted-mode)


;;; basic.el ends here