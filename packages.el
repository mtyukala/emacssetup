;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'package)

(when (>= emacs-major-version 24)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
  )

(package-initialize)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa '(hydra :repo "abo-abo/hydra" :fetcher github))

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)


(use-package better-defaults
  :ensure t)


(use-package material-theme
  :ensure t
  )

(use-package ein
  :ensure ein
  :init
  :defer)

(use-package elpy
  :ensure t
  :init
  :defer)


(use-package flycheck
  :ensure t
  :init
  :defer)


(use-package py-autopep8
  :ensure t
  :init
  :defer)

(use-package blacken
  :ensure t
  :init
  :config
  (add-hook 'python-mode-hook 'blacken-mode)
  )


(use-package lsp-mode
  :ensure t
  :init
  :config
  (add-hook 'python-mode-hook 'blacken-mode)
  )

(use-package pyenv-mode
  :ensure t
  :init
  :defer)


(use-package magit
  :ensure t
  :init
  :defer)

(use-package ido-vertical-mode
  :ensure t
  :init
  :defer)

(use-package smartparens
  :ensure t
  :init
  ;; (use-package smartparens-config)
  ;; (use-package smartparens-python)
  ;; (use-package smartparens-html)
  ;; (use-package smartparens-markdown)
  ;; (use-package smartparens-javascript)  
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :config
  (setq smartparens-strict-mode t)
  )

;; highlight the current line
(use-package hl-line
  :ensure t
  :init
  :defer
  :config
  (global-hl-line-mode +1))


(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))


;; highlight the current line
(use-package sh-script
  :ensure t
  :init
  :defer
  :config
  (global-hl-line-mode +1))

(use-package bash-completion
  :ensure t
  :init
  :defer
)


(use-package web-mode
  :ensure t
  :config
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)))


(use-package php-mode
  :ensure t
)


(use-package mmm-mode
  :ensure t
  :init
  (setq mmm-js-mode-enter-hook (lambda ()
                                 (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda ()
                                         (setq syntax-ppss-table nil))))


(use-package vue-html-mode
  :ensure t)


(use-package css-mode
  :init
  (setq css-indent-offset 2))


(use-package vue-mode
  :ensure t
)


(use-package pug-mode
  :ensure t
 
  :init
  (setq pug-tab-width 2))


(use-package scss-mode
  :ensure t
  )

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'scss-mode-hook 'prettier-js-mode)
  (add-hook 'xml-hook 'prettier-js-mode)
  :init
  (setq prettier-js-args '("--trailing-comma" "all")))

;; desktop mode
(use-package desktop
  :ensure t
  :defer
  :init
  (setq desktop-auto-save-timeout 300)
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save nil)
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning t)
  (setq desktop-restore-eager 3)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  :config
  (desktop-save-mode 1)

)
;;   (use-package terraform-mode
;;   :ensure t
;;   :init

;;   :defer)
 


(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))


(use-package hlinum
  :ensure t 
  :init
  (hlinum-activate)
  )


 
(use-package highlight-quoted
  :ensure t
  :init
  :defer)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
;;; packages.el ends here
