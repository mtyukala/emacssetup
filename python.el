;;; Package --- Summary
;;; Commentary:
;;; Code:
(use-package elpy
  :ensure t
  :init
(elpy-enable))
;;(pyenv-mode)
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'before-save-hook #'autopep8-before-save)
(setq py-autopep8-options '("--ignore=E501"))
(setq py-autopep8-options '("--max-line-length=100"))

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'python-mode-hook 'flycheck-mode))

;; (use-package lsp
;;   :after company
;;   :config
;;   (add-hook 'python-mode-hook 'lsp-mode)
;;   (add-to-list 'company-backends 'company-lsp))

;;; python.el ends here
