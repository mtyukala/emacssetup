;;; Package --- Summary
;;; Commentary:
;;; Code:

(elpy-enable)
(pyenv-mode)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package lsp
  :after company
  :config
  (add-hook 'python-mode-hook 'lsp-mode)
  (add-to-list 'company-backends 'company-lsp))

;;; python.el ends here
