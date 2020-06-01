;;; Package --- Summary
;;; Commentary:
;;; Code:
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; (use-package elpy
;;   :mode ("\\.py\\'" . python-mode)
;; 	     :pin melpa-stable
;; 	     :config
;;              (setq elpy-rpc-backend "jedi")
;; 	     (elpy-enable))

;; (use-package python-black
;;   :demand t
;;   :after python)

(use-package blacken
  :pin melpa
	     :demand t
	     :config
	     (add-hook 'python-mode-hook 'blacken-mode)
	     :after python)


(use-package pyenv
  :ensure t
  :init
  (pyenv-mode))

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters            "jupyter")

;; Enable Flycheck
(when (require 'flycheck nil t)
  ;;(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; jedi mode setup
;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   :hook
;;   ((python-mode . jedi:setup))
;;   :init
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (add-hook 'python-mode-hook
;;             (lambda () (add-to-list 'company-backends 'company-jedi))))

;; (setq jedi-custom-file (expand-file-name "jedi-custom.el" user-emacs-directory))
;; (when (file-exists-p jedi-custom-file)
;;   (load jedi-custom-file))

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))
;;; python_config.el ends here
