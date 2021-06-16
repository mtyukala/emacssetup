;;; Package --- Summary
;;; Commentary:
;;; Code:



(use-package python-mode
  :ensure t
  :config
  (use-package elpy
    :ensure t
    :config (with-eval-after-load 'python (elpy-enable))
  ;;  (remove-hook 'elpy-modules 'elpy-module-flymake)
    ;;(setq elpy-rpc-backend "jedi")
    ;; :bind (:map elpy-mode-map
    ;;       ("M-." . elpy-goto-definition)
    ;;       ("M-," . pop-tag-mark))
    (elpy-enable))
  )
;; (use-package elpy
;;   :ensure t
;;   :after python
;;   :config (elpy-enable))


(use-package elpy
  :defer
  :init
  (defun enable-elpy-once ()
    (elpy-enable)
    (advice-remove 'python-mode 'enable-elpy-once))
  (advice-add 'python-mode :before 'enable-elpy-once)
  :config
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil
	elpy-rpc-python-command "python3"
	python-check-command "pyflakes"
	flycheck-python-flake8-executable "flake8")
(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))

;;; python_config.el ends here
