# emacssetup
For this confuguration to work. You must have a .emacssetup folder and .emacs file in your home folder.

**This is how your .emacs file look like:**
```
;;; Package --- Summary
;;; Commentary:
;;; Code:
 
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((my-emacs-dir "~/.emacssetup")
  (default-directory my-emacs-dir))
  (setq load-path (cons my-emacs-dir load-path))
  (normal-top-level-add-subdirs-to-load-path)))

(load "packages.el")
(load "basic.el")
;;(load "java.el")
(load "python_config.el")
(load "web.el")

;; desktop mode
(use-package desktop
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

 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(package-selected-packages
   (quote
    (company-jedi abbrev lsp-mode find-file-in-project diffscuss-mode py-autopep8 vimish-fold web-beautify web-completion-data highlight recently desktop-environment realgud meghanada hlinum highlight-symbol company aggressive-indent flycheck flylisp highlight-quoted smartparens material-theme js2-mode)))
 '(pyvenv-workon "denv"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here


```
