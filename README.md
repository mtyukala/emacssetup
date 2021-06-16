# emacs setup
For this configuration to work. You must have a .emacssetup folder and a .emacs file in your home folder.

**This is how your .emacs file must look like:**
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
(load "java_config.el")
(load "python_config.el")
(load "web.el")
  
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
