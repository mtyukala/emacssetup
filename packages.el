;;; Package --- Summary
;;; Commentary:
;;; Code:

;; define list of packages to install
(defvar my_packages
  '(better-defaults
    material-theme
    exec-path-from-shellu
    exec-path-from-shellu
    ein
    elpy
    flycheck                        ;; On the fly syntax checking
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    magit
    pyenv-mode))

(defun my-packages-installed-p ()
  "Install all packages defined above."
   (loop for p in my_packages
          when (not (package-installed-p p)) do (return nil)
                  finally (return t)))
;;; packages.el ends here
