;;; Package --- Summary
;;; Commentary:
;;; Code:

;; define list of packages to install
(defvar my_packages
  '(better-defaults
    material-theme
    exec-path-from-shell
    ein
    elpy
    flycheck                        ;; On the fly syntax checking
    py-autopep8                     ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    magit   
    pyenv-mode))

;; install all packages in list
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my_packages)
;;; packages.el ends here
