;;; Package --- Summary
;;; Commentary:
;;; Code:

;; java mode
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            (google-set-c-style)
            (google-make-newline-indent)
            (meghanada-mode t)
            (meghanada-telemetry-enable t)
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq c-basic-offset 2)
            (setq meghanada-server-remote-debug t)
            (setq meghanada-javac-xlint "-Xlint:all,-processing")

            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

;;:config
(use-package realgud
  :ensure t)
;;:commands
(cond
 ((eq system-type 'windows-nt)
  (setq meghanada-java-path(expand-file-name "bin/java" (getenv "JAVA_HOME")))
  (setq meghanada-maven-path "mvn.cmd"))
 (t
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))
 )

;;; java.el ends here
