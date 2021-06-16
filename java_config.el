;;; Package --- Summary
;;; Commentary:
;;; Code:

;; java mode
;; (use-package meghanada
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               (google-set-c-style)
;;               (google-make-newline-indent)
;;               (meghanada-mode t)
;;               (meghanada-telemetry-enable t)
;;               (flycheck-mode +1)
;;               (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;               )
;;             )
;;   :config
;;   (use-package realgud
;;     :ensure t)
;;   (cond
;;    ((eq system-type 'windows-nt)
;;     (setq meghanada-java-path(expand-file-name "bin/java" (getenv "JAVA_HOME")))
;;     (setq meghanada-maven-path "mvn.cmd"))
;;    (t
;;     (setq meghanada-java-path "java")
;;     (setq meghanada-maven-path "mvn"))
;;    )
;;   (use-package helm
;;     :ensure t
;;     :config (helm-mode))  
;;   (setq indent-tabs-mode nil)
;;   (setq tab-width 2)
;;   (setq c-basic-offset 2)
;;   (setq meghanada-server-remote-debug t)
;;   (setq meghanada-javac-xlint "-Xlint:all,-processing")
;;   (setq meghanada-server-jvm-option "-Xms256m -Xmx2048m -Dmeghanada.zpage.port=8080")
;;   (setq meghanada-import-static-enable "java.util.Objects,org.junit.Assert")
;;   (setq meghanada-cache-in-project nil)
;;   :commands (meghanada-mode))


(use-package lsp-mode
;;  :init (setq lsp-prefer-flymake nil)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))


(use-package lsp-java
  
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode)
  )
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp
  :ensure t
  :defer)

;;; java_config.el ends here
