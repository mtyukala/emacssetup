;;; Package --- Summary
;;; Commentary:
;;; Code:

;;(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("~/.bin/terraform-lsp/terraform-lsp" "-enable-log-file"))
                  :major-modes '(terraform)
                  :server-id 'terraform-ls))
(add-hook 'terraform-mode-hook #'lsp-deferred)
;;; terraform.el ends here
