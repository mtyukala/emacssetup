;;; Package --- Summary
;;; Commentary:
;;; Code:
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(require 'go-autocomplete)
(require 'go-autocomplete-config)
(ac-config-default)
(require 'go-complete)
(add-hook 'completion-at-point-functions 'go-complete-at-point)

(defun auto-complete-for-go ()
  "Setup auto complete."
  (auto-complete-mode 1)
  )
(add-hook 'go-mode-hook 'auto-complete-for-go)

;; set exec path
(defun set-exec-path-from-shell-PATH ()
  "Setting exec path from shell."
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
;; $GOPATH/bin/gocode
(setenv "GOPATH" "$GOPATH/bin/gocode")

;; format on save
(add-to-list 'exec-path "$GOPATH/bin/gocode/bin")
;;(add-hook 'before-save-hook 'gofmt-before-save)

(defun my-go-mode-hook ()
  "Go mode hooks."
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)
;;; go_config.el ends here
