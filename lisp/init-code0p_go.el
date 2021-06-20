;;; init-code0p_go.el --- Go mode setup
;; -*- emacs-lisp -*-

;; Copyright (C) 2015-2021 Tong Sun
;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is go-mode initialization.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package go-mode
  :ensure t
  :mode "\\.go\\(\\'\\|\\.\\)"
  ;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

  ;; https://github.com/jwiegley/use-package#hooks
  :hook (
	 (go-mode . dev/go-mode-hook)
	 (go-mode . lsp-deferred)
	 (go-mode . lsp-go-install-save-hooks)
	 )

  :config
  (defun dev/go-mode-hook ()
    (setq tab-width 2)
    ;; (setq indent-tabs-mode nil)
    ;; (flycheck-mode t)
    ;; (yas-minor-mode-on)
    )

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    "LSP Go save hooks."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    )

  ;; https://github.com/nlamirault/gotest.el
  ;; https://github.com/cweill/gotests
  (use-package gotest
    :bind (:map go-mode-map
		("C-c t p" . go-test-current-project)
		("C-c t f" . go-test-current-file)
		("C-c t t" . go-test-current-test)
		("C-c t x" . go-run)))

  ;; https://github.com/s-kostyaev/go-gen-test
  ;; https://github.com/cweill/gotests
  (use-package go-gen-test
    :bind (:map go-mode-map
		("C-c t g" . go-gen-test-dwim)))

  )

(provide 'init-code0p_go)
;;; init-code0p_go.el ends here
