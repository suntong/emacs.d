;; -*- emacs-lisp -*-

;; Copyright (C) 2015-2021 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; based on
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/

;;; Commentary:

;; This is go-mode initialization.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package go-mode
  :ensure t
  :mode "\\.go\\(\\'\\|\\.\\)"

  :hook (go-mode . dev/go-mode-hook)

  :config
  (defun dev/go-mode-hook ()
    (setq tab-width 2)
    ;; (setq indent-tabs-mode nil)
    ;; (flycheck-mode t)
    ;; (yas-minor-mode-on)
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

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; - The built-in xref package provides cross-references.
;; - The built-in Flymake package provides an on-the-fly diagnostic overlay.
;; - Company mode displays code completion candidates (with a richer UI than the built-in completion-at-point).
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  ;; The CAPF back-end provides a bridge to the standard completion-at-point-functions facility, and thus works with any major mode that defines a proper completion function.
  (setq lsp-completion-provider :capf)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  ;; don't add any dely before trying to complete thing being typed
  ;; the call/response to gopls is asynchronous so this should have little
  ;; to no affect on edit latency
  (setq company-idle-delay 0.1)
  ;; start completing after a single character instead of 3
  (setq company-minimum-prefix-length 1)
  ;; align fields in completions
  (setq company-tooltip-align-annotations t)
  )

;; End:
