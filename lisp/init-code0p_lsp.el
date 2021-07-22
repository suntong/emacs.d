;;; init-code0p_lsp.el --- LSP mode setup
;; -*- emacs-lisp -*-

;; Copyright (C) 2021 Tong Sun

;;; Commentary:

;; This is go-mode initialization.

;;; Code:

(eval-when-compile
  (require 'use-package))

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


;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; - The built-in xref package provides cross-references.
;; - The built-in Flymake package provides an on-the-fly diagnostic overlay.
;; - Company mode displays code completion candidates (with a richer UI than the built-in completion-at-point).
;; lsp-mode detects company-mode automatically and will use it if its installed
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  ;; default is Super-L, which is for Mac only
  (setq lsp-keymap-prefix "C-c l")
  ;; trace lsp stuff when debugging
  ;(setq lsp-go-gopls-server-args '("-logfile" "/tmp/gopls.log" "-rpc.trace") )
  :config
  (setq lsp-enable-snippet nil)
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  ;; The CAPF back-end provides a bridge to the standard completion-at-point-functions facility, and thus works with any major mode that defines a proper completion function.
  (setq lsp-completion-provider :capf)

  ; nicer keybindings
  :bind
  (

   ; global
   ("<C-S-down-mouse-1>" . mouse-buffer-menu)

   :map lsp-mode-map
	("<mouse-3>" . mouse-save-then-kill)
	("<C-S-mouse-3>" . lsp-mouse-click)
	("M-r" . lsp-rename)
	("M-/" . lsp-find-references)
	)

  )

(provide 'init-code0p_lsp)
;;; init-code0p_lsp.el ends here
