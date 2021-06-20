;;; init.el --- emacs init file

;; Copyright (C) 2015-2021 Tong Sun

;;; Commentary:

;;; Code:

;;;_* Startup

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;;;; startup.el
(setq
 inhibit-startup-message t
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message t
 )
;(menu-bar-mode -1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;;; use-package
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(eval-when-compile
  (require 'use-package))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;;;_* Load paths and customizations
(load (expand-file-name
         "load-path" (file-name-directory load-file-name)) nil t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;----------------------------------------------------------------------------
;; Users settings
;;----------------------------------------------------------------------------


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
)

;; lsp-mode will detect company-mode automatically and will use it if its installed
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  ;; The CAPF back-end provides a bridge to the standard completion-at-point-functions facility, and thus works with any major mode that defines a proper completion function.
  (setq lsp-completion-provider :capf)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))


;;----------------------------------------------------------------------------
;; Post initialization
;;----------------------------------------------------------------------------
;;;_* Post initialization


;;; init.el ends here
