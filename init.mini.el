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

;;;_* further customizations
(load-library "init-edit00_abbrev_test")

;; (use-package go-mode
;;   :ensure t
;;   :defer t
;;   :hook (go-mode . (lambda ()
;;                      (require 'lsp-go)
;;                      (require 'dap-go)
;;                      (lsp-deferred)))
;; )

;;----------------------------------------------------------------------------
;; Post initialization
;;----------------------------------------------------------------------------
;;;_* Post initialization


;;; init.el ends here
