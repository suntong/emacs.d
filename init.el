;;; init.el --- emacs init file

;; Copyright (C) 2015-2021 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on:
;;  https://github.com/purcell/emacs.d/blob/master/init.el
;;  https://github.com/svenax/dotfiles/blob/master/emacs/emacs.d/init.el
;;  https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;  https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el

;;; Commentary:

;; This is my modularized init file, breaking down the normally-humongous
;; init-file into manageable building blocks.

;; This is for my entire Emacs configuration, except for some small use of
;; custom.el.  It depends heavily on `use-package' by John Wiegley.

;; Components:
;; - addon-emacs00, to extend Emacs
;; - addon-edit00, to extend Emacs editing features
;; - init-code00, for init all programing code.
;;   + init-code0p, programming.  Others can be put into code1p, code2p, etc.
;;   + init-code0f, file types, html/css, etc
;;   + init-code0s, misc
;; - init-doc00, for init all text doc handling, adoc, md etc.

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
(load-library "init-emacs00")

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)
;; (setenv "TZ" "UTC+5")

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
;; Bootstrap config
(setq rr-initialization-errors nil)
(require 'init-bootstrap)
(defvar init-files
  '(
    ;;;_** Emacs configuration
    addon-emacs00
    addon-edit00
    addon-edit00_folding

    ;;;_** Package configuration
    init-edit00
    init-code0p
    init-code0f
    init-doc00

    ;; Appearance
    ;init-locales
    ;;----------------------------------------------------------------------------
    ;; Load configs for specific features and modes
    ;;----------------------------------------------------------------------------
    ;; init-smartparens
    ;; init-yasnippet
    ;; init-hippie-expand
    ;; init-bookmarks
    ;; init-helm
    ;; init-eshell
    ;; init-dired
    ;; init-git
    ;; init-ido
    ;; init-ibuffer
    ;; init-search
    ;; init-guidekey
    ;; init-flycheck

    ;; init-packages

    ;; init-defuns
    ;; init-coding

    ;; init-bindings
    ;; init-misc
    ;; init-compile

    ;; specific programming modes modes
    ;; init-csv
    ;; init-ess
    ;; init-haskell
    ;; init-latex
    ;; init-reftex
    ;; init-lisp
    ;; init-markdown
    ;; init-python
    ;; init-stan
    ;; init-org
    ;; init-org-templates
    ;; init-scala
    ;; init-sql
    ;; init-css
    ;; init-javascript
    ;; init-html
    ;; init-hadoop
    ;; init-devops
    ;; init-crontab
    ;; init-docker
    ;; init-http

    ;; company mode
    ;; init-company


    ;; ;; diminish mode line
    ;; init-diminish

    ;; ;; Bindings
    ;; init-bindings
    ;; init-hydra

    ;; ;; Mode mappings
    ;; init-mode-mapping
    ))

(rr-safe-load-init-files)

;;;_** Not installed through `package.el'



;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;(require 'server)
;(unless (server-running-p)
;  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Post initialization
;;----------------------------------------------------------------------------
;;;_* Post initialization

;(load-theme 'wombat)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]" ,load-file-name elapsed)))
            t))



;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:

;;; init.el ends here
