;;; init.el --- emacs init file

;; Copyright (C) 2015 Tong Sun

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
;; custom.el. It depends heavily on `use-package' by John Wiegley.

;; Components:
;; - addon-emacs00, to extend emacs 
;; - addon-edit00, to extend emacs editing features
;; - init-code00, for init all programing code. 
;;   + init-code0p, programming. Others can be put into code1p, code2p, etc.
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

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;;;_* Load paths and customizations
(load (expand-file-name
         "load-path" (file-name-directory load-file-name)) nil t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(load-library "init-emacs00") 
(load-library "init-use-package")

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)
;; (setenv "TZ" "UTC+5")

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
;(load-library "") 
;;;_** Emacs configuration
;(load-library "init-font")
(load-library "addon-emacs00") 
(load-library "addon-edit00") 
(message "Emacs configuration done.")

;;;_** Package configuration
(load-library "init-edit00") 
(load-library "init-code00") 
(load-library "init-doc00") 
(message "Packages configuration done.")

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
