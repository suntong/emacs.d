;;; init.el --- emacs init file

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on: 
;;  http://truongtx.me/2013/01/07/emacs-package-manager/

;;; Commentary:

;; This is a standalone package-installation script.

;; Use "Evaluate Buffer" to do the installation, or include it in
;; init.el so as to check & install everytime emacs starts.

;;; Code:

;;----------------------------------------------------------------------------
;; Automatically install required packages
;;----------------------------------------------------------------------------
;;;_* Required packages
;;; This will automatically check if those packages are
;;; missing, it will install them automatically
(when (not package-archive-contents)
   (package-refresh-contents))
(defvar myown/packages
   '(markdown-mode yasnippet))
(dolist (p myown/packages)
   (when (not (package-installed-p p))
     (package-install p)))

;; being part of Emacs: cc-mode cperl-mode 

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:
