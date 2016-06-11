;;; init.el --- emacs init file

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on: 
;;  http://truongtx.me/2013/01/07/emacs-package-manager/

;;; Commentary:

;; This is a standalone package-installation script.

;; Use "Evaluate Buffer" to do the installation, or include it in
;; init.el so as to check & install everytime emacs starts.

;; Select & evaluate the following code to show the list of packages:
;; (list-packages)

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
  '(undo-tree
    auto-complete
    markdown-mode markdown-toc
    yaml-mode
    go-autocomplete go-eldoc go-mode
    ))
(dolist (p myown/packages)
   (when (not (package-installed-p p))
     (package-install p)))

;; Packages being part of Emacs:
;; * cc-mode cperl-mode 

;; Packages to ingore:
;; - yasnippet, way to big for me

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:
