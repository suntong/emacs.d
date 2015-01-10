;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is addon to extend emacs editing features copied from my old brief mode.

;;; Code:

;; ***************************************************************
;; Jamie Lokier's folding mode
;; ***************************************************************
(autoload 'folding-mode "folding" "Folding mode" t)
(when (load "folding" 'noerror)
  (folding-mode-add-find-file-hook))

;;;_* End
