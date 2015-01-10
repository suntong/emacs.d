;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is text doc handling init.
;; It depends heavily on `use-package' by John Wiegley.

;;; Code:

;;;_* markdown-mode
(use-package markdown-mode
  :mode ("\\.\\(txt\\|markdown\\|md\\)\\'" . markdown-mode))

;;;_ > markdown-toc
(use-package markdown-toc
  :bind (("C-c C-x T" . markdown-toc/generate-toc)))
;; It will compute the TOC at insert it at current position, but work only for github
;; since those toc anchors are not inserted by default anywhere else.

;;;_* others
(load-library "init-doc00-doc-mode") 

;;;_* End
