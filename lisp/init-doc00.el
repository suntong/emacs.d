;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is text doc handling init.
;; It depends heavily on `use-package' by John Wiegley.

;;; Code:

(use-package markdown-mode
  :mode ("\\.\\(txt\\|markdown\\|md\\)\\'" . markdown-mode))


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:
