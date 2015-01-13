;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is ascii doc handling init.

;;; Code:

;;;_* ascii doc-mode
;; (use-package doc-mode
;;   :mode ("\\.a*doc\\($\\|\\.)" . doc-mode))
(autoload 'doc-mode "doc-mode" "Major mode for strcture doc file" t)
(add-to-list 'auto-mode-alist '("\\.a*doc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.a*doc\\." . doc-mode))
;; add folding mode support
(folding-add-to-marks-list 'doc-mode ">>{{{ " ">>}}} " nil)

;; End:
