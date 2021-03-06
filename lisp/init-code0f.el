;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is file types init. It depends heavily on `use-package' by John Wiegley.

;;; Code:

;;;_* html-mode, Adobe Extend Script
(add-hook 'html-mode-hook #'(lambda nil (setq sgml-mode t)))

;;;_* css-mode

(use-package css-mode
  :mode ("\\.css\\'" . css-mode))

;;;_* further customizations
(require 'preseed-generic-mode)

;;;_* end
