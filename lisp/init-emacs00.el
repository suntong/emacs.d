;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is Emacs initialization

;;; Code:

;;;_* Enable disabled commands

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;;_* Preliminary customization

;; Overwrite highlighted block
;; http://www.emacswiki.org/emacs/DeleteSelectionMode
(delete-selection-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Turns on full path in titlebar
(setq frame-title-format "%S: %f")

;; show column number of the cusor
(column-number-mode 1)

;;;; Be silent about successful auto saving
(defadvice do-auto-save (around do-auto-save-silent activate)
  (ad-set-arg 0 t)
  ad-do-it)

;;;; ansi-color
(setq ansi-color-for-comint-mode t)

;;;_* Mule / conding.c
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(define-coding-system-alias 'UTF-8 'utf-8)

;; End:
