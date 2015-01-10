;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is Emacs initialization

;;; Code:

;;;_* Enable disabled commands
;;********************************************************************

(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;;;_* Preliminary customization
;;********************************************************************

;;;_ - debugging
;;====================================================================
;; Set the debug option to enable a backtrace when a
;; problem occurs. Temp disabled because of an error in sys lib.
;(setq debug-on-error t)
;; Increase the "*Messages*" buffer size
(setq message-log-max 800)

;;;_ - emacs configuration
;;====================================================================

;; Tab setup
(setq-default
 tab-width 8
 standard-indent 8
 indent-tabs-mode t)			; makes sure tabs are used.

;; bypass "please type yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; force Emacs to scroll only one line when scrolling
(setq scroll-step 1)

;; To turn on auto-fill mode, use "M-x auto-fill-mode".
(setq-default fill-column 76)

;; turn auto compression on
(auto-compression-mode 1)

;; enable custom faces
(custom-set-faces)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; overcome "Variable binding depth exceeds max-specpdl-size"
;(setq max-specpdl-size 2000)

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

;; unidiff-formatted differences
(setq diff-switches "-ubBwd")

;; Pull current X selection into search string, from Mouse-2.
(define-key isearch-mode-map [down-mouse-2] nil)
(define-key isearch-mode-map [mouse-2] 'isearch-yank-x-selection)

;;;; Be silent about successful auto saving
(defadvice do-auto-save (around do-auto-save-silent activate)
  (ad-set-arg 0 t)
  ad-do-it)

;;;; ansi-color
(setq ansi-color-for-comint-mode t)

;;;_ - Wheel Mouse
;;====================================================================
;; Enable wheelmouse support by default
(mouse-wheel-mode t)
;; have the mouse scroll wheel affect the window it is over,
;; which is not necessarily the currently selected window
(setq mouse-wheel-follow-mouse t)
;; normal scroll, 5 lines; shift scroll, near full screen
(setq mouse-wheel-scroll-amount '(5 . nil))

;;;_ - Syntax Highlighting
;;====================================================================
;; Font Locking Setup. Turn on font-lock mode
(global-font-lock-mode t)
; Maximum colors
(setq font-lock-maximum-decoration t)
;(setq font-lock-maximum-size 800000)
; The string color always looks too dim to me, make it brighter
;(set-face-foreground 'font-lock-string-face "#f0c0d0")

;;;_ - Mule / conding.c
;;====================================================================
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(define-coding-system-alias 'UTF-8 'utf-8)

;; End:
