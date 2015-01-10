;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on: 
;;  https://github.com/svenax/dotfiles/blob/master/emacs/emacs.d/init.el
;;  https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el

;;; Commentary:

;; This is addon to extend emacs editing features. 

;;; Code:


;;;_* Functions

(defun sa-pad-line-ending ()
  "Pad out the line with the last character."
  (interactive)
  (end-of-line)
  (let ((f (symbol-value 'fill-column)))
    (insert-char (char-before) f)
    (move-to-column f)
    (kill-line)))


;;;_* Key bindings


;;;_* Extra Addons
(load-library "addon-edit00-brief") 


;;;_* End
