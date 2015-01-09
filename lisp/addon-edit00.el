;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on: 
;;  https://github.com/svenax/dotfiles/blob/master/emacs/emacs.d/init.el
;;  https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el

;;; Commentary:

;; This is addon to extend emacs editing features. 

;;; Code:

;;;_ , write-room

(defun write-room ()
  "Make a frame without any bling."
  (interactive)
  ;; to restore:
  ;; (setq mode-line-format (default-value 'mode-line-format))
  (let ((frame (make-frame
                '((minibuffer . nil)
                  (vertical-scroll-bars . nil)
                  (left-fringe . 0); no fringe
                  (right-fringe . 0)
                  (background-mode . dark)
                  (background-color . "cornsilk")
                  (foreground-color . "black")
                  (cursor-color . "green")
                  (border-width . 0)
                  (border-color . "black"); should be unnecessary
                  (internal-border-width . 64); whitespace!
                  (cursor-type . box)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (fullscreen . fullboth)  ; this should work
                  (unsplittable . t)))))
    (select-frame frame)
    (find-file "~/tmp/temp.md")
    (setq mode-line-format nil
          fill-column 65)
    (set-window-margins (selected-window) 50 50)))

;;;_** Functions

(defun sa-join-lines ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun sa-smart-open-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun sa-smart-open-line (arg)
  "Insert an empty line after the current line."
  (interactive "P")
  (if arg
      (sa-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun sa-pad-line-ending ()
  "Pad out the line with the last character."
  (interactive)
  (end-of-line)
  (let ((f (symbol-value 'fill-column)))
    (insert-char (char-before) f)
    (move-to-column f)
    (kill-line)))


;;;_** Key bindings
(bind-key "A-j" 'sa-join-lines)
(bind-key "A-<RET>" 'sa-smart-open-line)

(bind-key "RET" 'newline-and-indent)
(bind-key "M-<SPC>" 'cycle-spacing)

(bind-key "C-, o" 'ff-find-other-file)


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:
