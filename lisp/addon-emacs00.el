;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on: 
;;  https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;  https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el

;;; Commentary:

;; This is to extend Emacs features. 

;;; Code:

;;;_ , Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;;;_. Keybindings
(require 'bind-key)

;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;;   A-<anything>
;;   M-A-<anything>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;;;_ , global-map

;;;_  . C-?

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)

(defun reformat-json ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (mark) (point) "python -m json.tool" (buffer-name) t)))

;;;_  . M-?

;(bind-key "M-!" 'async-shell-command)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-H" 'mark-paragraph)
(bind-key "M-D" 'mark-defun)

(bind-key "M-T" 'tags-search)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-s n" 'find-name-dired)
;;(bind-key "M-s o" 'occur)

;;;_  . M-C-?

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(defun isearch-backward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-backward))

(bind-key "C-M-r" 'isearch-backward-other-window)

(defun isearch-forward-other-window ()
  (interactive)
  (split-window-vertically)
  (call-interactively 'isearch-forward))

(bind-key "C-M-s" 'isearch-forward-other-window)

;; Some further isearch bindings
(bind-key "C-c" 'isearch-toggle-case-fold isearch-mode-map)
(bind-key "C-t" 'isearch-toggle-regexp isearch-mode-map)
(bind-key "C-^" 'isearch-edit-string isearch-mode-map)
(bind-key "C-i" 'isearch-complete isearch-mode-map)

;;;_  . A-?

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;;;_ , ctl-x-map

;;;_  . C-x ?

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

;;;_  . C-x C-?

(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)


;;;_  . C-x M-?

(bind-key "C-x M-n" 'set-goal-column)

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" 'refill-paragraph)

;;;_ , mode-specific-map

;;;_  . C-c ?

(bind-key "C-c <tab>" 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c c" 'compile)
(bind-key "C-c d" 'delete-current-line)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(bind-key "C-c e b" 'do-eval-buffer)
(bind-key "C-c e c" 'cancel-debug-on-entry)
(bind-key "C-c e d" 'debug-on-entry)
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
(bind-key "C-c e j" 'emacs-lisp-mode)
(bind-key "C-c e l" 'find-library)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e s" 'scratch)
(bind-key "C-c e v" 'edit-variable)

(bind-key "C-c f" 'flush-lines)
(bind-key "C-c g" 'goto-line)


(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defcustom user-initials nil
  "*Initials of this user."
  :set
  #'(lambda (symbol value)
      (if (fboundp 'font-lock-add-keywords)
          (mapc
           #'(lambda (mode)
               (font-lock-add-keywords
                mode (list (list (concat "\\<\\(" value " [^:\n]+\\):")
                                 1 font-lock-warning-face t))))
           '(c-mode c++-mode emacs-lisp-mode lisp-mode
                    python-mode perl-mode java-mode groovy-mode
                    haskell-mode literate-haskell-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c n" 'insert-user-timestamp)
(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;;;_  . C-c C-?

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;;;_  . C-c M-?

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" 'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;;_ , ctl-period-map

;;;_  . C-. ?

(bind-key "C-. m" 'kmacro-keymap)

;;;_  . C-. C-i

(bind-key "C-. C-i" 'indent-rigidly)

;;;_ , help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;;;_  . C-h e ?

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)

(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)


;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:
