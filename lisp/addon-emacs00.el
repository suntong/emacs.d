;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; Based on: 
;;  https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;  https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el

;;; Commentary:

;; This is to extend Emacs features. 

;;; Code:

;;;_* Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;;;_* Keybindings
(require 'bind-key)

;;;_* Global-map:

;;;_ > Cursor movement

;; for control-cursor-up key
(defun pc-keys-scroll-down-one-line ()
  "Scrolls the visible part of the buffer one line down."
  (interactive)
  (let ((scroll-in-place nil))
    (scroll-down 1))
  )

;; for control-cursor-down key
(defun pc-keys-scroll-up-one-line ()
  "Scrolls the visible part of the buffer one line up."
  (interactive)
  (let ((scroll-in-place nil))
    (scroll-up 1))
  )

(bind-key "<C-up>" 'pc-keys-scroll-down-one-line)
(bind-key "<C-down>" 'pc-keys-scroll-up-one-line)
(bind-key "<M-up>" 'backward-paragraph)
(bind-key "<M-down>" 'forward-paragraph)

;;;_ > Deleting

;; Ctrl-minus - Delete curr. buffer
;(bind-key "<C-->" 'kill-this-buffer)
(global-set-key [?\C--] 'kill-this-buffer)

;;;_  - deleting words
; Say I have a line and cusor position like below:
;
; word1                                 word2 ...
;         ^
; After I use delete word, I want to get:
; word1   word2 ...
;
; i.e., only the space between the cursor and word2 is deleted, not with
; word2. I found it very inconvenient for me what emacs is doing:
;
; M-x kill-word: word2 also get killed.
; word1   ...
;
; M-SPC:  space before the cursor get killed
; word1 word2 ...
;
; fixup-whitespace: not quite to the point
;
; M-\:  all space get killed
; word1word2 ...

(defun kill-word-or-whitespace (arg)
  "Kill characters forward until encountering the end of a word or whitespace.
With argument, kill that many words."
  (interactive "*p")
  (save-excursion
    (let ((start (point))
          (next-word-end (progn (forward-word arg) (point)))
          (next-word-start (progn (backward-word arg) (point))))
      (kill-region start
                   (if (if (> arg 0)
                           (<= next-word-start start)
                         (>= next-word-start start))
                       next-word-end
                       next-word-start)))))

(defun backward-kill-word-or-whitespace (arg)
"Kill characters backward until reaching the beginning of a word or
whitespace.  With argument, kill that many words."

  (interactive "*p")
  (kill-word-or-whitespace (- arg)))

;; Ctrl-t - Delete next word, Brief keybinding
(bind-key "C-t" 'kill-word-or-whitespace)
(bind-key "<M-backspace>" 'backward-kill-word-or-whitespace)

;;;_* Personal bindings:

;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  outline-minor-mode key map
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

;;;_ > C-?

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

;;;_ > M-?

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

;;;_ > M-C-?

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

;;;_ > A-?

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;;;_* Ctl-x-map

;;;_ > C-x ?

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)
(bind-key "C-x r W" 'delete-whitespace-rectangle)

;;;_ > C-x C-?

(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)


;;;_ > C-x M-?

(bind-key "C-x M-n" 'set-goal-column)

;;;_  - refill-paragraph
;; (defun refill-paragraph (arg)
;;   (interactive "*P")
;;   (let ((fun (if (memq major-mode '(c-mode c++-mode))
;;                  'c-fill-paragraph
;;                (or fill-paragraph-function
;;                    'fill-paragraph)))
;;         (width (if (numberp arg) arg))
;;         prefix beg end)
;;     (forward-paragraph 1)
;;     (setq end (copy-marker (- (point) 2)))
;;     (forward-line -1)
;;     (let ((b (point)))
;;       (skip-chars-forward "^A-Za-z0-9`'\"(")
;;       (setq prefix (buffer-substring-no-properties b (point))))
;;     (backward-paragraph 1)
;;     (if (eolp)
;;         (forward-char))
;;     (setq beg (point-marker))
;;     (delete-horizontal-space)
;;     (while (< (point) end)
;;       (delete-indentation 1)
;;       (end-of-line))
;;     (let ((fill-column (or width fill-column))
;;           (fill-prefix prefix))
;;       (if prefix
;;           (setq fill-column
;;                 (- fill-column (* 2 (length prefix)))))
;;       (funcall fun nil)
;;       (goto-char beg)
;;       (insert prefix)
;;       (funcall fun nil))
;;     (goto-char (+ end 2))))

;;;_  - quoting
(defvar quote-string "> "
  "String used for paragraph quoting:
`quote-region', which mapped to \\[quote-region], and,
`quote-reformat', which mapped to \\[quote-reformat] .")

;; (defun quote-reformat ()
;;   "Reformat a paragraph of indented quotation,
;;    using the variable `quote-string'."
;;   (interactive)
;;   (beginning-of-line 1)
;;   (if (looking-at "\n")
;;       (forward-line 1))
;;   (let ((bofp (point)))
;;     (skip-chars-forward quote-string)
;;     (let ((fill-prefix (buffer-substring bofp (point))))
;;       (fill-paragraph nil))))

;; quote-region-pre, quote a region as previous content
(defun quote-region-pre(b e)
  "Quote the mouse selected *lines* using the variable `quote-string'"
  (interactive "r")
  (forward-line -1)
  (setq e1 (point))
  (forward-line)
  (string-rectangle b e1 quote-string)
  )

;; quote-region-3rdp, quote a region as third-party content
(defun quote-region-3rdp(b e)
  "Quote the mouse selected *lines*"
  (interactive "r")
  (forward-line -1)
  (setq e1 (point))
  (forward-line)
  (insert "`-----\n")
  (string-rectangle b e1 "| ")
  (goto-char b)
  (insert ",-----\n")
  (goto-char e)
  (forward-line 2)
  )

;; (bind-key "C-x M-q" 'refill-paragraph)
;; (bind-key "C-x C-r" 'quote-reformat)
;; -- fill-paragraph is good enough
(bind-key "C-x C-b" 'fill-paragraph)
(bind-key "C-x M-q" 'unfill-paragraph)

(bind-key "C-x M-]" 'quote-region-pre)
(bind-key "C-x M-[" 'quote-region-3rdp)

;; from addon-edit00.el
(bind-key "C-x M-p" 'sa-pad-line-ending)

;;;_* Mode-specific-map

;;;_ > C-c ?

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

;;;_   : insert the date & time

;; Functions to insert the date, the time, and the date and time at
;; point.

(defvar insert-date-format "%Y-%m-%d" ;" %a %d %b %Y"
  "*Format for \\[insert-date] (c.f. 'format-time-string' for how to
format).")

(defvar insert-time-format "%H:%M:%S"
  "*Format for \\[insert-time] (c.f. 'format-time-string' for how to
format).")

(defun insert-date ()
  "Insert the current date according to the variable \"insert-date-format\"."
  (interactive)
  (insert (format-time-string insert-date-format)))

(defun insert-time ()
  "Insert the current time according to the variable \"insert-time-format\"."
  (interactive "*")
  (insert (format-time-string insert-time-format (current-time)))
  )

(defun insert-date-and-time ()
  "Insert the current date according to the variable \"insert-date-format\",
then a space, then the current time according to the variable
\"insert-time-format\"."
  (interactive "*")
  (progn
    (insert-date)
    (insert " ")
    (insert-time))
  )


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

(bind-key "C-c C-d C-d" 'insert-date)
(bind-key "C-c C-d C-t" 'insert-time)
(bind-key "C-c C-d C-w" 'insert-date-and-time) ; Insert whole date & time
(bind-key "C-c C-d u" 'insert-user-timestamp)  ; Insert user-timestamp
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

;;;_ > C-c C-?

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;;;_ > C-c M-?

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
          (replace-match " " nil nil nil 1)))))
  (forward-paragraph)			; to blank line
  (forward-line)			; to next paragraph
  )


(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;;_* Ctl-period-map

;;;_ > C-. ?

(bind-key "C-. m" 'kmacro-keymap)

;;;_ > C-. C-i

(bind-key "C-. C-i" 'indent-rigidly)

;;;_* M-o, Outline-minor-mode key map
;; http://www.emacswiki.org/emacs/OutlineMinorMode

(defvar meta-o-map)
(define-prefix-command 'meta-o-map nil "Outline-")
(bind-key "M-o M-o" 'meta-o-map)

; HIDE
(bind-key "M-o M-o q" 'hide-sublevels)    ; Quiet, Hide everything but the top-level headings
(bind-key "M-o M-o t" 'hide-body)         ; Topics, Hide everything but headings (all body lines)
(bind-key "M-o M-o o" 'hide-other)        ; Other, Hide other branches
(bind-key "M-o M-o c" 'hide-entry)        ; Current, Hide this entry's body
(bind-key "M-o M-o l" 'hide-leaves)       ; Leaves, Hide body lines in this entry and sub-entries
(bind-key "M-o M-o d" 'hide-subtree)      ; Down, Hide everything in this entry and sub-entries
; SHOW
(bind-key "M-o M-o a" 'show-all)          ; Show (expand) everything
(bind-key "M-o M-o e" 'show-entry)        ; Show this heading's body
(bind-key "M-o M-o i" 'show-children)     ; Show this heading's immediate child sub-headings
(bind-key "M-o M-o k" 'show-branches)     ; Show all sub-headings under this heading
(bind-key "M-o M-o s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(bind-key "M-o M-o u" 'outline-up-heading)                ; Up
(bind-key "M-o M-o n" 'outline-next-visible-heading)      ; Next
(bind-key "M-o M-o p" 'outline-previous-visible-heading)  ; Previous
(bind-key "M-o M-o f" 'outline-forward-same-level)        ; Forward - same level
(bind-key "M-o M-o b" 'outline-backward-same-level)       ; Backward - same level

;;;_* Help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;;;_ > C-h e ?

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)

(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)


;; End:
