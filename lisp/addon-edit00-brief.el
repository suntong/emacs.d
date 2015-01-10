;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is addon to extend emacs editing features copied from my old brief mode. 

;;; Code:

;;;_* brief-split{-to-next-line
;;--------------------------------------------------------------------
(defun brief-split{-to-next-line()
  "Move all trailing '{' to the next line in the whole buffer."
; Credit: Christoph Conrad
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "\\(.*\\){[ \t]*$" nil t)
        (when (null (string-match "^[ \t]*$"  (match-string 1)))
          (replace-match "\\1" nil nil)
          (reindent-then-newline-and-indent)
          (insert "{"))))))

;;;_* brief-join{-with-previous-line
;;--------------------------------------------------------------------
(defun brief-join{-with-previous-line()
  "Join all separate '{' to the previous line in the whole buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*{[ \t]*$" nil t)
          (delete-indentation)
      ))))

;;;_* brief-spaces-before-if-parens
;;--------------------------------------------------------------------
(defun brief-spaces-before-if-parens nil
  "adds spaces, so if(thing, other); becomes if (thing, other);"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "\\(if\\|while\\)(" nil t)
	  (replace-match "\\1 ("))))

;;;_* brief-spaces-around-parens
;;--------------------------------------------------------------------
(defun brief-spaces-around-parens nil
  "adds spaces, so foo.bar(thing, other); becomes foo.bar( thing, other );"
; Credit: Edric M Ellis
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "(\\([^ )]\\)" nil t)
	  (replace-match "( \\1"))
	(goto-char (point-min))
	(while (re-search-forward "\\([^ (]\\))" nil t)
	  (replace-match "\\1 )"))))

;;;_* brief-nospaces-around-parens
;;--------------------------------------------------------------------
(defun brief-nospaces-around-parens nil
  "remove spaces, so foo.bar ( thing, other ); becomes foo.bar(thing, other);"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "( " nil t)
	  (replace-match "("))
	(goto-char (point-min))
	(while (re-search-forward " (" nil t)
	  (replace-match "("))
	(goto-char (point-min))
	(while (re-search-forward " )" nil t)
	  (replace-match ")"))))

;;;_* Keybindings
;;--------------------------------------------------------------------

;; C-c C-f -- Control Flow. brace, if, parens
(bind-key "C-c C-f C-b" 'brief-join{-with-previous-line)
(bind-key "C-c C-f C-i" 'brief-spaces-before-if-parens)
(bind-key "C-c C-f C-p" 'brief-nospaces-around-parens)

;;;_* End
