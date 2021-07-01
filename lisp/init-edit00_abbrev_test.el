;; https://emacs.stackexchange.com/questions/66410/different-abbrevs-for-each-major-mode

(defvar my-abbrev-regexp
  (rx (or bol (not (any "`" wordchar)))
      (group (one-or-more (any "`" wordchar)))
      (zero-or-more (not (any "`" wordchar))))
  "Use as :regexp in abbrev tables to make \\=` a valid abbrev char.

If `words-include-escapes' is used then this regexp can fail.
Refer to the elisp comments in `abbrev--before-point' for details.")

(abbrev-table-put emacs-lisp-mode-abbrev-table :regexp my-abbrev-regexp)
(define-abbrev emacs-lisp-mode-abbrev-table "`foo" "bar")
(add-hook 'emacs-lisp-mode-hook #'abbrev-mode)

;; use it with a mode which is not loaded by default
(with-eval-after-load "sh-script"
  (abbrev-table-put sh-mode-abbrev-table :regexp my-abbrev-regexp)
  (define-abbrev sh-mode-abbrev-table "`foo" "bar")
  (add-hook 'sh-mode-hook #'abbrev-mode))

(progn
  ;; python
  (when (boundp 'python-mode-abbrev-table)
    (clear-abbrev-table python-mode-abbrev-table))

  (define-abbrev-table 'python-mode-abbrev-table
    '(

      ("fo" "bar")
      ("`foo" "bar")
      ("`p" "print()" ahf)
      ;;

      ))
  (abbrev-table-put python-mode-abbrev-table :regexp my-abbrev-regexp)
  (add-hook 'python-mode-hook #'abbrev-mode)
  )

;;  the “ahf” stand for abbrev hook function.
(defun ahf ()
  "Abbrev hook function, used for `define-abbrev'.
prevent inserting the char that triggered expansion."
  (interactive)
  t)

(setq save-abbrevs nil)
