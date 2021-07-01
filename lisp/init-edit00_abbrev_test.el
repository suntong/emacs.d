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
      ("p" "print(▮)" ahf)
      ("`foo" "bar")
      ("`p" "print()" ahf)
      ;;

      ))
  (abbrev-table-put python-mode-abbrev-table :regexp my-abbrev-regexp)
  (add-hook 'python-mode-hook #'abbrev-mode)
  )

;; == adapted from http://ergoemacs.org/emacs/elisp_abbrev.html

;;  the “ahf” stand for abbrev hook function.
(defun ahf ()
  "Abbrev hook function, used for `define-abbrev'.
prevent inserting the char that triggered expansion."
  (interactive)
  t)

(put 'ahf 'no-self-insert t)

(setq abbrev-expand-function 'global-expand-abbrev)
;; (setq abbrev-expand-function 'abbrev--default-expand)

(defun global-expand-abbrev ()
  "Function for value of `abbrev-expand-function'.
Expand the symbol before cursor.
Returns the abbrev symbol if there's a expansion, else nil."
  (interactive)
  (let ( $p1 $p2
             $abrStr
             $abrSymbol
             )

    (save-excursion
      (forward-symbol -1)
      (setq $p1 (point))
      (forward-symbol 1)
      (setq $p2 (point)))

    (setq $abrStr (buffer-substring-no-properties $p1 $p2))
    (setq $abrSymbol (abbrev-symbol $abrStr))
    (if $abrSymbol
        (progn
          (abbrev-insert $abrSymbol $abrStr $p1 $p2 )
          (global-abbrev-position-cursor $p1)
          $abrSymbol)
      nil)))

(defun global-abbrev-position-cursor (&optional @pos)
  "Move cursor (from @POS) back to ▮ if exist, else put at end.
Return true if found, else false."
  (interactive)
  (let (($found-p (search-backward "▮" (if @pos @pos (max (point-min) (- (point) 100))) t )))
    (when $found-p (delete-char 1))
    $found-p
    ))

(setq save-abbrevs nil)
