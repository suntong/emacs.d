;;; init-edit00_abbrev --- abbrev package init
;; -*- emacs-lisp -*-

;; Copyright (C) 2015-2021 Tong Sun
;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is init-edit00_abbrev --- abbrev package init.
;; Adapted from 〈Using Emacs Abbrev Mode for Abbreviation〉
;; http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;;; Code:

(eval-when-compile
  (require 'use-package))

;;;_* abbrev
(use-package abbrev
  :defer t
  :disabled t
  :commands abbrev-mode
  :diminish abbrev-mode
  :init
  (hook-into-modes #'abbrev-mode '(text-mode-hook))

  :config
  (progn
   (if (file-exists-p abbrev-file-name)
       (quietly-read-abbrev-file))

   (add-hook 'expand-load-hook
             (lambda ()
               (add-hook 'expand-expand-hook 'indent-according-to-mode)
               (add-hook 'expand-jump-hook 'indent-according-to-mode)))))


(defvar extended-abbrev-regexp
  "\\(`[0-9A-Za-z._-]+\\)"
  "Use as :regexp in abbrev tables to make \\=` a valid abbrev char.

If making \\=` optional (suffix it with ?), `re-search-backward' will
not be able to be that aggressive to match to it.  Thus, making
the leading \\=` mandatory.

If `words-include-escapes' is used then this regexp can fail.
Refer to the elisp comments in `abbrev--before-point' for details.")

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; symbols
    ("`bu" "•" ahf)
    ("`la" "←" ahf)
    ("`ua" "↑" ahf)
    ("`ra" "→" ahf)
    ("`da" "↓" ahf)
    ("`fn" "ƒ" ahf)

    ;; code
    ("`rt" "return")

    ;; personal

    ;;

    ))



(progn
  ;; powershell
  (when (boundp 'powershell-mode-abbrev-table)
    (clear-abbrev-table powershell-mode-abbrev-table))

  (define-abbrev-table 'powershell-mode-abbrev-table
    '(
      ("`if" "if (3 -gt 2) { 3 }")
      ("`els" "else { 4 }")
      ("`iexp" "( $x -gt 5 ? 3 : 4)")

      ("`frch" "foreach ($e in $arr) { $e }")

      ("`whl" "$i=5; while($i -lt 10) { $a[$i] }")
      ("`frl" "for ($i=0; $i -le $arr.length; $i=$i+1) { $arr[$i] }")

      ;;
      )))

(progn
  ;; golang
  (when (boundp 'go-mode-abbrev-table)
    (clear-abbrev-table go-mode-abbrev-table))

  (define-abbrev-table 'go-mode-abbrev-table
    '(
      ("`gmn" "package main\n\nimport \"fmt\"\n\nfunc main() {\n\n	fmt.Printf(\"%v\\n\", ▮)\n\n}" ahf) ; go main

      ("`c" "const ▮ = 3" ahf)
      ("`dv" "var ▮ = " ahf)		; def v
      ("`v" "▮ := 3" ahf)

      ("`pf" "fmt.Printf(\"%v\\n\", ▮)" ahf)
      ("`pl" "fmt.Println(▮)" ahf)
      ("`spf" "fmt.Sprintf(\"%v\", ▮)" ahf)

      ("`df" "func ▮(x int) int {\n	return nil\n}" ahf) ; def func
      ("`if" "if ▮ { 3 }" ahf)
      ("`ie" " if err != nil { panic(err) }")
      ("`ei" "else if ▮ > 0 { 3 }")
      ("`els" "else { ▮ }" ahf)
      ("`frl" "for ii := 0; ii < 4; ii++ { ▮ii }" ahf) ; for loop
      ("`fr" "for key, val := range xxx {
▮
    }
" ahf)					; for range
      ("`cmb" "/* \n▮\n*/" ahf)		; comment block
      ("`ft" "fallthrough" ahf)
      ("`stru" "type myS struct {\ny string\nx int\n}" ahf)
      ("`swtc" "	switch 3 {\n	case 1:\n		fmt.Println( 3 )\n	case 2, 3:\n		fmt.Println( 4 )\n	default:\n		fmt.Println( 5 )\n	}" ahf)

      ("`sl" "var ▮ = []int{1,2}" ahf)	; slice
      ("`ms" "var ▮ = map[string]string{`a`: `1`, `b`: `2`}" ahf) ; map str
      ("`mb" "var ▮ = make([]byte, 0, 9)" ahf)
      ("`mm" "var ▮ = make(map[string]int)" ahf)
      ("`ln" "len(▮)" ahf)
      ("`rmc" "regexp.MustCompile(`▮`)" ahf)
      ("`rfa" "re.FindAll(▮, -1)" ahf)

      ;;

      ))
  (abbrev-table-put go-mode-abbrev-table :regexp extended-abbrev-regexp)
  ;; (add-hook 'go-mode-hook #'abbrev-mode)
  )

(progn
  ;; python
  (when (boundp 'python-mode-abbrev-table)
    (clear-abbrev-table python-mode-abbrev-table))

  (define-abbrev-table 'python-mode-abbrev-table
    '(

      ("`foo" "bar")
      ("`p" "print(▮)" ahf)
      ;;

      ))
  (abbrev-table-put python-mode-abbrev-table :regexp extended-abbrev-regexp)
  )

(set-default 'abbrev-mode t)

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
      ;; (forward-symbol -1)
      (re-search-backward extended-abbrev-regexp nil t)
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

(provide 'init-edit00_abbrev)
;;; init-edit00_abbrev.el ends here
