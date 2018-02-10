;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;; Commentary:

;; This is for the primary programming languages that I use.
;; It depends heavily on `use-package' by John Wiegley.

;;; Code:

;;;_* diff-mode

(use-package diff-mode
  :commands diff-mode
  )

;;;_* ediff

(use-package ediff
  :pre-init
  (progn
    (defvar ctl-period-equals-map)
    (define-prefix-command 'ctl-period-equals-map)
    (bind-key "C-. =" 'ctl-period-equals-map)

    (bind-key "C-. = c" 'compare-windows)) ; not an ediff command, but it fits

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  :config
  (use-package ediff-keep))


;;;_* cc-mode

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :init
  (progn
    (defun my-paste-as-check ()
      (interactive)
      (save-excursion
        (insert "/*\n")
        (let ((beg (point)) end)
          (yank)
          (setq end (point-marker))
          (goto-char beg)
          (while (< (point) end)
            (forward-char 2)
            (insert "CHECK: ")
            (forward-line 1)))
        (insert "*/\n")))

    ;; (defun my-c-indent-or-complete ()
    ;;   (interactive)
    ;;   (let ((class (syntax-class (syntax-after (1- (point))))))
    ;;     (if (or (bolp) (and (/= 2 class)
    ;;                         (/= 3 class)))
    ;;         (call-interactively 'indent-according-to-mode)
    ;;       (call-interactively 'auto-complete))))

    (defvar printf-index 0)

    (defun insert-counting-printf (arg)
      (interactive "P")
      (if arg
          (setq printf-index 0))
      (if t
          (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                          (setq printf-index (1+ printf-index))))
        (insert (format "printf(\"step %d..\\n\");\n"
                        (setq printf-index (1+ printf-index)))))
      (forward-line -1)
      (indent-according-to-mode)
      (forward-line))

    (defun my-c-mode-common-hook ()
      (abbrev-mode 1)
      (gtags-mode 1)
      (hs-minor-mode 1)
      (hide-ifdef-mode 1)
      (whitespace-mode 1)
      (which-function-mode 1)
      ;; (auto-complete-mode 1)
      (yas-minor-mode 1)
      (bug-reference-prog-mode 1)

      (diminish 'gtags-mode)
      (diminish 'hs-minor-mode)
      (diminish 'hide-ifdef-mode)

      (bind-key "C-c p" 'insert-counting-printf c-mode-base-map)

      ;; (setq ac-sources (list (if (and (fboundp 'semantic-active-p)
      ;;                                 (funcall #'semantic-active-p))
      ;;                            'ac-source-semantic
      ;;                          'ac-source-gtags)))
      ;; (bind-key "<A-tab>" 'ac-complete c-mode-base-map)

      ;;(doxymacs-mode 1)
      ;;(doxymacs-font-lock)

      (bind-key "<return>" 'newline-and-indent c-mode-base-map)

      ;; (set (make-local-variable 'yas-fallback-behavior)
      ;;      '(apply my-c-indent-or-complete . nil))
      ;; (bind-key "<tab>" 'yas-expand-from-trigger-key c-mode-base-map)

      (unbind-key "M-j" c-mode-base-map)
      (bind-key "C-c C-i" 'c-includes-current-file c-mode-base-map)
      (bind-key "C-c C-y" 'my-paste-as-check c-mode-base-map)

      (set (make-local-variable 'parens-require-spaces) nil)
      (setq indicate-empty-lines t)
      (setq fill-column 72)

      (bind-key "M-q" 'c-fill-paragraph c-mode-base-map)

      (let ((bufname (buffer-file-name)))
        (when bufname
          (cond
           ((string-match "/ledger/" bufname)
            (c-set-style "ledger"))
           ((string-match "/ansi/" bufname)
            (c-set-style "ti")
            (substitute-key-definition 'fill-paragraph 'ti-refill-comment
                                       c-mode-base-map global-map)
            (bind-key "M-q" 'ti-refill-comment c-mode-base-map))
           ((string-match "/edg/" bufname)
            (c-set-style "edg"))
           (t
            (c-set-style "clang")))))

      (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|DEBUG\\)("
                                           1 font-lock-warning-face t))))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

  :config
  (progn
    (setq c-syntactic-indentation nil)

    (bind-key "#" 'self-insert-command c-mode-base-map)
    (bind-key "{" 'self-insert-command c-mode-base-map)
    (bind-key "}" 'self-insert-command c-mode-base-map)
    (bind-key "/" 'self-insert-command c-mode-base-map)
    (bind-key "*" 'self-insert-command c-mode-base-map)
    (bind-key ";" 'self-insert-command c-mode-base-map)
    (bind-key "," 'self-insert-command c-mode-base-map)
    (bind-key ":" 'self-insert-command c-mode-base-map)
    (bind-key "(" 'self-insert-command c-mode-base-map)
    (bind-key ")" 'self-insert-command c-mode-base-map)
    (bind-key "<" 'self-insert-command c++-mode-map)
    (bind-key ">" 'self-insert-command c++-mode-map)

    (use-package cedet
      :disabled t
      :init
      (progn
        ;; Add further minor-modes to be enabled by semantic-mode.  See
        ;; doc-string of `semantic-default-submodes' for other things you can
        ;; use here.
        (dolist (submode '(global-semantic-idle-summary-mode
                           global-semantic-mru-bookmark-mode
                           global-semantic-idle-local-symbol-highlight-mode
                           global-semantic-show-unmatched-syntax-mode))
          (add-to-list 'semantic-default-submodes submode t))

        ;; Enable Semantic
        (semantic-mode 1)

        (when nil              ; jww (2012-06-20): this kills buffers
          ;; if you want to enable support for gnu global
          (use-package semanticdb-global)

          (semanticdb-enable-gnu-global-databases 'c-mode)
          (semanticdb-enable-gnu-global-databases 'c++-mode))))

    (add-to-list 'c-style-alist
                 '("ti"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 3)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . +)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . c-lineup-arglist-intro-after-paren)
                       (arglist-close . c-lineup-arglist)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("edg"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . +)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("ledger"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . 0)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("clang"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . 0)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (defun opencl ()
      (interactive)
      (find-file "~/src/ansi/opencl.c")
      (find-file-noselect "~/Contracts/TI/bugslayer/cl_0603/cl_0603.c")
      (find-file-noselect "~/Contracts/TI/bugslayer")
      (magit-status "~/src/ansi")
      (gud-gdb "gdb --fullname ~/Contracts/TI/bin/c60/acpia6x"))

    (defun ti-refill-comment ()
      (interactive)
      (let ((here (point)))
        (goto-char (line-beginning-position))
        (let ((begin (point)) end
              (marker ?-) (marker-re "\\(-----\\|\\*\\*\\*\\*\\*\\)")
              (leader-width 0))
          (unless (looking-at "[ \t]*/\\*[-* ]")
            (search-backward "/*")
            (goto-char (line-beginning-position)))
          (unless (looking-at "[ \t]*/\\*[-* ]")
            (error "Not in a comment"))
          (while (and (looking-at "\\([ \t]*\\)/\\* ")
                      (setq leader-width (length (match-string 1)))
                      (not (looking-at (concat "[ \t]*/\\*" marker-re))))
            (forward-line -1)
            (setq begin (point)))
          (when (looking-at (concat "[^\n]+?" marker-re "\\*/[ \t]*$"))
            (setq marker (if (string= (match-string 1) "-----") ?- ?*))
            (forward-line))
          (while (and (looking-at "[^\n]+?\\*/[ \t]*$")
                      (not (looking-at (concat "[^\n]+?" marker-re
                                               "\\*/[ \t]*$"))))
            (forward-line))
          (when (looking-at (concat "[^\n]+?" marker-re "\\*/[ \t]*$"))
            (forward-line))
          (setq end (point))
          (let ((comment (buffer-substring-no-properties begin end)))
            (with-temp-buffer
              (insert comment)
              (goto-char (point-min))
              (flush-lines (concat "^[ \t]*/\\*" marker-re "[-*]+\\*/[ \t]*$"))
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*/\\* ?" nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0)))
              (goto-char (point-min))
              (while (re-search-forward "[ \t]*\\*/[ \t]*$" nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0)))
              (goto-char (point-min)) (delete-trailing-whitespace)
              (goto-char (point-min)) (flush-lines "^$")
              (set-fill-column (- 80    ; width of the text
                                  6     ; width of "/*  */"
                                  leader-width))
              (goto-char (point-min)) (fill-paragraph nil)
              (goto-char (point-min))
              (while (not (eobp))
                (insert (make-string leader-width ? ) "/* ")
                (goto-char (line-end-position))
                (insert (make-string (- 80 3 (current-column)) ? ) " */")
                (forward-line))
              (goto-char (point-min))
              (insert (make-string leader-width ? )
                      "/*" (make-string (- 80 4 leader-width) marker) "*/\n")
              (goto-char (point-max))
              (insert (make-string leader-width ? )
                      "/*" (make-string (- 80 4 leader-width) marker) "*/\n")
              (setq comment (buffer-string)))
            (goto-char begin)
            (delete-region begin end)
            (insert comment)))
        (goto-char here)))))

;;;_* js, based on cc-mode

(use-package js
  :defer t
  :init
  (progn
    (setq
     js-indent-level 2
     tab-width 2
     indent-tabs-mode nil))
  ;; :config
  ;; (progn
  ;;   (defun my-javascript-mode-hook ()
  ;;     (setq
  ;;      indent-tabs-mode nil
  ;;      tab-width 2
  ;;      js-indent-level 2
  ;;      ))
  ;;   (add-hook 'javascript-mode-hook 'my-javascript-mode-hook))
  )

;;;_* cperl-mode
;; http://www.emacswiki.org/emacs/CPerlModeOutlineMode

(use-package cperl-mode
  :commands cperl-mode
  :mode ("\\.p[lm]$" . cperl-mode)
  :init
  (progn
    (mapc (lambda (pair)
            (if (eq (cdr pair) 'perl-mode)
                (setcdr pair 'cperl-mode)))
          (append auto-mode-alist interpreter-mode-alist))

    (add-hook 'cperl-mode-hook
              (lambda ()
		"cperl-mode customizations, must be done after cperl-mode loads"

                (outline-minor-mode t)
                ;;(which-function-mode t)
                (eval-when-compile (require 'cperl-mode))
                (abbrev-mode)
		
		(defun cperl-outline-level ()
		  (looking-at outline-regexp)
		  (let ((match (match-string 1)))
		    (cond
		     ((eq match "=head1" ) 1)
		     ((eq match "package") 2)
		     ((eq match "=head2" ) 3)
		     ((eq match "=item"  ) 4)
		     ((eq match "sub"    ) 5)
		     (t 7)
		     )))

                (setq
                 indent-tabs-mode nil
                 ;;cperl-hairy t
                 cperl-indent-level 4
                 cperl-brace-offset 0
                 cperl-continued-brace-offset -4
                 cperl-continued-statement-offset 4
                 cperl-label-offset -4
                 cperl-indent-parens-as-block t
                 cperl-close-paren-offset -4
                 cperl-invalid-face nil
                 cperl-electric-parens nil
                 cperl-electric-keywords t
		 my-cperl-outline-regexp
		 (concat
		  "^"                              ; Start of line
		  "[ \\t]*"                        ; Skip leading whitespace
		  "\\("                            ; begin capture group \1
		  (join "\\|"
			"=head[12]"                  ;     POD header
			"package"                    ;     package
			"=item"                      ;     POD item
			"sub"                        ;     subroutine definition
			"if" "else" "unless" "while" "until" "return"
			)
		  "\\)"                            ; end capture group \1
		  "\\b"                            ; Word boundary
		  )
		 cperl-outline-regexp  my-cperl-outline-regexp
		 outline-regexp        cperl-outline-regexp
		 outline-level        'cperl-outline-level
		 )

                (modify-syntax-entry ?_ "w" cperl-mode-syntax-table)))))


;;;_* sh-script

(use-package sh-script
  :defer t
  :init
  (progn
    (setq ;; sh-mode.el
     sh-basic-offset 2
     sh-indentation 2))
  :config
  (progn
    (defvar sh-script-initialized nil)
    (defun initialize-sh-script ()
      (unless sh-script-initialized
        (setq sh-script-initialized t)
        (info-lookup-add-help :mode 'shell-script-mode
                              :regexp ".*"
                              :doc-spec
                              '(("(bash)Index")))))
    (add-hook 'shell-mode-hook 'initialize-sh-script))  )

;; shell-mode indentation
(setq sh-basic-offset 2)
(setq sh-indent-for-case-label 0)
(setq sh-indent-for-case-alt 2)
(setq sh-indent-for-continuation 2)
(setq sh-indent-for-done 0)
;(setq sh-indent-after-open 2)
;(setq sh-indent-after-loop-construct 2)
(setq sh-indent-comment t)

;;;_* apropos
(setq ;; apropos.el
 apropos-do-all t)

;;;_* font locking
;; (setq font-lock-global-modes '(not web-mode))
  (setq font-lock-maximum-decoration t)
  ;; (global-font-lock-mode t)

;;;_* further customizations
(load-library "init-code0p_go")
;(load-library "init-code3p_lisp")
;(load-library "init-code3p_python")
;(load-library "init-code3p_ruby")

;;;_* End:
