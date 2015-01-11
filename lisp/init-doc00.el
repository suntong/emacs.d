;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is text doc handling init.
;; It depends heavily on `use-package' by John Wiegley.

;;; Code:

;;;_* markdown-mode
(use-package markdown-mode
  :mode ("\\.\\(txt\\|markdown\\|md\\)\\(\\'\\|\\.\\)" . gfm-mode)
  ;; A [GitHub Flavored Markdown] (GFM) mode, `gfm-mode', is also
  ;; available as part of markdown-mode. The most important differences
  ;; are:
  ;;
  ;; - GFM code blocks, with optional programming language keywords,
  ;;   will be highlighted. This is most important to me as the default
  ;;   markdown-mode will not highlight them.
  ;;
  ;; - Underscores inside of words (such as test_variable) will not
  ;;   trigger emphasis.

;;;_ > markdown-toc
  :init
  (progn
    (use-package markdown-toc
      :bind (("C-c C-x T" . markdown-toc/generate-toc)))
    ;; It will compute the TOC at insert it at current position, but
    ;; work only for github since those toc anchors are not inserted
    ;; by default anywhere else.
    )
  :config  ; runs after the mode is loaded
  (progn (add-hook 'markdown-mode-hook 'flyspell-mode))
  )


;;;_* others
(load-library "init-doc00-doc-mode") 

;;;_* End
