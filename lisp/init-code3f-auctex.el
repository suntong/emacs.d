;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is auctex init.

;;; Code:

;;;_ , auctex

(use-package tex-site
  ;; :load-path "site-lisp/auctex/preview/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (progn
    (defun latex-help-get-cmd-alist ()  ;corrected version:
      "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
      ;; mm, does it contain any cached entries
      (if (not (assoc "\\begin" latex-help-cmd-alist))
          (save-window-excursion
            (setq latex-help-cmd-alist nil)
            (Info-goto-node (concat latex-help-file "Command Index"))
            (goto-char (point-max))
            (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
              (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                    (value (buffer-substring (match-beginning 2)
                                             (match-end 2))))
                (add-to-list 'latex-help-cmd-alist (cons key value))))))
      latex-help-cmd-alist)

    (use-package latex-mode
      :defer t
      :config
      (progn
        (use-package preview)
        (use-package ac-math)

        (defun ac-latex-mode-setup ()
          (nconc ac-sources
                 '(ac-source-math-unicode ac-source-math-latex
                                          ac-source-latex-commands)))

        (add-to-list 'ac-modes 'latex-mode)
        (add-hook 'latex-mode-hook 'ac-latex-mode-setup)

        (info-lookup-add-help :mode 'latex-mode
                              :regexp ".*"
                              :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                              :doc-spec '(("(latex2e)Concept Index" )
                                          ("(latex2e)Command Index")))))))

;; Local Variables:
;;   mode: emacs-lisp
;;   mode: allout
;;   outline-regexp: "^;;;\\([*]+\\)"
;; End:
