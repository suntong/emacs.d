;; -*- emacs-lisp -*-

;;;_ , lisp-mode

;; Utilities every Emacs Lisp coder should master:
;;
;;   paredit          Lets you manipulate sexps with ease
;;   redshank         Think: Lisp refactoring
;;   edebug           Knowing the traditional debugger is good too
;;   eldoc
;;   cldoc
;;   elint
;;   elp
;;   ert
;;   ielm

(use-package lisp-mode
  ;; :load-path "site-lisp/slime/contrib/"
  :init
  (progn
    (defface esk-paren-face
      '((((class color) (background dark))
         (:foreground "grey50"))
        (((class color) (background light))
         (:foreground "grey55")))
      "Face used to dim parentheses."
      :group 'starter-kit-faces)

    ;; Change lambda to an actual lambda symbol
    (mapc (lambda (major-mode)
            (font-lock-add-keywords
             major-mode
             '(("(\\(lambda\\)\\>"
                (0 (ignore
                    (compose-region (match-beginning 1)
                                    (match-end 1) ?λ))))
               ("(\\|)" . 'esk-paren-face)
               ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
                (1 font-lock-keyword-face)
                (2 font-lock-function-name-face
                 nil t)))))
          lisp-modes)

    (defvar slime-mode nil)
    (defvar lisp-mode-initialized nil)

    (defun initialize-lisp-mode ()
      (unless lisp-mode-initialized
        (setq lisp-mode-initialized t)

        (use-package redshank
          :diminish redshank-mode)

        (use-package elisp-slime-nav
          :diminish elisp-slime-nav-mode)

        (use-package edebug)

        (use-package eldoc
          :diminish eldoc-mode
          :defer t
          :init
          (use-package eldoc-extension
            :disabled t
            :defer t
            :init
            (add-hook 'emacs-lisp-mode-hook
                      #'(lambda () (require 'eldoc-extension)) t))

          :config
          (eldoc-add-command 'paredit-backward-delete
                             'paredit-close-round))

        (use-package cldoc
          :diminish cldoc-mode)

        (use-package ert
          :commands ert-run-tests-interactively
          :bind ("C-c e t" . ert-run-tests-interactively))

        (use-package elint
          :commands 'elint-initialize
          :init
          (defun elint-current-buffer ()
            (interactive)
            (elint-initialize)
            (elint-current-buffer))

          :config
          (progn
            (add-to-list 'elint-standard-variables 'current-prefix-arg)
            (add-to-list 'elint-standard-variables 'command-line-args-left)
            (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
            (add-to-list 'elint-standard-variables 'emacs-major-version)
            (add-to-list 'elint-standard-variables 'window-system)))

        (use-package highlight-cl
          :init
          (mapc (function
                 (lambda (mode-hook)
                   (add-hook mode-hook
                             'highlight-cl-add-font-lock-keywords)))
                lisp-mode-hooks))

        (defun my-elisp-indent-or-complete (&optional arg)
          (interactive "p")
          (call-interactively 'lisp-indent-line)
          (unless (or (looking-back "^\\s-*")
                      (bolp)
                      (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
            (call-interactively 'lisp-complete-symbol)))

        (defun my-lisp-indent-or-complete (&optional arg)
          (interactive "p")
          (if (or (looking-back "^\\s-*") (bolp))
              (call-interactively 'lisp-indent-line)
            (call-interactively 'slime-indent-and-complete-symbol)))

        (defun my-byte-recompile-file ()
          (save-excursion
            (byte-recompile-file buffer-file-name)))

        ;; Register Info manuals related to Lisp
        (use-package info-lookmore
          :init
          (progn
            (info-lookmore-elisp-cl)
            (info-lookmore-elisp-userlast)
            (info-lookmore-elisp-gnus)
            (info-lookmore-apropos-elisp)))

        (mapc (lambda (mode)
                (info-lookup-add-help
                 :mode mode
                 :regexp "[^][()'\" \t\n]+"
                 :ignore-case t
                 :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
              lisp-modes)))

    (defun my-lisp-mode-hook ()
      (initialize-lisp-mode)

      (auto-fill-mode 1)
      (paredit-mode 1)
      (redshank-mode 1)
      (elisp-slime-nav-mode 1)

      (local-set-key (kbd "<return>") 'paredit-newline)

      (add-hook 'after-save-hook 'check-parens nil t)

      (if (memq major-mode
                '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
          (progn
            (bind-key "<M-return>" 'outline-insert-heading emacs-lisp-mode-map)
            (bind-key "<tab>" 'my-elisp-indent-or-complete emacs-lisp-mode-map))
        (turn-on-cldoc-mode)

        (bind-key "<tab>" 'my-lisp-indent-or-complete lisp-mode-map)
        (bind-key "M-q" 'slime-reindent-defun lisp-mode-map)
        (bind-key "M-l" 'slime-selector lisp-mode-map))

      (yas-minor-mode 1))

    (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)))

