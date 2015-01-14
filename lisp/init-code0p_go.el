;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is go-mode initialization.

;;; Code:

(use-package go-mode
  :mode "\\.go\\(\\'\\|\\.\\)"

  :config
  (progn

    (defun gopath-set-here ()
      (interactive)
      (message (or (buffer-file-name) default-directory))
      (setenv "GOPATH"
              (f-expand (or (buffer-file-name) default-directory))))

    ;; -- Use the style of `gofmt -tabs=false -tabwidth=2` to format code
    (add-hook 'go-mode-hook (lambda ()
			      (setq tab-width 2)
			      (setq indent-tabs-mode nil) ))
    (setq gofmt-command (cond
                         ((executable-find "goimports")
			  "goimports -tabs=false -tabwidth=2")
                         (t "gofmt -tabs=false -tabwidth=2")))
    ;; -- 
    
    ;; (use-package go-stacktracer
    ;;   :commands (go-stacktracer-region))

    (use-package go-eldoc
      :init
      (progn
        (add-hook 'go-mode-hook 'go-eldoc-setup)))

    (use-package go-autocomplete
      :pre-load
      (progn
        (use-package auto-complete))
      ;; :init
      ;; (progn
      ;;   (add-hook 'go-mode-hook
      ;;             #'(lambda ()
      ;;                 (setq ac-sources
      ;;                       '(ac-source-go
      ;;                         ac-source-yasnippet))))
      ;; 	))

      (bind-key "M-." 'godef-jump go-mode-map))
  )

;; End:
