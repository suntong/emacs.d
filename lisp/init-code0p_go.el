;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>
;; based on
;; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/

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
			  "goimports")
                         (t "gofmt")))
    ;; Call Gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)

    ;; -- 
    
    ;; (use-package go-stacktracer
    ;;   :commands (go-stacktracer-region))

    ;; (use-package go-eldoc
    ;;   :init
    ;;   (progn
    ;;     (add-hook 'go-mode-hook 'go-eldoc-setup)))

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

      ;; (bind-key "M-." 'godef-jump go-mode-map)
      )
    )
  )

;; End:
