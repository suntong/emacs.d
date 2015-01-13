;; -*- emacs-lisp -*-

;;;_ , ruby-mode

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :config
  (progn
    (use-package yari
      :init
      (progn
        (defvar yari-helm-source-ri-pages
          '((name . "RI documentation")
            (candidates . (lambda () (yari-ruby-obarray)))
            (action  ("Show with Yari" . yari))
            (candidate-number-limit . 300)
            (requires-pattern . 2)
            "Source for completing RI documentation."))

        (defun helm-yari (&optional rehash)
          (interactive (list current-prefix-arg))
          (when current-prefix-arg (yari-ruby-obarray rehash))
          (helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))

    (defun my-ruby-smart-return ()
      (interactive)
      (when (memq (char-after) '(?\| ?\" ?\'))
        (forward-char))
      (call-interactively 'newline-and-indent))
    (defun my-ruby-mode-hook ()
      (require 'inf-ruby)
      (inf-ruby-keys)
      (bind-key "<return>" 'my-ruby-smart-return ruby-mode-map)
      (bind-key "C-h C-i" 'helm-yari ruby-mode-map)
      (set (make-local-variable 'yas-fallback-behavior)
           '(apply ruby-indent-command . nil))
      (bind-key "<tab>" 'yas-expand-from-trigger-key ruby-mode-map))
    (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))
