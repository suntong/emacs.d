;; -*- emacs-lisp -*-

;;;_ , magit

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :init
  (progn
    (defun magit-status-with-prefix ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'magit-status)))

    (defun eshell/git (&rest args)
      (cond
       ((or (null args)
            (and (string= (car args) "status") (null (cdr args))))
        (magit-status default-directory))
       ((and (string= (car args) "log") (null (cdr args)))
        (magit-log))
       (t (throw 'eshell-replace-command
                 (eshell-parse-command
                  (concat "*" command)
                  (eshell-stringify-list (eshell-flatten-list args)))))))

    (add-hook 'magit-mode-hook 'hl-line-mode))

  :config
  (progn
    (setenv "GIT_PAGER" "")

    (use-package magit-review
      :disabled t
      :commands magit-review
      :config (require 'json))

    (unbind-key "M-h" magit-mode-map)
    (unbind-key "M-s" magit-mode-map)

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))

    ;; (require 'magit-topgit)
    ;; (require 'rebase-mode)

    (defvar magit-git-monitor-process nil)
    (make-variable-buffer-local 'magit-git-monitor-process)

    (defun start-git-monitor ()
      (interactive)
      (unless magit-git-monitor-process
        (setq magit-git-monitor-process
              (start-process "git-monitor" (current-buffer) "git-monitor"
                             "-d" (expand-file-name default-directory)))))

    ;; (add-hook 'magit-status-mode-hook 'start-git-monitor)
    ))

