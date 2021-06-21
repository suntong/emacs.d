;; -*- emacs-lisp -*-

;; Copyright (C) 2015-2021 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is init-edit00, using general editing packages.
;; It depends heavily on `use-package' by John Wiegley.

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

;;;_* autorevert
(use-package autorevert
  :defer t
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook
            #'(lambda ()
                (auto-revert-mode 1))))

;;;_* flycheck
(use-package flycheck
  :defer t
  :ensure t
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  :init (global-flycheck-mode))

;;;_* flyspell
(use-package ispell
  :defer t
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package flyspell
  :defer t
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (define-key flyspell-mode-map [(control ?.)] nil))

;;;_* generic-x
(require 'generic-x)
;; It will add syntax highlighting for batch files, ini files, command files,
;; registry files, apache files, samba files, resource files, fvwm files, etc.

;;;_* hi-lock
(use-package hi-lock
  :defer t
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))


;;;_* popup-ruler
(use-package popup-ruler
  :defer t
  :bind (("C-. r" . popup-ruler)
         ("C-. R" . popup-ruler-vertical)))


;;;_* rectangle-mark-mode
;; In the rectangle-mark-mode, introduced in Emacs 24.4 and later
;;
;; - There are two ways to erase the text in a rectangle: C-x r d
;;   (delete-rectangle) to delete the text outright, or C-x r k
;;   (kill-rectangle) to remove the text and save it as the last
;;   killed rectangle.
;;
;; - C-x r M-w (copy-rectangle-as-kill) is the equivalent of M-w for
;;   rectangles: it records the rectangle as the "last killed
;;   rectangle", without deleting the text from the buffer.
;;
;; - To yank the last killed rectangle, type C-x r y (yank-rectangle).
;;
;; To use mouse to select column,
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))
;; Mouse based rectangle highlighting by dragging the mouse while holding down the shift key
(global-set-key (kbd "S-<down-mouse-1>") #'mouse-start-rectangle)
;; From PythonNut, http://emacs.stackexchange.com/questions/7244/

;;;_* session
;; https://github.com/ronert/.emacs.d/blob/master/lisp/init-sessions.el#L29
(use-package session
  :ensure t
  ;:pin melpa-stable
  :config
  (progn
    (setq session-save-file (expand-file-name "~/.emacs.d/.session"))
    (add-hook 'after-init-hook 'session-initialize)

    ;; save a bunch of variables to the desktop file
    ;; for lists specify the len of the maximal saved data also
    (setq desktop-globals-to-save
          (append '((extended-command-history . 30)
                    (file-name-history        . 100)
                    (grep-history             . 30)
                    (compile-history          . 30)
                    (minibuffer-history       . 50)
                    (query-replace-history    . 60)
                    (read-expression-history  . 60)
                    (regexp-history           . 60)
                    (regexp-search-ring       . 20)
                    (search-ring              . 20)
                    (comint-input-ring        . 50)
                    (shell-command-history    . 50)
                    desktop-missing-file-warning
                    tags-file-name
                    register-alist)))
    ))


;;;_* show-paren-mode
(show-paren-mode 1)
;; C-M-f Move forward over a balanced expression
;; C-M-b Move backward over a balanced expression
;; C-M-k Kill balanced expression forward
;; C-M-SPC put the mark at the end of the sexp.
;; C-M-n Move forward over a parenthetical group
;; C-M-p Move backward over a parenthetical group
;; Or Use C-M-right and C-M-left to go to the beginning or the end of the current expression.

;;;_* smartparens
(use-package smartparens
  :defer t
  :commands (smartparens-mode show-smartparens-mode)
  :config (require 'smartparens-config))

;;;_* undo-tree
;; http://www.emacswiki.org/emacs/RedoMode
(use-package undo-tree
  :defer t
  :init (progn
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (global-undo-tree-mode)
    )
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c u l" . undo-tree-switch-branch)
         ("C-c u '" . undo-tree-visualize)) )

;;;_* visual-line-mode
;; The problem with visual-line mode is that by default, there are no
;; indicators showing where the line is wrapped. To fix this,
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; You need to put the assignment before the global-visual-line-mode call
;; From Daniel Neal, http://stackoverflow.com/questions/18268741/

;;;_* wrap-region
(use-package wrap-region
  :defer t
  :commands wrap-region-mode
  :diminish wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode c-mode c++-mode))
     ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))


;;;_* others
;(load-library "init-edit01_yasnippet") 

;; End:
