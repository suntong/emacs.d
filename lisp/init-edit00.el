;; -*- emacs-lisp -*-

;; Copyright (C) 2015 Tong Sun

;; Author: Tong Sun <suntong001@users.sourceforge.net>

;;; Commentary:

;; This is init-edit00, using general editing packages.
;; It depends heavily on `use-package' by John Wiegley.

;;; Code:

;;;_* abbrev
(use-package abbrev
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

;;;_* allout

;; allout hot-spot keys and activities
;;
;; p allout-previous-visible-heading
;; u allout-up-current-level
;;
;; f allout-forward-current-level
;; b allout-backward-current-level
;;
;; a allout-beginning-of-current-entry
;; e allout-end-of-entry
;;
;; i allout-show-children
;; s allout-show-current-subtree
;; t allout-toggle-current-subtree-exposure

(use-package allout
  :config
  (progn
    (defvar allout-unprefixed-keybindings nil)

    (defun my-allout-mode-hook ()
      (dolist
	  (mapping
	   '(
	     (?t . allout-hide-bodies)    ; Topics, Hide everything but headings (all body lines)
	     (?c . allout-hide-current-entry) ; Current, Hide this entry's body
	     (?l . allout-hide-current-leaves) ; Leaves, Hide body lines in this entry and sub-entries
	     (?i . allout-show-current-branches) ; Show all sub-headings under this heading
	     (?e . allout-show-entry)		 ; Show this heading's body
	     (?o . allout-show-to-offshoot)))
        (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
                                  " " (char-to-string (car mapping)))
                         (quote ,(cdr mapping))
                         allout-mode-map)))

      (setq allout-command-prefix (kbd "C-c C-v"))
      )

    (defvar my-allout-font-lock-keywords
      '(;;
	;; Highlight headings according to the level.
	(eval . (list (concat "^\\(" allout-regexp "\\).+")
		      0 '(or (cdr (assq (allout-depth)
					'((1 . font-lock-function-name-face)
					  (2 . font-lock-variable-name-face)
					  (3 . font-lock-keyword-face)
					  (4 . font-lock-builtin-face)
					  (5 . font-lock-comment-face)
					  (6 . font-lock-constant-face)
					  (7 . font-lock-type-face)
					  (8 . font-lock-string-face))))
			     font-lock-warning-face)
		      nil t)))
      "Additional expressions to highlight in Outline mode.")

    ;; add font-lock to allout mode
    (defun my-allout-font-lock-hook ()
      (set (make-local-variable 'font-lock-defaults)
	   '(my-allout-font-lock-keywords t nil nil allout-back-to-current-heading)))


    (add-hook 'allout-mode-hook 'my-allout-mode-hook)
    ;(add-hook 'allout-mode-hook 'my-allout-font-lock-hook)
    ))

;; Enable the allout minor mode on Emacs starts for all modes
;; by creating a global version of the minor mode
(defun my-turn-on-allout-mode-maybe ()
  "Enable `allout-mode', where applicable."
  ;; Unconditional here, but edit as desired if it turns out
  ;; that you don't actually want this for ALL modes.
  ;; (This function is called in every buffer, when the
  ;; global mode is enabled.)
  (allout-mode 1))

(define-globalized-minor-mode my-global-allout-mode allout-mode
  my-turn-on-allout-mode-maybe
  :group 'allout)

(my-global-allout-mode 1)


;;;_* auto-complete
(use-package auto-complete-config
  :disabled t
  :diminish auto-complete-mode
  :init
  (progn
    (use-package pos-tip)
    (ac-config-default))

  :config
  (progn
    ;;(ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<backtab>")
    (setq ac-use-menu-map t)

    (bind-key "A-M-?" 'ac-last-help)
    (unbind-key "C-s" ac-completing-map)))

;;;_* autopair
(use-package autopair
  :disabled t
  :commands autopair-mode
  :diminish autopair-mode
  :init
  (hook-into-modes #'autopair-mode '(c-mode-common-hook
                                     text-mode-hook
                                     ruby-mode-hook
                                     python-mode-hook
                                     sh-mode-hook)))

;;;_* autorevert
(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook
            #'(lambda ()
                (auto-revert-mode 1))))

;;;_* flycheck
(use-package flycheck
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point)
  :init
  (progn
    (flycheck-define-checker clang++-ledger
      "Clang++ checker for Ledger"
      :command
      '("clang++" "-Wall" "-fsyntax-only"
        "-I/Users/johnw/Products/ledger/debug" "-I../lib"
        "-I../lib/utfcpp/source"
        "-I/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7"
        "-include" "system.hh" "-c" source-inplace)
      :error-patterns
      '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning:\\s-*\\(?4:.*\\)"
         warning)
        ("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error:\\s-*\\(?4:.*\\)"
         error))
      :modes 'c++-mode
      :predicate '(string-match "/ledger/" (buffer-file-name)))

    (push 'clang++-ledger flycheck-checkers)))

;;;_* flyspell
(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :config
  (define-key flyspell-mode-map [(control ?.)] nil))

;;;_* grep
(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s f" . find-grep)
         ("M-s g" . grep))
  :init
  (progn
    (defun find-grep-in-project (command-args)
      (interactive
       (let ((default (thing-at-point 'symbol)))
         (list (read-shell-command "Run find (like this): "
                                   (cons (concat "git --no-pager grep -n "
                                                 default)
                                         (+ 24 (length default)))
                                   'grep-find-history))))
      (if command-args
          (let ((null-device nil))      ; see grep
            (grep command-args))))

    (bind-key "M-s p" 'find-grep-in-project))

  :config
  (progn
    (use-package grep-ed)

    (grep-apply-setting 'grep-command "egrep -nH -e ")
    (if t
        (progn
          (setq-default grep-first-column 1)
          (grep-apply-setting
           'grep-find-command
           '("ag --noheading --nocolor --smart-case --nogroup --column -- "
             . 61)))
      (grep-apply-setting
       'grep-find-command
       '("find . -name '*.hs' -type f -print0 | xargs -P4 -0 egrep -nH "
         . 62)))))

;;;_* hi-lock
(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))


;;;_* ido
(use-package ido
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :init
  (ido-mode 'buffer)

  :config
  (progn
    (use-package ido-hacks
      :init
      (ido-hacks-mode 1))

    (use-package ido-springboard)

    (defun ido-smart-select-text ()
      "Select the current completed item.  Do NOT descend into directories."
      (interactive)
      (when (and (or (not ido-require-match)
                     (if (memq ido-require-match
                               '(confirm confirm-after-completion))
                         (if (or (eq ido-cur-item 'dir)
                                 (eq last-command this-command))
                             t
                           (setq ido-show-confirm-message t)
                           nil))
                     (ido-existing-item-p))
                 (not ido-incomplete-regexp))
        (when ido-current-directory
          (setq ido-exit 'takeprompt)
          (unless (and ido-text (= 0 (length ido-text)))
            (let ((match (ido-name (car ido-matches))))
              (throw 'ido
                     (setq ido-selected
                           (if match
                               (replace-regexp-in-string "/\\'" "" match)
                             ido-text)
                           ido-text ido-selected
                           ido-final-text ido-text)))))
        (exit-minibuffer)))

    (add-hook 'ido-minibuffer-setup-hook
              #'(lambda ()
                  (bind-key "<return>" 'ido-smart-select-text
                            ido-file-completion-map)))

    (defun ido-switch-buffer-tiny-frame (buffer)
      (interactive (list (ido-read-buffer "Buffer: " nil t)))
      (with-selected-frame
          (make-frame '((width                . 80)
                        (height               . 22)
                        (left-fringe          . 0)
                        (right-fringe         . 0)
                        (vertical-scroll-bars . nil)
                        (unsplittable         . t)
                        (has-modeline-p       . nil)
                        ;;(background-color     . "grey80")
                        (minibuffer           . nil)))
        (switch-to-buffer buffer)
        (set (make-local-variable 'mode-line-format) nil)))

    (bind-key "C-x 5 t" 'ido-switch-buffer-tiny-frame)))

;;;_* paren
(unless (use-package mic-paren
          :init
          (paren-activate))
  (use-package paren
    :init
    (show-paren-mode 1)))

;;;_* popup-ruler
(use-package popup-ruler
  :bind (("C-. r" . popup-ruler)
         ("C-. R" . popup-ruler-vertical)))

;;;_* recentf
(use-package recentf
  :if (not noninteractive)
  :init
  (progn
    (recentf-mode 1)

    (defun recentf-add-dired-directory ()
      (if (and dired-directory
               (file-directory-p dired-directory)
               (not (string= "/" dired-directory)))
          (let ((last-idx (1- (length dired-directory))))
            (recentf-add-file
             (if (= ?/ (aref dired-directory last-idx))
                 (substring dired-directory 0 last-idx)
               dired-directory)))))

    (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))

;;;_* rect-mark
;; In the Rectangle Mark mode,
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
(use-package rect-mark)
;; Support for marking a rectangle of text with highlighting.
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)

;; Modify picture mode so that it automatically uses the rect-mark
;; equivalents of many commands.
(add-hook 'picture-mode-hook 'rm-example-picture-mode-bindings)
(autoload 'rm-example-picture-mode-bindings "rect-mark"
  "Example rect-mark key and mouse bindings for picture mode.")

;;;_* session
(use-package session
  :if (not noninteractive)
  :load-path "site-lisp/session/lisp/"
  :init
  (progn
    (session-initialize)
    (defun remove-session-use-package-from-settings ()
      (when (string= (file-name-nondirectory (buffer-file-name)) "settings.el")
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^ '(session-use-package " nil t)
            (delete-region (line-beginning-position)
                           (1+ (line-end-position)))))))
    (add-hook 'before-save-hook 'remove-session-use-package-from-settings)
    ;; expanded folded secitons as required
    (defun le::maybe-reveal ()
      (when (and (or (memq major-mode  '(org-mode outline-mode))
                     (and (boundp 'outline-minor-mode)
                          outline-minor-mode))
                 (outline-invisible-p))
        (if (eq major-mode 'org-mode)
            (org-reveal)
          (show-subtree))))
    (add-hook 'session-after-jump-to-last-change-hook
              'le::maybe-reveal)
    (defun save-information ()
      (with-temp-message "Saving Emacs information..."
        (recentf-cleanup)
        (loop for func in kill-emacs-hook
              unless (memq func '(exit-gnus-on-exit server-force-stop))
              do (funcall func))
        (unless (or noninteractive
                    running-alternate-emacs
                    (eq 'listen (process-status server-process)))
          (server-start))))
    (run-with-idle-timer 300 t 'save-information)
    (if window-system
        (add-hook 'after-init-hook 'session-initialize t))))


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
  :commands (smartparens-mode show-smartparens-mode)
  :config (require 'smartparens-config))

;;;_* undo-tree
;; http://www.emacswiki.org/emacs/RedoMode
(use-package undo-tree
  :init (progn
    (global-undo-tree-mode 1)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c '" . undo-tree-visualize)) )

;;;_* visual-line-mode
;; The problem with visual-line mode is that by default, there are no
;; indicators showing where the line is wrapped. To fix this,
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; You need to put the assignment before the global-visual-line-mode call
;; From Daniel Neal, http://stackoverflow.com/questions/18268741/

;;;_* wrap-region
(use-package wrap-region
  :commands wrap-region-mode
  :diminish wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode c-mode c++-mode))
     ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))


;;;_* others
;(load-library "init-edit01-yasnippet") 

;; End:
