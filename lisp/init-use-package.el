;; -*- emacs-lisp -*-

;;; Setup use-package

(require 'use-package)
(use-package package
  :init (progn (add-to-list 'package-archives
                            '("melpa" . "http://melpa.milkbox.net/packages/") t)
               (package-initialize)))

(provide 'init-use-package)
