;;; addon-edit00_folding.el --- Support code and region folding -*- lexical-binding: t -*-

;;; Commentary:
;; https://github.com/CharellKing/.emacs.d/blob/master/lisp/init-fold.el

;;; Code:
(require 'origami)

(global-set-key (kbd "C-\\") 'origami-recursively-toggle-node)
(global-set-key (kbd "M-\\") 'origami-close-all-nodes)
(global-set-key (kbd "M-+") 'origami-open-all-nodes)

(global-origami-mode)

(provide 'addon-edit00_folding)
;;; addon-edit00_folding.el ends here
