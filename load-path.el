;; -*- emacs-lisp -*-

;;; Set load path

(defconst user-lisp-directory
  (expand-file-name "lisp" user-emacs-directory))
(defconst user-data-directory
  (file-truename "~/.config/emacs-user-data"))
(defconst user-cache-directory
  (file-truename "~/.cache/emacs-user-cache"))

;;; base directories
(defconst backup-dir
  (expand-file-name (concat "backups/" (user-real-login-name) "/")
                    user-cache-directory)
  "Directory for Emacs backups.")

(defconst
  autosave-dir
  (expand-file-name (concat "autosaves/" (user-real-login-name) "/")
                    user-cache-directory)
  "Directory for Emacs auto saves.")

;; These should always exist
(make-directory user-data-directory t)
(make-directory user-cache-directory t)
(make-directory autosave-dir t)

(setq
 auto-save-list-file-prefix (expand-file-name
                             "auto-save-list/" user-data-directory)
 backup-directory-alist (list (cons "." backup-dir))
 auto-save-file-name-transforms `((".*" ,autosave-dir t)) )

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

(eval-when-compile (require 'cl))
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(sanityinc/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(provide 'load-path)
