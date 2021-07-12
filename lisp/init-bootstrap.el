(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (require feature)
    ('error (add-to-list 'rr-initialization-errors
                         (format "[ERROR LOADING \"%s\"]: %s" (symbol-name feature) ex)))))

(defun rr-safe-load-init-files ()
  (dolist (file init-files)
    (safe-require file)))

(defun rr-unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

(provide 'init-bootstrap)
