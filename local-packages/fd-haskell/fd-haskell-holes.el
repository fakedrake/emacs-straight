(defun haskell-error-at-point (pos)
  "The flycheck error at point as taken by the overlay."
  (if-let
      ((err
        (find-if
         'identity
         (mapcar (lambda (ov) (overlay-get ov 'flycheck-error))
                 (overlays-at pos)))))
      (flycheck-error-message err)))

(defmacro haskell-with-current-error-buffer (pos &rest body)
  "Put the error at POS in a tmp buffer and execut BODY"
  `(if-let ((error-at-pt (haskell-error-at-point pos)))
       (with-temp-buffer
         (insert error-at-pt)
         (goto-char (point-min))
         (progn ,@body))
     (error "No flycheck error at point.")))

(defun haskell-type-of-hole (pos)
  "Get the type of the hole at POS using flycheck as a string.
POS defaults to `point'. Use `flycheck-type-of-hole' for some normalizations."
  (-when-let* ((errors (flycheck-overlay-errors-at (or pos (point)))))
    (with-temp-buffer
      (insert (flycheck-error-message (car errors)))
      (goto-char (point-min))
      (save-match-data
        (when (or
               (search-forward-regexp
                "Found hole: _[[:alnum:]_']* :: \\([[:alnum:]_']+\\)"
                nil t)
               (search-forward-regexp
                "• Found type wildcard ‘_’[[:space:]]*standing for ‘\\([^’]*?\\)’"
                nil t))
          (match-string 1))))))

(defun haskell-expand-type-hole ()
  "Repace the hole at point with it's type using flychceck."
  (interactive)
  (if-let ((bounds (bounds-of-thing-at-point 'symbol))
           (ty (haskell-type-of-hole (point))))
      (progn
        (delete-region (car bounds) (cdr bounds))
        (insert ty))))

(defun flycheck-type-of-hole (&optional pos)
  "Get the type of the hole at POS.

POS defaults to `point'."
  (-when-let* ((errors (flycheck-overlay-errors-at (or pos (point)))))
    (with-temp-buffer
      (insert (flycheck-error-message (car errors)))
      (goto-char (point-min))
      (save-match-data
        (when (search-forward-regexp
               "Found hole: _[[:alnum:]_']* :: \\([[:alnum:]_']+\\)"
               nil t)
          (match-string 1))))))

(provide 'fd-haskell-holes)
