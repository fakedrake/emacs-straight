(defun haskell-jump-to-or-insert-defininition ()
  "Look for the last function declaration before the point. If
there is a definion alread jump to that. If not insert one."
  (interactive)
  (if-let* ((decl-pos (save-excursion
                        (end-of-line)
                        (haskell-previous-declaration-search)))
            (fun-name (match-string 1))
            (def-pos (or (haskell-find-definition fun-name decl-pos)
                         (progn
                           (haskell-make-available-line)
                           (haskell-insert-function-stump fun-name)))))
      (goto-char def-pos)))

(defun haskell-make-available-line ()
  "Find a blank line suitable for inserting a function
definition. If there seems to be none before the next block of
code create one."
  (end-of-line)
  (when (re-search-forward "^\\($\\|.\\)" nil t)
    (unless (looking-back "\n")
      (beginning-of-line) (save-excursion (insert "\n")))
    (beginning-of-line)))

(defun haskell-insert-function-stump (fun-name)
  "Insert a function definition stump."
  (insert fun-name) (insert " ")
  (save-excursion (insert " = undefined")))

(defun haskell-previous-declaration-search ()
  "Search backward for a function declaration."
  (save-excursion
    (re-search-backward "^\\([[:alnum:]]*\\) ::")))

(defun haskell-find-definition (func-name &optional decl-point)
  "Search for the definition of FUNC-NAME starting at DECL-POINT
or the beginning of the buffer. Retrn the resulting position. The
return value is the point before the = sign."
  (save-excursion
    (goto-char (or decl-point (point-min)))
    (when (re-search-forward
           (concat "^\\(" func-name "\\)\\( +[[:alnum:]_]+\\)* *=") nil t)
      (if (looking-back " =") (backward-char 2) (backward-char 1))
      (point))))

(defun haskell-beginning-of-defun ()
  (save-match-data (re-seach-backwards "^[[:alnum:]]+")))

(provide 'fd-haskell-insert-definition)
