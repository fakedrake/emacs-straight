(require 'haskell-cabal)

;;;###autoload
(defun haskell-cabal--find-tags-dir-ad (fn &rest r)
  (cl-flet ((get-dir (x) (expand-file-name (locate-dominating-file default-directory x))))
    (or
     (get-dir "tags")
     (get-dir "TAGS")
     (get-dir "stack.yaml")
     (apply fn r))))

(provide 'fd-haskell-tags)
