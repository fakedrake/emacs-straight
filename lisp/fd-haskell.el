(use-package dnb-haskell
  :hook haskell-mode)

(use-package haskell-mode
  :init
  (add-to-list
   'haskell-compilation-error-regexp-alist
   '("^[[:space:]]+[[:word:]'-_]+, called at \\(\\([[:word:]]*/\\)*[[:word:]]+\\.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) in " 1 2 (3 . 4) 0))

  :custom
  (haskell-process-type 'auto)
  (haskell-process-path-ghci "stack")
  (haskell-process-args-ghci '("repl")))


(defvar hs-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '((haskell-error
     "^\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\): error:$"
     2 3 4 2 1)
    (haskell-rts-stack
     "^[[:space:]]+[[:word:]]+, called at \\(\\([[:word:]]*/\\)*[[:word:]]+\\.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) in "
    1 3 4)))


;; Add hs-compilation-error-regex-alist to the appropriate lists. If
;; the var was updated don't keep adding, edit the result.
(require 'compile)
(dolist (el hs-compilation-error-regex-alist)
  (unless (memq (car el) compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist (car el)))

  (let ((ael (assq (car el) compilation-error-regexp-alist-alist)))
    (if ael (setf (cdr ael) (cdr el))
      (add-to-list 'compilation-error-regexp-alist-alist el))))
