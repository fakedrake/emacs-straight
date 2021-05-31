
(defvar flycheck--restore-wrapper-function nil)
(defun flycheck-debug-wrapper-function (argv)
  (message (mapconcat 'identity argv " "))
  argv)

(define-minor-mode flycheck-debug-minor-mode
  ""
  :group 'flycheck
  :global t
  (cond
   ((and flycheck-debug-minor-mode (not flycheck--restore-wrapper-function))
    (setq flycheck--restore-wrapper-function flycheck-command-wrapper-function
          flycheck-command-wrapper-function 'flycheck-debug-wrapper-function))
   (t (setq flycheck-command-wrapper-function flycheck--restore-wrapper-function
            flycheck--restore-wrapper-function nil))))
