(use-package helm
  :demand
  :preface (require 'helm-config)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini))
  :config
  (helm-mode 1)
  (setq helm-buffer-skip-remote-checking t
        helm-candidate-number-limit 100
        helm-mode-fuzzy-match t))

;; The following caches the symbol table when loaded
(defun register-definition-prefixes-ad-before (file prefixes)
    (setq help-definition-prefixes nil))
(advice-add #'register-definition-prefixes :before 'register-definition-prefixes-ad-before)
(defun help-definition-prefixes-ad (fn)
  (or help-definition-prefixes (funcall fn)))
(advice-add #'help-definition-prefixes :around 'help-definition-prefixes-ad)
(provide 'fd-helm)
