(use-package helm
  :demand
  :preface (require 'helm-config)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini))
  :config
  (helm-mode 1)
  (setq helm-buffer-skip-remote-checking t
        helm-mode-fuzzy-match t))

(provide 'fd-helm)
