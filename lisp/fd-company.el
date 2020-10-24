(use-package company
  :hook (after-init-hook . global-company-mode)
  :init
  (when (boundp 'global-auto-complete-mode)
    (setq global-auto-complete-mode -1)))
(provide 'fd-company)
