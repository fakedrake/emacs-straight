(use-package company
  :init
  (global-company-mode 1)
  (when (boundp 'global-auto-complete-mode)
    (setq global-auto-complete-mode -1)))
