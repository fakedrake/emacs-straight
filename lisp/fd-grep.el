(use-package wgrep)
(use-package rg
  :after (wgrep)
  :config
  (defalias 'rgrep 'rg)
  (autoload 'wgrep-rg-setup "wgrep-rg")
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))
