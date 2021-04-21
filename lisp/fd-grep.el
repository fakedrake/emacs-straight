(defun tags-find-symbol (symbol file-domain)
  (interactive (list
                (rg-read-pattern 'literal (grep-tag-default)) (rg-read-files)))
  (rg-project (format "\\b%s\\b" (regexp-quote symbol)) file-domain))

(use-package wgrep)
(use-package rg
  :after (wgrep)
  :config
  (defalias 'rgrep 'rg)
  (setq rg-executable (executable-find "rg"))
  (autoload 'wgrep-rg-setup "wgrep-rg")
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))
