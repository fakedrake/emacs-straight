(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map ([backtab] . yas/expand))
  :config
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))
(use-package yasnippet-snippets)
