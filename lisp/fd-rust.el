(setq flymake-list-only-diagnostics ())

(use-package eglot)
(use-package rust-mode
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (setq eglot-rust-server 'rust-analyzer))
