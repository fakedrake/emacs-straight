(setq flymake-list-only-diagnostics ())

                                        ; (use-package eglot)

(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package rust-mode
  :config
  (add-to-list 'exec-path (concat (file-name-as-directory (getenv "CARGO_HOME")) "bin"))
  (setq rust-cargo-default-arguments "-- --nocapture")
  (require 'lsp-rust)
  (add-to-list 'eglot-server-programs '(rust-mode "rustup" "run" "nightly" "rust-analyzer"))
  (setq eglot-rust-server 'rust-analyzer))
