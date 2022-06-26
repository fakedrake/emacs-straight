(setq flymake-list-only-diagnostics ())

                                        ; (use-package eglot)

(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package eglot
  :ensure t
  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(defun fd/rust-hook ()
  (delete #'flymake-eldoc-function eldoc-documentation-functions)
  (add-to-list 'eldoc-documentation-functions #'flymake-eldoc-function))

(use-package rust-mode
  :hook ((rust-mode . fd/rust-hook))
  :config
  (add-to-list 'exec-path (concat (file-name-as-directory (getenv "CARGO_HOME")) "bin"))
  (setq rust-cargo-default-arguments "-- --nocapture")
  (require 'lsp-rust)
  (add-to-list 'eglot-server-programs '(rust-mode "/home/christosp/.cargo/bin/rustup" "run" "nightly" "rust-analyzer"))
  (setq eglot-rust-server 'rust-analyzer))

(use-package rmsbolt
  :config
  (setq rmsbolt--shell "msys2"))


(setq dap-cpptools-extension-version "1.5.1")

(with-eval-after-load 'lsp-rust
  (require 'dap-cpptools))

(require 'dap-gdb-lldb)
(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :program (executable-find "cargo")
                                     :cwd "${workspaceFolder}"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}"
                                     :target nil)))

;; (use-package exec-path-from-shell
;;  :ensure
;;  :init (exec-path-from-shell-initialize))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1)
  (dap-ui-controls-mode -1))
