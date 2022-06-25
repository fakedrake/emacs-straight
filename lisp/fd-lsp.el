(defun lsp-or-eglot ()
  "Run `lsp-deferred' unless this is a remote file."
  (interactive)
  (if-let ((fname (buffer-file-name)))
      (if (file-remote-p fname) (eglot-ensure) (lsp-deferred))))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((cc-mode . lsp-or-eglot)
         (haskell-mode . lsp-or-eglot)
         (rust-mode . lsp-or-eglot))
  :config
  (require 'lsp-mode)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet nil)
  ; (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil))

(use-package all-the-icons)

(use-package lsp-ui
  :config
  ; (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-diagnostic-max-lines 3))

(use-package dap-mode)

;; (use-package lsp-haskell)

;; (setq-default lsp--use-nix t)

;; ; cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
;; ; (setq lsp-log-io t) to log the exact LSP messages going between the server and client.
;; (defun lsp-server-wrapper-function-nix (argv)
;;   (if (and (nix-find-sandbox default-directory) lsp--use-nix)
;;       (append (list "nix-shell" "-I" "." "--command" )
;;               (list (mapconcat 'identity argv " ")))
;;     argv))
