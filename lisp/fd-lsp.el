(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((cc-mode . lsp-deferred) (haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (advice-add 'lsp-resolve-final-function :filter-return #'lsp-server-wrapper-function-nix))

(use-package lsp-ui
  :config
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet nil)
  ; (setq lsp-ui-doc-enable Nil)
  (setq lsp-lens-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-diagnostic-max-lines 3))

(use-package lsp-haskell)

; cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
; (setq lsp-log-io t) to log the exact LSP messages going between the server and client.
(defun lsp-server-wrapper-function-nix (argv)
  (if (nix-find-sandbox default-directory)
      (append (list "nix-shell" "-I" "." "--command" )
              (list (mapconcat 'identity argv " ")))
    argv))
