(add-to-list 'exec-path "/Users/cperivol/.cargo/bin")

;; (defun fd-rust-mode-after-save ()
;;   (rust-update-buffer-project)
;;   (let ((default-directory
;;           (or (and rust-buffer-project
;;                    (file-name-directory rust-buffer-project))
;;               default-directory)))
;;     (start-process "rusty-tags-process" "*rusty-tags-process*" "rusty-tags" "emacs" "--output=TAGS")))

;; (defun fd-rust-mode-hook ()
;;   (add-hook 'after-save-hook 'fd-rust-mode-after-save nil t))

;; (use-package rust-mode
;;   :hook ((rust-mode . fd-rust-mode-hook))
;;   :config
;;   (require 'fd-gud-lldb))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq lsp-rust-analyzer-server-args nil)
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rustic-lsp-check-conn ()
  (message "checking command..." )
  t)


(defun rustic-lsp-conn-command ()
  (cons
   (executable-find "rust-analyzer" t)
   (cl-rest lsp-rust-analyzer-server-args)))

(with-eval-after-load "lsp-rust"
  (defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
    "Start a program in a subprocess.  Return the process object
for it. Similar to `start-process-shell-command', but calls
`start-file-process'."
    ;; On remote hosts, the local `shell-file-name' might be useless.
    (let ((command (mapconcat 'identity args " ")))
      (funcall start-file-process-shell-command name buffer command)))

  (advice-add 'start-file-process-shell-command
              :around #'start-file-process-shell-command@around)

 (lsp-register-client
  (make-lsp-client
   :new-connection (lsp-tramp-connection #'rustic-lsp-conn-command)
   :remote? t
   :major-modes '(rustic-mode)
   :initialization-options 'lsp-rust-analyzer--make-init-options
   :notification-handlers (ht<-alist lsp-rust-notification-handlers)
   :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
   :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
   :after-open-fn (lambda ()
                    (when lsp-rust-analyzer-server-display-inlay-hints
                      (lsp-rust-analyzer-inlay-hints-mode)))
   :ignore-messages nil
   :server-id 'rust-analyzer-remote)))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (setq-local lsp--use-nix nil)
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rustfmt)
