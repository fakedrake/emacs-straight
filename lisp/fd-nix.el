(use-package  nix-mode)
(use-package nix-sandbox
  :config
  (setenv "NIX_PATH" (shell-command-to-string "echo -n $NIX_PATH"))
  (add-hook 'comint-mode-hook 'nix-update-exec-path)
  (add-hook 'find-file-hook 'nix-update-exec-path))

(defun nix-update-exec-path (&optional dir)
  (let ((real-dir (or dir default-directory)))
    (unless (file-remote-p real-dir)
      (if-let ((sandbox (nix-find-sandbox real-dir)))
          (setq-local exec-path (nix-exec-path sandbox))))))
