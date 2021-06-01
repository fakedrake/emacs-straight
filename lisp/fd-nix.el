(use-package nix-sandbox
  :config
  (setenv "NIX_PATH" (shell-command-to-string "echo -n $NIX_PATH"))
  (add-hook 'find-file-hook 'nix-update-exec-path))

(defun nix-update-exec-path (&optional file-path)
 (unless (and file-path (file-remote-p file-path))
    (if-let ((sandbox (nix-find-sandbox
                       (file-name-directory (or file-path (buffer-file-name))))))
        (setq-local exec-path (nix-exec-path sandbox)))))
