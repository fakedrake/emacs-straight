(add-to-list 'exec-path "/Users/cperivol/.cargo/bin")

(defun fd-rust-mode-after-save ()
  (let ((default-directory
          (or (and rust-buffer-project
                   (file-name-directory rust-buffer-project))
              default-directory)))
    (start-process "rusty-tags-process" "*rusty-tags-process*" "rusty-tags" "emacs")))

(defun fd-rust-mode-hook ()
  (add-hook 'after-save-hook 'fd-rust-mode-after-save nil t))

(use-package rust-mode
  :hooks ((rust-mode . fd-rust-mode-hook))
  :config
  (require 'fd-gud-lldb))

(use-package rustfmt)
