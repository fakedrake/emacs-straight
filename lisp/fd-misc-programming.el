(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(add-hook 'before-save-hook 'maybe-delete-whitespace)

(defvar keep-whitespace nil
  "Don't delete trailing whitespaces in a file.")
(defun maybe-delete-whitespace ()
  "if `keep-whitespace' is non-nil delete trailing whitespaces on
each line."
  (interactive)
  (if keep-whitespace
      (message "Unset `keep-whitespace' to remove trailing whitespace on save")
    (delete-trailing-whitespace)))
(setq require-final-newline 'save)
(electric-pair-mode)
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\C-x\\" 'indent-buffer)
(setq ediff-split-window-function 'split-window-horizontally)


(global-set-key (kbd "C-j") 'default-indent-new-line)
(global-set-key (kbd "C-M-l") 'add-dir-local-variable)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-line-column 80)
(global-whitespace-mode t)
(setq narrow-to-defun-include-comments t)
(setq project-vc-ignores '())
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(use-package format-all)

(use-package tree-mode)
(use-package vterm)
