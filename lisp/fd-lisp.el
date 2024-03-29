
(define-key emacs-lisp-mode-map "\C-c\C-e" 'eval-buffer)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(defun fd-elisp-hooks ()
  (define-key emacs-lisp-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key emacs-lisp-mode-map (kbd "C-M-d") 'edebug-defun))


(use-package chicken-scheme)
(use-package geiser-chez)
(use-package geiser-chicken)
(add-hook 'emacs-lisp-mode-hook 'fd-elisp-hooks)

(use-package sly
  :config
  (setq inferior-lisp-program "ros run -Q"))
(use-package racket-mode)
(use-package cider)
