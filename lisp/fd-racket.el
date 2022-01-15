(defun racket-racket-clear-repl ()
  (interactive)
  (when racket-repl-buffer-name
    (with-current-buffer racket-repl-buffer-name
      (racket-repl-clear-repl))))

(defun racket-repl-clear-repl ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (end-of-buffer)
      (forward-line 0)
      (delete-region (point-min) (point)))))

(use-package racket-mode
  :straight (racket-mode :type git :repo "greghendershott/racket-mode" :branch "rebased-multi-back-end")
  :bind (:map racket-mode-map
         ("C-c C-k" . racket-racket-clear-repl)
         :map racket-repl-mode-map
         ("C-c C-k" . racket-repl-clear-repl))
  :hook ((racket-mode-hook . company-mode)
         (racket-repl-mode-hook . company-mode))
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode))