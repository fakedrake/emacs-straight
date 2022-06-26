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
  ; XXX: If you have problems with windows use branch windows-fix from
  ; https://gitlab-uk.rnd.huawei.com/c84174081/racket-mode
  :bind (:map racket-mode-map
         ("C-c C-k" . racket-racket-clear-repl)
         :map racket-repl-mode-map
         ("C-c C-k" . racket-repl-clear-repl))
  :hook ((racket-mode-hook . company-mode)
         (racket-repl-mode-hook . company-mode))
  :config
  (require 'racket-xp)
  (add-hook 'racket-mode-hook #'racket-xp-mode))

(when (eq system-type 'windows-nt)
  (defun racket--ensure-updated-back-end-on-remote/around (oldfn)
    "When on windows expand-file-name prepends c: even if the
remote server is not a widows server. Here e assume that "
    (concat "/" (funcall oldfn)))

  (advice-add 'racket--ensure-updated-back-end-on-remote :around
              #'racket--ensure-updated-back-end-on-remote/around))
