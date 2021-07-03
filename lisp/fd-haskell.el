(use-package haskell-mode
  :after (company-mode)
  :hook ((haskell-mode . company-mode)))

(straight-use-package
 '(fd-haskell :type git :host github :repo "fakedrake/fd-haskell"
              :files (:defaults "snippets")))
(straight-use-package 'haskell-snippets)
(require 'fd-haskell)
(require 'fd-haskell-tags)
(fd-haskell-configure-haskell-mode)
(defun fd-haskell-comint-mode-hook ()
  (company-mode 1))
(add-hook 'haskell-comint-mode-hook 'fd-haskell-comint-mode-hook)


(advice-add 'haskell-shell-calculate-command :around
            #'haskell-shell-calculate-command/around)

(defun haskell-shell-calculate-command/around (oldfn)
  (if-let ((sandbox (nix-find-sandbox default-directory)))
      (format "nix-shell --command \"%s\" %s" (funcall oldfn)
              sandbox)
    (funcall oldfn)))
(use-package auto-minor-mode
  :config
  (require 'fd-profiterole)
  (add-to-list 'auto-minor-mode-alist '(".profiterole.txt" . profiterole-mode)))
