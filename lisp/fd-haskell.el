(use-package haskell-mode
  :after (company-mode)
  :hook ((haskell-mode . company-mode)))

(straight-use-package
 '(fd-haskell :type git :host github :repo "fakedrake/fd-haskell"
              :files (:defaults "snippets")))
(straight-use-package 'haskell-snippets)
(require 'fd-haskell)
(fd-haskell-configure-haskell-mode)
(defun fd-haskell-comint-mode-hook ()
  (company-mode 1))
(add-hook 'haskell-comint-mode-hook 'fd-haskell-comint-mode-hook)
