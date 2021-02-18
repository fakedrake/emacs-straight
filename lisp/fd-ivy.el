(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer))
  :custom ((ivy-display-style 'fancy)
           (ivy-use-virtual-buffers t))
  :config
  (ivy-mode t))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x i" . counsel-imenu)
         ("C-x M-f" . counsel-git)
         ("C-x C-f" . counsel-find-file)))

;; Better filtering
(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package swiper
  :bind (("C-M-s" . counsel-grep-or-swiper)
         ("C-r" . swiper)
         ("C-s" . swiper)))

(use-package ivy-hydra)
(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode))
