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

(defun fd-ivy-recompute-index-swiper-backward (_re-str cands)
  (- (ivy-recompute-index-swiper _re-str cands) 1))

(push (cons 'fd-swiper-backward 'df-ivy-recompute-index-swiper-backward)
      ivy-index-functions-alist)

(defun fd-swiper-backward (&optional initial-input)
  (interactive)
  (let ((ivy-index-functions-alist
         '((swiper . fd-ivy-recompute-index-swiper-backward))))
    (swiper initial-input)))

(defun swiper-C-r (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (if (string= ivy-text "")
      (ivy-next-history-element 1)
    (ivy-previous-line arg)))

(use-package swiper
  :bind (("C-M-s" . counsel-grep-or-swiper)
         ("C-r" . fd-swiper-backward)
         ("C-s" . swiper-isearch)
         :map ivy-minibuffer-map
         ("C-r" . swiper-C-r)))

(use-package ivy-hydra)
(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode))
