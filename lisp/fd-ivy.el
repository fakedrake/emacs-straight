(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-S-j" . 'ivy-immediate-done) ; Use the current input
         ("C-j" . 'ivy-alt-done)) ; go into the selected directory
  :custom ((ivy-display-style 'fancy)
           (ivy-use-virtual-buffers t))
  :config
  (setf (alist-get t ivy-format-functions-alist) 'ivy-format-function-line)
  (ivy-mode t))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ;; ("C-h f" . counsel-describe-function)
         ;; ("C-h v" . counsel-describe-variable)
         ("C-x i" . counsel-imenu)
         ("C-x M-f" . counsel-git)
         ("C-x C-f" . counsel-find-file))
  :config
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist nil 'remove-item) nil))


(use-package amx)
;; ;; Better filtering
;; (use-package prescient)
;; (use-package ivy-prescient
;;   :config
;;   (push (cons 'fd-swiper-backward 'fd-ivy-recompute-index-swiper-backward)
;;         ivy-index-functions-alist)
;;    (ivy-prescient-mode t))

;; (defun fd-ivy-recompute-index-swiper-backward (_re-str cands)
;;   (- (ivy-recompute-index-swiper _re-str cands) 1))

;; (defun fd-swiper-backward (&optional initial-input)
;;   (interactive)
;;   (let ((ivy-index-functions-alist
;;          '((swiper . fd-ivy-recompute-index-swiper-backward))))
;;     (swiper initial-input)))

;; (defun swiper-C-r (&optional arg)
;;   "Move cursor vertically down ARG candidates.
;; If the input is empty, select the previous history element instead."
;;   (interactive "p")
;;   (if (string= ivy-text "")
;;       (ivy-next-history-element 1)
;;     (ivy-previous-line arg)))

;; (defun fd-swiper--recompute-background-faces ()
;;   "Background faces are the faces of matched strings that are not
;; the selected ones. We use the same faces as the matched ones but
;; a bit lighter using colir. The default function for recomputingn
;; faces does not work if the match face has inheritted the
;; attributes."
;;   (let ((faces '(swiper-background-match-face-1
;;                  swiper-background-match-face-2
;;                  swiper-background-match-face-3
;;                  swiper-background-match-face-4))
;;         (colir-compose-method #'colir-compose-soft-light))
;;     (cl-mapc (lambda (f1 f2)
;;                (let* ((bg (face-background f1 nil 'use-inherit))
;;                       ;; FIXME: (colir-color-parse "color-22") is nil.
;;                       (bg (and bg (colir-color-parse bg))))
;;                  (when bg
;;                    (setq bg (colir-blend bg (colir-color-parse "#ffffff")))
;;                    (set-face-background f2 bg))))
;;              swiper-faces
;;              faces)))

;; (use-package ctrlf
;;   :config
;;   (ctrlf-mode +1))

;; Hit C-' during swiper to jump to a result
;(use-package avy)

; C-o to get commands
;(use-package ivy-hydra)
;; (use-package which-key
;;   :config
;;   (add-hook 'after-init-hook 'which-key-mode))
