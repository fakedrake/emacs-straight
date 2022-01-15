(use-package flycheck)
(use-package haskell-mode
  :after (company-mode)
  :hook ((haskell-mode . fd-haskell-mode-hook)))

(defun fd-haskell-mode-hook ()
  (company-mode))

;; (straight-use-package
;;  '(fd-haskell :type git :host github :repo "fakedrake/fd-haskell"
;;               :files (:defaults "snippets")))
;; (require 'fd-haskell)

;; (use-package haskell-snippets)


(defun fd-haskell-comint-mode-hook ()
  (company-mode 1))



(defun haskell-shell-calculate-command/around (oldfn)
  (if-let ((sandbox (nix-find-sandbox default-directory)))
      (format "nix-shell --command \"%s\" %s" (funcall oldfn)
              sandbox)
    (funcall oldfn)))


(defun fd-haskell-comint-mode-hook ()
  (set (make-variable-buffer-local 'compilation-error-regexp-alist)
       '(haskell-called-at haskell-warning haskell-error)))

(use-package fd-haskell
  :straight nil
  :load-path "local-packages/fd-haskell"
  :after (flycheck)
  :config
  ;; :straight (fd-haskell :type git :host github :repo "fakedrake/fd-haskell"
  ;;                       :files (:defaults "snippets"))
                                        ; (require 'fd-haskell-tags)
  (fd-haskell-configure-haskell-mode)
  (add-hook 'haskell-comint-mode-hook 'fd-haskell-comint-mode-hook)
                                        ; Add nix shell capabilities.
  (advice-add 'haskell-shell-calculate-command :around
              #'haskell-shell-calculate-command/around)
  (setf (alist-get 'haskell-called-at compilation-error-regexp-alist-alist)
        (list (rx (: "called at" (in blank) (path-ext-linum-maybe-col "hs" 1 2 3))) 1 2 3 2))
  (setf (alist-get 'haskell-warning compilation-error-regexp-alist-alist)
        (list (rx (: bol (path-ext-linum-maybe-col "hs" 1 2 3) ": warning")) 1 2 3 1))
  (setf (alist-get 'haskell-error compilation-error-regexp-alist-alist)
        (list (rx (: bol (path-ext-linum-maybe-col "hs" 1 2 3) ": error")) 1 2 3 2))
  (require 'fd-fluidb-comint))
