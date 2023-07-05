(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Set the CNTLM proxy
(require 'cl-lib)


(defmacro init-require (sym)
  `(load (expand-file-name (format "lisp/%s.el" (symbol-name ,sym))
                           user-emacs-directory)))

(add-to-list 'load-path
	     (let ((default-directory user-emacs-directory))
	       (expand-file-name "lisp")))
(init-require 'fd-huawei)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (let ((buf (url-retrieve-synchronously
		"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		nil 'inhibit-cookies) ))
      (with-current-buffer buf
	(goto-char (point-max))
	(eval-print-last-sexp))))
  (load bootstrap-file nil nil))

(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://melpa.org/packages/"))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(use-package git)

; (require 'fd-benchmark-init)

(init-require 'fd-misc)
(init-require 'fd-misc-programming)
(init-require 'fd-yasnippet)
(use-package yaml-mode)
(init-require 'fd-clipboard)
(when (eq system-type 'darwin) (init-require 'fd-macosx))
(init-require 'fd-grep)
(init-require 'fd-ivy)

;; Programming modes
(init-require 'fd-lsp) ; must be before all programming stuff
(require 'fd-compilation)
(init-require 'fd-python)
; (init-require 'fd-haskell)
(init-require 'fd-nix)
(init-require 'fd-flycheck)
; (init-require 'fd-fluidb)
(init-require 'fd-rust)
; (init-require 'fd-rust-eglot)
(init-require 'fd-tags)
(init-require 'fd-documents)
(init-require 'fd-org)

(init-require 'fd-cc-mode)
(init-require 'fd-lisp)
(init-require 'fd-company)
(init-require 'fd-git)
; (init-require 'fd-factor)
(init-require 'fd-yasnippet)
; (init-require 'fd-fluidb)
(init-require 'fd-racket)
(init-require 'fd-visual)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "66bdbe1c7016edfa0db7efd03bb09f9ded573ed392722fb099f6ac6c6aefce32" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "566c61d0e40a728d5b631ac276017ace6866574ee58a4b6fbc770e436e481d57" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
