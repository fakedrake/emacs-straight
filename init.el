(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

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

(defmacro init-require (sym)
  `(load (expand-file-name (format "lisp/%s.el" (symbol-name ,sym))
                           user-emacs-directory)))

(add-to-list 'load-path
	     (let ((default-directory user-emacs-directory))
	       (expand-file-name "lisp")))

; (require 'fd-benchmark-init)

(init-require 'fd-misc)
(init-require 'fd-misc-programming)
(use-package yaml-mode)
(init-require 'fd-clipboard)
(when (eq system-type 'darwin) (init-require 'fd-macosx))
(init-require 'fd-grep)
; (require 'fd-helm)
(init-require 'fd-ivy)
; (require 'fd-selectrum)
(require 'fd-compilation)
(init-require 'fd-python)
(init-require 'fd-haskell)
(init-require 'fd-nix)
(init-require 'fd-flycheck)
(init-require 'fd-fluidb)
(init-require 'fd-rust)
(init-require 'fd-tags)
(init-require 'fd-documents)
(use-package realgud-lldb)

(init-require 'fd-cc-mode)
(init-require 'fd-lsp)
(init-require 'fd-lisp)
(init-require 'fd-coq)
(init-require 'fd-company)
(init-require 'fd-git)
(init-require 'fd-factor)
(init-require 'fd-visual)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-format-functions-alist
   '((counsel-compile-env . counsel-compile-env--format-hint)
     (counsel-kmacro . counsel--kmacro-format-function)
     (counsel-colors-web . counsel--colors-web-format-function)
     (counsel-colors-emacs . counsel--colors-emacs-format-function)
     (counsel-evil-registers . counsel--yank-pop-format-function)
     (counsel-yank-pop . counsel--yank-pop-format-function)
     (counsel-git-log . counsel--git-log-format-function)
     (counsel-faces . counsel--faces-format-function)
     (swiper-isearch . swiper-isearch-format-function)
     (swiper-all . swiper--all-format-function)
     (swiper-multi . swiper--all-format-function)
     (t . ivy-format-function-line)))
 '(safe-local-variable-values
   '((TeX-auto-local . "/Users/cperivol/Projects/phd/thesis/style")
     (TeX-style-local . "/Users/cperivol/Projects/phd/thesis/style")
     (reftex-default-bibliography "/Users/cperivol/Projects/phd/thesis/bib/zotero.bib")
     (compile-root . "/Users/cperivol/Projects/phd/thesis/")))
 '(warning-suppress-types '((comp) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
