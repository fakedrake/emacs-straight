(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Set the CNTLM proxy
(require 'cl)
(defun setup-cntlm-proxy--internal (set-or-unset)
  (cl-flet ((s (v) (when set-or-unset v)))
    (setenv "HTTPS_PROXY" (s "http://127.0.0.1:3128"))
    (setenv "HTTP_PROXY" (s "http://127.0.0.1:3128"))
    (setq url-proxy-services
	  (s '(("http"     . "127.0.0.1:3128")
	       ("https"     . "127.0.0.1:3128")
               ("no_proxy" . "^.*\\.huawei\\.com"))))))
(defun enable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal t))
(defun disable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal nil))

(enable-cntlm-proxy)


(when (eq system-type 'windows-nt)
  (defun add-to-exec-path (path)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat path ";" (getenv "PATH"))))

  (mapcar 'add-to-exec-path '("c:/Users/c84174081/scoop/shims")))

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
; (init-require 'fd-haskell-internal)
(init-require 'fd-nix)
(init-require 'fd-flycheck)
; (init-require 'fd-fluidb)
; (init-require 'fd-rust)
(init-require 'fd-rust-eglot)
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
(init-require 'fd-huawei)
(init-require 'fd-racket)
(init-require 'fd-visual)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((compile-root . "/plink:semedi-ts-03:/home/christosp/Projects/gmdbv5/")))
 '(warning-suppress-types '((jedi))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
