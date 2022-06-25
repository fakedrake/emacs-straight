(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Set the CNTLM proxy
(require 'cl-lib)
(defun setup-cntlm-proxy--internal (set-or-unset)
  (cl-flet ((s (v) (when set-or-unset v)))
    (setenv "HTTPS_PROXY" (s "http://127.0.0.1:3128"))
    (setenv "HTTP_PROXY" (s "http://127.0.0.1:3128"))
    (setenv "https_proxy" (s "http://127.0.0.1:3128"))
    (setenv "http_proxy" (s "http://127.0.0.1:3128"))
    (setq url-proxy-services
	  (s '(("http"     . "127.0.0.1:3128")
	       ("https"     . "127.0.0.1:3128")
               ("no_proxy" . "^.*\\.huawei\\.com"))))))
(defun enable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal t))
(defun disable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal nil))

; (enable-cntlm-proxy)
(disable-cntlm-proxy)


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
(init-require 'fd-rust)
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
 '(custom-safe-themes
   '("86014f0f5b78a610977acc74f127b56c7f0506a5973d872d11752879855d401e" "1b43d37da6d8935d26e55f15d3af672b14e6a25f8c891c40efe19255c41264da" "5a343f93cded62690a8167421249b7ceea1cca89224ad3550e85006305344ca0" "34235be96d680dee818d58371286aed3c13441f287156bb03f302c46637bcfca" "00eae1b4c390947bf53ee1dca98c6bb9b6fb5f4bc87cfc1b226b6258826b9aa6" "f58f766170b04a1c73d4cf152eac9bb6e317ceb6f5a2f92dcc3e300c9e28eb8f" "55a1a4c438a0517d916959a8a177af489417fa1d0db76fe1149261ced31437f8" "3bddc745807c3eb8f2bebda5edeb3186f67c669a0d9ae401f9fda900814dd8e1" "f695610f81acabf7455a76187ed5e66c4b67a73b76a0c2208bac61e6667c086a" "58115a5caf2c19aa48de6562c70dfaec47b429707389e03ef59f22f6f6be65ab" "e1d6039990206eb201dbe9a08246cede66e28bab7c0a85fbedfe3714cb3daf8f" "0f0e3de6e1599045abbd5e56a5d1ca686900fd564d27866b1269617c199559f0" "b616b5b7a808c4d46a90549bca2f4dcb5bff4f0e04ddaece5611b186f7e9de53" "1f96167d9def2e56d201a4fe5c70a3081f44e43e257ca0003d0373d0294e2e84" default))
 '(safe-local-variable-values
   '((compile-root . "/plink:semedi-ts-03:/home/christosp/Projects/gmdbv5/")))
 '(warning-suppress-types '((jedi))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
