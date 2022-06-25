(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Set the CNTLM proxy
(require 'cl-lib)

(defun cntlm-host-ip ()
  (when (file-exists-p "/etc/resolv.conf")
    (with-temp-buffer
      (insert-file-contents "/etc/resolv.conf")
      (search-forward-regexp
       "nameserver +\\([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\)")
      (match-string-no-properties 1))))

(defun setup-cntlm-proxy--internal (set-or-unset)
  (cl-flet ((s (v) (when set-or-unset v)))
    (let* ((host-url (or (cntlm-host-ip) "127.0.0.1"))
           (prx (format "%s:3128" host-url))
           (http-url (format "http://%s" prx)))
      (setenv "HTTPS_PROXY" http-url)
      (setenv "HTTP_PROXY" http-url)
      (setenv "https_proxy" http-url)
      (setenv "http_proxy" http-url)
      (setq url-proxy-services
	    (s `(("http"     . ,prx)
	         ("https"    . ,prx)
                 ("no_proxy" . "^.*\\.huawei\\.com")))))))

(defun enable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal t))
(defun disable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal nil))

(enable-cntlm-proxy)
; (disable-cntlm-proxy)

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
(init-require 'fd-racket)
(init-require 'fd-visual)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
