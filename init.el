(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'package)
(add-to-list 'package-archives '("melpa"
                                 . "https://melpa.org/packages/"))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(use-package git)

(defmacro init-require (sym)
  `(let ((default-directory user-emacs-directory))
    (load (expand-file-name (format "lisp/%s.el" (symbol-name ,sym))))))
(add-to-list 'load-path 
	     (let ((default-directory user-emacs-directory))
	       (expand-file-name "lisp")))
; (require 'fd-benchmark-init)
(init-require 'fd-misc)
(init-require 'fd-misc-programming)
(use-package yaml-mode)
(init-require 'fd-clipboard)
(when (eq system-type 'darwin) (init-require 'fd-macosx))
; (require 'fd-helm)
(init-require 'fd-ivy)
; (require 'fd-selectrum)
(require 'fd-compilation)
(init-require 'fd-python)

(straight-use-package
 '(fd-haskell :type git :host github :repo "fakedrake/fd-haskell"
              :files (:defaults "snippets")))
(require 'fd-haskell)
(fd-haskell-configure-haskell-mode)
(init-require 'fd-lisp)
(init-require 'fd-coq)
(init-require 'fd-company)
(init-require 'fd-git)
(init-require 'fd-visual)
