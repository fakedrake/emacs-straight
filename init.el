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

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'fd-misc)
(require 'fd-clipboard)
(when (eq system-type 'darwin) (require 'fd-macosx))
(require 'fd-helm)
(require 'fd-compilation)
(require 'fd-python)
(require 'fd-company)
(require 'fd-visual)
