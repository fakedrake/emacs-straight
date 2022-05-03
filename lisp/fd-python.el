;;; fd-python.el --- My python configuration -*- lexical-binding: t -*-

;; Author: Chris Perivolaropoulos
;; Maintainer: Chris Perivolaropoulos
;; Version: 1.0
;; Package-Requires: (company-jedi python)
;; Homepage: http://github.com/fakedrake/emacs-straight
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Pytho configuration

;;; Code:

(defun python-smart-fill-paragraph (&optional justify region)
  (interactive)
  (if (null (python-info-docstring-p))
      (fill-paragraph justify region)
    (let ((paragraph-start "\\(^\\(?2:\s+\\)\\(?3:\\**[[:alnum:]_]+\\):\\|\n\s*\n\\|^\s*- \\)"))
      (fill-paragraph justify region)
      (save-match-data
        (save-excursion
          (when (and (re-search-backward paragraph-start nil t)
                     (save-match-data (looking-at "^\s+[[:alnum:]_]+:")))
            (let ((spaces (match-string 2))
                  (word (match-string 3)))
              ;; Return and Raises are special cases
              (when (or (string= word "Returns")
                        (string= word "Raises")
                        (string= word "Yields"))
                (search-forward ":" nil t)
                (insert "\n") (backward-char))
              (let ((rbeg (save-excursion (end-of-line) (point)))
                    (rend (save-excursion (end-of-paragraph-text) (point))))
                (indent-lines-to rbeg rend (+ 2 (length spaces)))
                (fill-region rbeg rend)))))))))

(defun fd-python-indent-line-function (pyfun)
  (if (and (python-info-docstring-p) (not (looking-at "\"\"\"")))
      (if (looking-back "^\s*\\(Returns\\|Raises\\|Args\\|Yields\\):\s*\n\s*")
          (progn (indent-relative-maybe) (insert "  "))
        (indent-relative))
    (funcall pyfun)))


;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located. I use miniconda.
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks
            (lambda () (pyvenv-restart-python))))


;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

(use-package eglot
  :ensure t)

(defun fd-python-eglot-enable ()
  "set variables and hook for eglot python IDE"
  (interactive)
  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (add-to-list 'eglot-server-programs `(python-mode "pylsp"))
  (add-hook 'python-mode-hook 'eglot-ensure))

(defun fd-python-eglot-disable ()
  "remove hook for eglot python"
  (interactive)
  (remove-hook 'python-mode-hook 'eglot-ensure))

(use-package cython-mode
  :ensure t)

(defun fd-python-hook ()
  (jedi:setup)
  (setq-local indent-tabs-mode nil))

;;; fd-python.el ends here
