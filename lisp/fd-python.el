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

;; company-jedi wires up jedi to be a backend for the auto completion
;; library, company-mode. DO NOT USE JEDI SEPARATELY
(use-package company-jedi
  :hook ((python-mode . (lambda () (add-to-list 'company-backends 'company-jedi))))
  :custom
  (jedi:complete-on-dot t)
  (jedi:use-shortcuts t))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
              ("C-c M-t" . fd-python-run-tests)
              ("M-." . jedi:goto-definition)
              ("M-," . jedi:goto-definition-pop-marker)
              ("M-q" . 'python-smart-fill-paragraph))
  :custom
  ((python-environment-virtualenv '("python3" "-m" "venv"))
   (paragraph-start (concat paragraph-start "\\|\\s-*\"\"\".*$")))
  :hook (python-mode . jedi:setup)
  :config
  (advice-add 'python-indent-line-function
              :around #'fd-python-indent-line-function))

;;; fd-python.el ends here
