;;; fd-haskell.el --- summary -*- lexical-binding: t -*-

;; Author: Chris Perivolaropoulos
;; Maintainer: Chris Perivolaropoulos
;; Version: 0.0.2
;; Keywords: haskell


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

;; Haskell

;;; Code:

(require 'compile)
(require 'flycheck)
(require 'yasnippet)

(require 'haskell-indentation)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-modules)

(require 'fd-gud-haskell)
(require 'fd-haskell-holes)
(require 'fd-haskell-tags)
(require 'fd-haskell-insert-definition)
(require 'fd-haskell-ligatures)
(require 'fd-haskell-modules)
(require 'fd-haskell-test-files)
(require 'fd-haskell-comint)
(require 'fd-floskell)

;; Make sure our mode overrides interactive-haskell-mode



(rx-define any-word (&rest ws)
  (eval (cons '| (mapcar (lambda (w) `(: word-start ,w word-end)) (list ws)))))

(add-to-list
 'haskell-compilation-error-regexp-alist
 '("^[[:space:]]+[[:word:]'-_]+, called at \\(\\([[:word:]]*/\\)*[[:word:]]+\\.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) in " 1 2 (3 . 4) 0))

(defvar hs-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '((haskell-error
     "^\\([a-z\\-0-9]*\s*>\s*\\|\\)\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\): error:$"
     3 4 5 2 2)
    (haskell-rts-stack
     "^[[:space:]]+[[:word:]]+, called at \\(\\([[:word:]]*/\\)*[[:word:]]+\\.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) in "
    1 3 4)))

;; Add hs-compilation-error-regex-alist to the appropriate lists. If
;; the var was updated don't keep adding, edit the result.
(dolist (el hs-compilation-error-regex-alist)
  (unless (memq (car el) compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist (car el)))

  (let ((ael (assq (car el) compilation-error-regexp-alist-alist)))
    (if ael (setf (cdr ael) (cdr el))
      (add-to-list 'compilation-error-regexp-alist-alist el))))

(defvar fd-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x SPC") #'haskell-comint-set-breakpoint)
    (define-key map (kbd "C-c C-k") #'haskell-comint-clear-buffer)
    (define-key map (kbd "C-c C-z") #'haskell-shell-switch-to-shell)
    (define-key map (kbd "C-c C-l") #'haskell-shell-load-file)
    (define-key map (kbd "C-c C-r") #'haskell-shell-reload-last-file)
    (define-key map (kbd "C-c C-t") #'haskell-toggle-src-test)
    (define-key map (kbd "C-c C-i") #'haskell-jump-to-or-insert-defininition)
    (define-key map (kbd "C-c i") #'haskell-jump-to-or-insert-defininition)
    (define-key map (kbd "C-c >") #'fd-haskell-indent-shift-right)
    (define-key map (kbd "C-c <") #'fd-haskell-indent-shift-left)
    map)
  "Keymap for using `interactive-haskell-mode'.")

(defun fd-haskell-indent-shift-right (start end &optional count)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count) 2))
    (indent-rigidly start end count)))

(defun fd-haskell-indent-shift-left (start end &optional count)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count 2))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun fd-haskell--move-keymap-to-top (mode)
  (let ((pair (assq mode minor-mode-map-alist)))
    (if (not pair)
        (error "Map %s not member of minor-mode-map-alist")
      (setq minor-mode-map-alist
            (cons pair (assq-delete-all mode minor-mode-map-alist))))))

(defun fd-haskell-cabal-hook ()
  ;; Many tools require an up-to-date cabal file.
  (setq-local revert-without-query t))

(defun fd-haskell-find-tag-default ()
  (or (thing-at-point 'haskell-module t) (thing-at-point 'symbol t)))

;; Advice
(defun haskell-process-guard-idle (old-fn &rest r)
  (if (and (haskell-process-cmd (haskell-interactive-process))
           (= (line-end-position) (point-max)))
      (error "Haskell process busy")
    (apply old-fn r)))

(defun haskell-process-interrupt-ad (old-fn &rest r)
  (apply old-fn r)
  (haskell-process-reset (haskell-commands-process)))


(defvar-local fd-haskell--flycheck-stack-default-directory nil)
(defun flycheck-haskell--find-stack-default-directory-ad (old-fn)
  (if (null fd-haskell--flycheck-stack-default-directory)
      (setq fd-haskell--flycheck-stack-default-directory (funcall old-fn))
    fd-haskell--flycheck-stack-default-directory))

(defun fd-haskell--advise-haskell-mode ()
  (advice-add 'haskell-process-interrupt :around #'haskell-process-interrupt-ad)
  (advice-add 'haskell-interactive-handle-expr :around #'haskell-process-guard-idle)
  (advice-add 'flycheck-haskell--find-stack-default-directory
              :around #'flycheck-haskell--find-stack-default-directory-ad)
  (advice-add 'haskell-process-interrupt :around #'haskell-cabal--find-tags-dir-ad))


;;;###autoload
(define-minor-mode fd-haskell-mode
  "Some extras for haskell-mode."
  :lighter " FD-Haskell"
  :keymap fd-haskell-mode-map
  (setq comment-auto-fill-gonly-comments nil
        haskell-font-lock-symbols t
        haskell-process-args-stack-ghci nil)
  (setq haskell-tags-on-save t
        haskell-stylish-on-save t)
  (setq haskell-hoogle-command "hoogle --count=50")
  (setq-local comment-auto-fill-only-comments nil)
  (if (executable-find "hasktags")
      (setq haskell-hasktags-path (executable-find "hasktags"))
    (warn "Couln't find hasktags."))
  (yas-minor-mode nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand-from-trigger-key)
  (fd-haskell--move-keymap-to-top 'yas-minor-mode)
  ;; (flycheck-mode)
  ;; (add-hook 'flycheck-after-syntax-check-hook 'haskell-check-module-name nil t)
  (haskell-indentation-mode t)
  (haskell-decl-scan-mode 1)
  (haskell-comint-pdbtrack-mode 1)
  (rainbow-delimiters-mode 1)
  (setq-local lsp-enable-xref nil)
  (xref-etags-mode 1)
  (setq semantic-symref-tool 'grep)
  (company-mode)
  (if-let* ((adv (assq 'ghc-check-syntax-on-save
                       (ad-get-advice-info-field #'save-buffer 'after))))
      (ad-advice-set-enabled adv nil))
  (setq-local find-tag-default-function 'fd-haskell-find-tag-default)
  (font-lock-flush)
  (floskell-mode))


(defconst fd-haskell-yasnippet-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

;;;###autoload
(defun haskell-flycheck-jump-to-error-file ()
  (interactive)
    (let ((fname
           (with-current-buffer flycheck-error-message-buffer
             (save-excursion
               (goto-char (point-min))
               (search-forward-regexp "In \"\\(.*\\.hs\\)\"")
               (match-string 1)))))
      (find-file fname)
      (flycheck-clear)
      (flycheck-buffer)))
(defface fd-haskell-trace-function-face
  '((t (:background "pale green")))
  "Face for trace function calls so they stand out.")


;;;###autoload
(defun fd-haskell-configure-haskell-mode ()
  "Run once."
  (advice-add 'haskell-cabal--find-tags-dir :around #'haskell-cabal--find-tags-dir-ad)
  (add-to-list 'yas-snippet-dirs 'fd-haskell-yasnippet-snippets-dir t)
  (yas-load-directory fd-haskell-yasnippet-snippets-dir t)
  (fd-haskell--advise-haskell-mode)
  (add-hook 'haskell-mode-hook 'fd-haskell-mode)
  (remove-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-cabal-mode-hook 'fd-haskell-cabal-hook)
  (defvar-local haskell-tags-file-dir nil)
  (fset 'haskell-cabal--find-tags-dir-old (symbol-function 'haskell-cabal--find-tags-dir)))
(provide 'fd-haskell)
