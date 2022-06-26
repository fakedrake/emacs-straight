;;; auctex-code.el --- Code editing support for AUCTeX
;;
;; Copyright (C) 2021 Christos Perivolaropoulos
;;
;; Author: Christos Perivolaropoulos (christos.perivolaropoulos@huawei.com)
;; Version: 1.0
;; Package-Requires: ((auctex "11.86"))
;; URL: http://github.com/fakedrake/auctex-code
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to
;;
;;     The Free Software Foundation, Inc.
;;     51 Franklin Street, Fifth Floor
;;     Boston, MA, 02110-1301
;;     USA.
;;
;; Keywords: latex
;;
;;; Commentary:
;;
;; This package provides some basic utilities for working with code
;; from within AUCTeX.  `LaTeX-edit-code-start' is the entry point of
;; this package; bind it to your favorite key and use it inside of any
;; environment in `LaTeX-code-environments'.  To commit your changes
;; to the parent buffer and return to it, simply use `save-buffer' (or
;; whichever key it is bound to).  The contents of the parent buffer
;; will be updated and the buffer will be killed.
;;
;; Beware!  Editing embedded code is asynchronous.  If you kill the
;; buffer that was editing it, your changes will be lost!  In a future
;; update I will add a `yes-or-no-p' confirmation to killing the
;; buffer, but I've yet to figure that one out.

;;; Code:

(defgroup LaTeX-code nil
  "Code support in AUCTeX."
  :group 'LaTeX)


(defvar-local LaTeX-edit-code-parent-buffer nil)
(defvar-local LaTeX-edit-code-parent-buffer-point nil)


(defun LaTeX-mark-environment-contents ()
  "Mark the contents of the innermost LaTeX environment.  See
also `LaTeX-find-matching-begin' and `LaTeX-find-matching-end'."
  (interactive)
  ;; Search for the end of the current environment.
  (LaTeX-find-matching-end)
  ;; Then place a mark.
  (push-mark (search-backward "\\end"))
  ;; Search for the beginning of the current environment.
  (LaTeX-find-matching-begin)
  ;; Search for the end of the \begin{...}
  (search-forward "}"))

(defun fd-auctex-code-indent-line ()
  "Do not indent line when in code"
  (interactive)
  (let ((env (LaTeX-current-environment)))
    (if (and env
             (assq (intern env) LaTeX-code-environments)
             (not (looking-at " *\\end *{ *\\([A-Za-z*]+\\) *}")))
        'noindent
      (LaTeX-indent-line))))

;;;###autoload
(defcustom LaTeX-code-environments
  '((luacode . lua-mode)
    (luacode* . lua-mode)
    (pycode . python-mode)
    (haskellcode . haskell-mode)
    (sqlcode . sql-mode))
  "An alist of symbols for envs and corresponding modes"
  :group 'LaTeX-code
  :type '(alist :key-type symbol :value-type symbol))

;;;###autoload
(defun LaTeX-edit-code-start ()
  "Place code in a separate buffer in the corresponding modex."
  (interactive)
  ;; If we are in a code environment,
  (if-let* ((env (LaTeX-current-environment))
            (mode (cdr (assq (intern env) LaTeX-code-environments))))
      ;; Make a few notes about where we are and what we're looking at
      (let* ((code-parent-buffer (current-buffer))
             (code-where-edit-started (point))
             (code-buffer-name (format "*%s [%s]*" (buffer-name) env))
             (code-buffer (get-buffer-create code-buffer-name)) ; prepare buffer
             (this-code (progn (LaTeX-mark-environment-contents) ; get code
                               (buffer-substring-no-properties (point) (mark)))))
        ;; Switch to the code buffer we just created.
        (switch-to-buffer-other-window code-buffer)
        (funcall mode)
        (auctex-code-mode)
        (insert (concat (string-trim-right this-code) "\n"))
        (LaTeX-edit-code--unindent-buffer)
        (beginning-of-buffer)
        (while (and (< (point) (- (point-max) 1)) (looking-at " *$"))
          (delete-region (point) (1+ (match-end 0))))
        ;; Record some buffer-local variables
        (setq-local LaTeX-edit-code-parent-buffer code-parent-buffer)
        (setq-local LaTeX-edit-code-parent-buffer-point code-where-edit-started))
    (message "Not in a known code environment.")))

(defun LaTeX-edit-code--unindent-buffer ()
  (let ((min-indent))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (unless (looking-at " *$")
          (setq min-indent
                (if min-indent
                    (min min-indent (current-indentation))
                  (current-indentation))))
        (forward-line))
      (when min-indent
        (goto-char (point-min))
        (while (< (point) (point-max))
          (beginning-of-line)
          (if (looking-at " *$")
              (delete-region (point) (match-end 0))
            (delete-char min-indent))
          (forward-line))))))

(defun mk-column-at (point)
  "Return column number at POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun LaTeX-edit-code--indent-string (str)
  "Indent a string 2 spaces deeper than the the innermost environment"
  (let ((off (+ 2 (save-excursion
                    (LaTeX-find-matching-begin)
                    (mk-column-at (point))))))
    (concat (mapconcat (lambda (x) (concat (s-repeat off " ") x))
                       (s-lines str)
                       "\n"))))

(defun LaTeX-edit-code-finish ()
  "Dump the contents of the current buffer into the
buffer/environment it was called from, replacing what is
currently in that environment.

Note that this function isn't too smart yet; it does not
intelligently handle a scenario where other editing has taken
place in the parent buffer.  If point has moved into a different
environment, that environment will be replaced instead of its
original one.  Remember, you can always `undo' your changes.  See
`LaTeX-mark-environment-contents'."
  (interactive)
  ;; 'Ensure' this is a code buffer that was made with
  ;; `LaTeX-edit-code-start'
  (if (bufferp LaTeX-edit-code-parent-buffer)
      ;; Grab the code
      (let* ((buf LaTeX-edit-code-parent-buffer)
             (pt LaTeX-edit-code-parent-buffer-point)
             (the-code (progn (widen)
                              (LaTeX-edit-code--trim
                               (buffer-substring (point-min)
                                                 (point-max))))))
        ;; Kill the buffer
        (kill-buffer-and-window)
        ;; and switch to its parent
        (switch-to-buffer buf)
        (save-excursion
          ;; Mark the current environment
          (LaTeX-mark-environment-contents)
          ;; and replace it
          (delete-region (point) (mark))
          ;; with the code
          (insert "\n" (LaTeX-edit-code--indent-string the-code) "\n"))
        ;; and return point to its rightful place
        (goto-char pt))
    (message "%s  %s"
             "Something went wrong."
             "Am I *really* in a buffer created with `LaTeX-edit-code-finish'?")))


(defun LaTeX-edit-code--trim (str)
  (replace-regexp-in-string "^\n+" ""
                            (replace-regexp-in-string "\n+$"  "" str)))

(define-minor-mode auctex-code-mode
  "Like prog-mode but can exit"
 ;; The initial value.
 :init-value nil
 ;; The indicator for the mode line.
 :lighter " Tex-code"
 ;; The minor mode bindings.
 :keymap
 (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'LaTeX-edit-code-finish)
            map))


(provide 'fd-auctex-code)
;;; auctex-code.el ends here
