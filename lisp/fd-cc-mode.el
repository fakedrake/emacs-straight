(require 'cc-mode)
(use-package clang-format
  :bind (:map c-mode-base-map ("M-q" . 'clang-format-fill-paragraph))
  :custom ((clang-format-fallback-style "Google")))

(defun in-comment-p (&optional pos)
  "Test if character at POS is comment.  If POS is nil, character at `(point)' is tested"
  (interactive)
  (unless pos (setq pos (point)))
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      ;; learn this trick from flyspell
                      (or (eq f 'font-lock-comment-face)
                          (eq f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

(defun clang-format-fill-paragraph (&optional justify region)
  (interactive)
  (if (in-comment-p)
      (call-interactively 'fill-paragraph justify region)
    (if region
        (let ((beg (car region))
              (end (cdr region)))
          (clang-format-region beg end))
      (let ((beg (save-excursion (c-beginning-of-defun) (point)))
            (end (save-excursion (c-end-of-defun) (point))))
        (clang-format-region beg end)))))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))


(use-package clang-format
  :bind (:map c-mode-base-map ("M-q" . 'clang-format-fill-paragraph))
  :custom ((clang-format-fallback-style "Google")))

(defun in-comment-p (&optional pos)
  "Test if character at POS is comment.  If POS is nil, character at `(point)' is tested"
  (interactive)
  (unless pos (setq pos (point)))
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      ;; learn this trick from flyspell
                      (or (eq f 'font-lock-comment-face)
                          (eq f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

(defun clang-format-fill-paragraph (&optional justify region)
  (interactive)
  (if (in-comment-p)
      (call-interactively 'fill-paragraph justify region)
    (if region
        (let ((beg (car region))
              (end (cdr region)))
          (clang-format-region beg end))
      (let ((beg (save-excursion (c-beginning-of-defun) (point)))
            (end (save-excursion (c-end-of-defun) (point))))
        (clang-format-region beg end)))))

(defun c++-to-headers-mode ()
  "Change the mode of a c++ header file to c++-mode if there is
at least one .cpp file in the same directory."
  (when (and (s-ends-with? ".h" (buffer-file-name))
             (eq major-mode 'c-mode)
             (delete-if-not
              (lambda (f) (or (s-ends-with? ".cc" f) (s-ends-with? ".cpp" f)
                         (s-ends-with? ".hpp" f) (s-ends-with? ".hh" f)))
              (directory-files default-directory)))
    (c++-mode)))

(defun flycheck-no-remote ()
  (unless (file-remote-p (buffer-file-name (current-buffer)))
    (flycheck-mode 1)))

(use-package flycheck
  :config
  (setq flycheck-clang-language-standard "c++2a")
  (add-hook 'c++-mode-hook 'flycheck-no-remote)
  (add-hook 'c-mode-hook 'flycheck-no-remote))

(use-package ggtags
  ; :hook ((c++-mode-hook . ggtags-mode))
  :config
  (define-key ggtags-navigation-map "\M->" nil)
  (define-key ggtags-navigation-map "\M-<" nil)
  (setq ggtags-highlight-tag nil)
  (add-hook 'c++-mode-hook 'ggtags-mode)
  (add-hook 'c-mode-hook 'ggtags-mode))

(provide 'fd-cc-mode)
