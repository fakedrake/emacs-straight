(require 'cc-mode)
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))


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

(defun fd-cc-mode-init ()
  (c++-to-headers-mode))

(use-package flycheck
  :config
  (setq flycheck-clang-language-standard "c++2a")
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))

(provide 'fd-cc-mode)
