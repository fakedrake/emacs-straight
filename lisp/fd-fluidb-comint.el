(defface fluidb-comint-before-face
  '((t (:foreground "red1" :weight bold)))
  "Before tags face"
  :group 'fluidb-comint-highlight-mode-group)
(defface fluidb-comint-after-face
  '((t (:foreground "green1" :weight bold)))
  "After tags face"
  :group 'fluidb-comint-highlight-mode-group)
(defface fluidb-comint-node-face
  '((t (:background "yellow" :foreground "black")))
  "After tags face"
  :group 'fluidb-comint-highlight-mode-group)

(defvar fluidb-comint-highlight-keywords
  '(("\\[Before\\]" . 'fluidb-comint-before-face)
    ("\\[After\\]" . 'fluidb-comint-after-face)
    ("\\<N[0-9]+\\>" . 'fluidb-comint-node-face)))

(define-minor-mode fluidb-comint-highlight-mode
  "Highlight common patterns in fluidb stdoupt"
  :lighter " fluidb-hl"
  :global nil
  :group 'fluidb-comint-highlight-mode-group
  (if fluidb-comint-highlight-mode
      (font-lock-add-keywords nil fluidb-comint-highlight-keywords)
    (font-lock-remove-keywords nil fluidb-comint-highlight-keywords))
  (font-lock-flush))

(provide 'fd-fluidb-comint)
