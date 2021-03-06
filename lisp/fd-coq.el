(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :bind (:map coq-mode-map ("C-c C-l" . proof-process-buffer))
  :config
  (setq coq-prettify-symbols-alist
      '(("[[" . #x2726)
        ("]]" . #x27e7)
        ("->" . ?→)
        ("exists" . ?∃)
        ("=>" . ?⇒)
        ("forall" . ?∀)
        (">>" . ?»)
        ("|-" . ?⊦)
        ("||-" . ?⊩)
        ("|=" . ?⊧)
        ("|<=" . ?⊑)
        ("\\/" . ?∨)
        ("/\\" . ?∧)
        ("{{" . #x2983)
        ("}}" . #x2984))))
