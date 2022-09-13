;;;###autoload
(setq haskell-font-lock-symbols-alist
      '(("Integer" . "ℤ")
        ("Bool" . "𝔹")
        ("Rational" . "ℚ")
        ("++" . "⧺")
        (">>" . "⨠")
        ("<>" . "•")
        ("\\" . "λ")
        ("not" . "¬")
        ("->" . "→")
        ("<-" . "←")
        ("=>" . "⇒")
        ("()" . "∅")
        ("==" . "≡")
        ("/=" . "≢")
        (">=" . "≥")
        ("<=" . "≤")
        ("!!" . "‼")
        ("&&" . "∧")
        ("||" . "∨")
        ("sqrt" . "√")
        ("undefined" . "⊥")
        ("pi" . "π")
        ("~>" . "⇝") ;; Omega language
        ;; ("~>" "↝") ;; less desirable
        ("-<" . "↢") ;; Paterson's arrow syntax
        ;; ("-<" "⤙") ;; nicer but uncommon
        ("::" . "∷")
        ("." "∘" ; "○"
         ;; Need a predicate here to distinguish the . used by
         ;; forall <foo> . <bar>.
         haskell-font-lock-dot-is-not-composition)
        ("forall" . "∀")))
(provide 'fd-haskell-ligatures)
