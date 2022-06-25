(require 'cl-lib)
(cl-delete-if (lambda (x) (equal "plinkh" (car x))) tramp-methods)
(add-to-list 'tramp-methods
             `("plinkh"
               (tramp-login-program        "plink")
               (tramp-login-args           (("-load") ("%h") ("-t")
                                            ("env")
				            (,(format "'TERM=%s'" tramp-terminal-type))
                                            ("'PROMPT_COMMAND='")
                                            (,(format "'PS1=%s'" tramp-initial-end-of-output))
                                            ("/bin/bash")
                                            ("-lc")
                                            ("%l")))
               (tramp-remote-shell         ,tramp-default-remote-shell)
               (tramp-remote-shell-login   ("-l"))
               (tramp-remote-shell-args    ("-c"))))

(remove-hook 'tramp-cleanup-connection-hook #'tramp-recentf-cleanup)
(remove-hook 'tramp-cleanup-all-connection-hook #'tramp-recentf-cleanup-all)
