;; Colorize issues with the writing in the buffer.
(use-package writegood-mode
  :bind ("C-c g" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))


(defun fd-tex-hook ()
  ;; REMEMBER TO SET TeX-master in .dir-locals.el so that the latex
  ;; commands will work properly.
  ; We always have graphicx, luaextra and amsmath even if they are not
  ; in the current file
  (TeX-run-style-hooks "article" "luaextra" "graphics" "amsmath")

  ;; Set the project root
  (add-to-list 'TeX-tree-roots (expand-file-name (project-root (project-current t))))

  ;; Inline code
  (font-latex-add-keywords '(("code" "{")) 'italic-command)
  (font-latex-add-keywords '(("code" "{")) 'variable)

  ; Use latexmk
  (setq TeX-command-default "latexmk")
  (push '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file")
        TeX-command-list))

(use-package lua-mode)

(use-package reftex
  :ensure
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-bibpath-environment-variables '("./" "./bib/" "./ref" "./reference"))
  ;(setq reftex-default-bibliography '("~/Documents/research_papers/bibtex.bib"))
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))


(use-package latex
  :ensure t
  :straight auctex
  :config
  (setq-default TeX-engine 'luatex)
  (setq TeX-PDF-mode t)
  (require 'fd-auctex-code)

  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.

  (define-key LaTeX-mode-map (kbd "C-c '") 'LaTeX-edit-code-start)
  (add-hook 'TeX-mode-hook 'fd-tex-hook)

  (setq TeX-complete-expert-commands t)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (setq preview-auto-reveal t)
  (message "Setup of latex done"))

(use-package company-auctex
  :ensure t
  :hook (TeX-mode . company-auctex-init))
