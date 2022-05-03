;; Colorize issues with the writing in the buffer.
;; (use-package writegood-mode
;;   :bind ("C-c g" . writegood-mode)
;;   :config
;;   (add-to-list 'writegood-weasel-words "actionable"))

<<<<<<< HEAD
=======


(use-package pdf-tools
  :ensure t
  :init (add-to-list 'exec-path (straight--build-dir "pdf-tools"))
  :config
  (pdf-tools-install)
  (setq pdf-misc-print-programm "/usr/bin/lpr"
        pdf-misc-print-programm-args (quote ("-o media=A4" "-o fitplot -o sides=two-sided-long-edge")))
  (setq pdf-view-use-scaling t))

>>>>>>> github/master
(defun fd-tex-hook ()
  ;; REMEMBER TO SET TeX-master in .dir-locals.el so that the latex
  ;; commands will work properly.
  ; We always have graphicx, luaextra and amsmath even if they are not
  ; in the current file
  (TeX-run-style-hooks "article" "luaextra" "graphics" "amsmath" "minted")
  ;; Set the project root
  (add-to-list 'TeX-tree-roots (expand-file-name (project-root (project-current t))))
  (TeX-source-correlate-mode 1)
  ;; Inline code
  (loop
   for lang in '("code" "hask" "sql" "cpp" "py")
   do
   (font-latex-add-keywords `((,lang "{")) 'italic-command)
   (font-latex-add-keywords `((,lang "{")) 'variable))

  (add-to-list 'LaTeX-verbatim-environments "minted")
  (add-to-list 'LaTeX-verbatim-environments "haskellcode")
  (add-to-list 'LaTeX-verbatim-environments "pycode")
  (add-to-list 'LaTeX-verbatim-environments "sqlcode")
  (add-to-list 'LaTeX-verbatim-environments "cppcode")
  ; Use latexmk
  (set (make-local-variable 'indent-line-function) #'fd-auctex-code-indent-line))

(use-package lua-mode)

(use-package reftex
  :ensure t
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
  (setq-default TeX-master nil)
  (setq TeX-command-default "latexmk")
  (push '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t :help "Run latexmk on file")
        TeX-command-list)
  (push '("build.sh" "./build.sh" TeX-run-TeX nil t :help "Run a build script")
        TeX-command-list)

  ; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.

  (define-key LaTeX-mode-map (kbd "C-c '") 'LaTeX-edit-code-start)
  (add-hook 'TeX-mode-hook 'fd-tex-hook)

  (setq TeX-complete-expert-commands t)
  (setq preview-auto-reveal t)
  (message "Setup of latex done"))

(use-package company-auctex
  :ensure t
  :hook (TeX-mode . company-auctex-init))
<<<<<<< HEAD

(unless (eq system-type 'windows-nt)
  (use-package pdf-tools
    :config
    (pdf-tools-install)
    (setq pdf-misc-print-programm "/usr/bin/lpr"
          pdf-misc-print-programm-args (quote ("-o media=A4" "-o fitplot -o sides=two-sided-long-edge")))
    (setq pdf-view-use-scaling t)))
=======
>>>>>>> github/master
