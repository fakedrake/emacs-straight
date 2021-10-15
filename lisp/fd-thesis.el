(defconst fd-theorem-header
  "\\usepackage{amsthm}
\\newtheorem{definition}{Definition}[section]
\\newtheorem{theorem}{Theorem}
")

(defun setup-article ()
  (setq org-latex-default-class "fd-article")
  (setq org-latex-default-figure-position "H")

  (require 'ob-latex)
  ; (setq org-latex-packages-alist '(("" "amsthm")))
  ;; XXX: ensure the fonts are all there.
  ;; For arch that would be:
  ;; pacman -S ttf-dejavu ttf-freefont.
  ;;
  ;; Usage: on top of the .org doc put these.
  ;; #+LaTeX_CLASS: fakedrake-org-article
  ;; #+LaTeX_HEADER: <some extra headings>
  (add-to-list 'org-latex-classes
	       '("thesis"
		 "\\documentclass[11pt,a4paper]{report}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
	       `("fd-article"
                ,(concat "\\documentclass[11pt]{article}\n" fd-theorem-header)
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-packages-alist
               '("" "tikz" t))
 ; (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)
  (setq org-export-filter-link-functions '(fd-link-filter))
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((latex . t)))
 (add-to-list 'org-latex-packages-alist '("" "minted"))
 (setq org-latex-listings 'minted))

(use-package graphviz-dot-mode)

(defun setup-dot2tex ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (latex . t)
     (haskell . t)
     (js . t)
     (dot . t)
     (dot2tex . t)
     (algorithmic . t)))
  (setq org-transclusion-exclude-elements nil)
  (add-to-list 'org-src-lang-modes '("dot2tex" . graphviz-dot)))

; (org-export-filter-src-block-functions)

(defun fd-link-filter (data-str backend-sym info)
  (save-match-data
    (and (eq backend-sym 'latex)
         (with-temp-buffer
           (insert data-str)
           (goto-char (point-min))
           (when (re-search-forward "\\\\href{.*.org}{\\(.\\|\n\\)*}" nil t)
             (concat (match-string 1) " "))))))

(provide 'fd-thesis)
