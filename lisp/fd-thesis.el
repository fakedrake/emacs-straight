(defcustom fd-latex-header
  "\\usepackage{amsthm}
\\newtheorem{definition}{Definition}[section]
\\newtheorem{theorem}{Theorem}
\\usepackage{fontspec}
\\newcommand{\\fntsize}{9}
\\setmonofont{FiraCode}[Scale=0.8]
"
  "The latex header")

(defcustom fd-latex-extra-commands
  "\\usepackage{cancel}
\\newcommand{\\lsemi}{\\ltimes}
\\newcommand{\\rsemi}{\\rtimes}
\\newcommand{\\lnsemi}{\\cancel\\ltimes}
\\newcommand{\\rnsemi}{\\cancel\\rtimes}
"
  "some extra commands")


(defun setup-article ()
  (setq org-latex-default-class "fd-article")
  (setq org-latex-default-figure-position "H")
  (setq org-format-latex-header
        (concat "\\documentclass[preview]{standalone}
\\usepackage[usenames]{color}
\[PACKAGES]
\[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}\n"))

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
                 ,(concat "\\documentclass[11pt]{article}\n"
                          fd-latex-header
                          fd-latex-extra-commands)
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-packages-alist
               '("" "tikz" t))
  ;; Long captions are horrible to write:
  ;; https://emacs.stackexchange.com/questions/20720/figures-with-multiline-captions-in-org-mode
  (setq org-export-filter-link-functions '(fd-link-filter))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)))
  (setq latex-minted-options '("breaklines"))

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "cancel"))
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
