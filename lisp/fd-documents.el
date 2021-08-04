;; Colorize issues with the writing in the buffer.
(use-package writegood-mode
  :bind ("C-c g" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))

(defun org-count-words (start end)
  "Count words in region skipping code blocks"
  (let ((words 0))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (forward-word-strictly 1)
          (when
           (save-excursion
             (back-to-indentation)
             (cond ((looking-at "#\\+begin_") t)
                   ((looking-at "#") (forward-line) nil)
                   (t (setq words (1+ words)) nil)))
           (unless (re-search-forward "^[ \n]*#\\+end_src" nil t)
             (goto-char (point-max)))))))
    words))

;; add length display to mode-line construct
(setq mode-line-misc-info
      (append
       (assq-delete-all 'org-wc-mode mode-line-misc-info)
       '((org-wc-mode
	  (1 (:eval (word-count-msg)))
	  nil))))

(define-minor-mode org-wc-mode
  "Toggle word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of words is displayed in the
mode-line.")



(defun word-count-msg ()
  (if (use-region-p)
      (format " region words:%d" (org-count-words (point) (mark)))
    (format " words:%d"
            (org-count-words (point-min) (point-max)))))

(defun fd-org-mode-hook ()
  (yas-minor-mode)
  (org-wc-mode 1))

(setq fd-todo-highlights
      '(("\\[todo:\\([^\\]]+?\\)\\]" . (1 font-lock-constant-face))))


(define-minor-mode fd-org-highlights
  "Highlight varous things in org mode."
  nil
  " fd-hl"
  nil
  (font-lock-add-keywords nil '("\\[todo:[\\]\\]" . 'error))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("<C-tab>" . yas-expand)
              ("M-j" . 'org-meta-return)
              ("C-c C-j" . 'org-meta-return)
              ("S-<left>" . 'windmove-left)
              ("S-<right>" . 'windmove-right)
              ("<C-tab>". 'yas-expand))
  :hook ((org-mode . fd-org-mode-hook))
  :config
  (org-wc-mode 1)
  (yas-minor-mode 1)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "open")))


(setq langtool-jar "/usr/local/Cellar/languagetool/5.2.3/libexec/languagetool.jar")
(use-package languagetool
  :config
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)

  (setq languagetool-server-language-tool-jar "/usr/local/Cellar/languagetool/5.2.3/libexec/languagetool-server.jar")
  (setq languagetool-language-tool-jar langtool-jar))
(use-package org-ref)


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


(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom ((org-roam-directory "~/RoamNotes")
           (org-return-follows-link t)
           (org-roam-completion-everywhere t))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))
