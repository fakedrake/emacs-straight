(setq org-nodes-directory (expand-file-name "~/RoamNotes"))

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
  (yas-minor-mode 1)
  (visual-line-mode 1)
  (org-wc-mode 1)
  (setq-local electric-pair-pairs
              (append '((?* . ?*) (?/ . ?/) (?~ . ?~)) electric-pair-pairs))
                                        ; remove angle branckets from the syntax table. Electric pair mode
                                        ; tries to match them but they are also used as snippets `'
  (let ((stx (make-syntax-table)))
    (modify-syntax-entry ?< "_" stx)
    (modify-syntax-entry ?> "_" stx)
    (set-syntax-table stx)))

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
  :custom ((org-image-actual-width 500))
  :hook ((org-mode . fd-org-mode-hook))
  :config
  (setq org-latex-compiler "lualatex"
        org-latex-pdf-process
        '("latexmk -shell-escape -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
        org-cite-global-bibliography
        (list (concat org-nodes-directory "/bibtex.bib"))
        org-cite-export-processors '((t . (fd-natbib "unsrt" nil))))

  ;; Define fd-natbib that automatically adds the natbib bibliography
  ;; at the end of the document.
  (require 'oc)
  (require 'oc-natbib)
  (let ((natbib (org-cite--get-processor 'natbib)))
    (org-cite-register-processor 'fd-natbib
      :export-bibliography (org-cite-processor-export-bibliography natbib)
      :export-citation (org-cite-processor-export-citation natbib)
      :export-finalizer #'fd-natbib-cite-processor-finalize
      :cite-styles (org-cite-processor-cite-styles natbib)))
  (org-wc-mode 1)
  (yas-minor-mode 1)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))


(defun fd-natbib-cite-processor-finalize (output citation-keys bib-files bib-style back-end info)
  (let* ((natbib (org-cite--get-processor 'natbib))
         (out (funcall (org-cite-processor-export-finalizer natbib)
                       output citation-keys bib-files bib-style back-end info)))
    (with-temp-buffer
      (save-match-data
        (save-excursion (insert out))
        (unless (save-excursion (re-search-forward "^[[:space:]]*\\bibliograpy" nil  t))
          (when (search-forward "\\end{document}" nil t)
            (let ((bib-str (funcall (org-cite-processor-export-bibliography natbib)
                                    citation-keys bib-files bib-style nil back-end info)))
              (goto-char (match-beginning 0))
              (insert bib-str))))
        (buffer-string)))))


(use-package org-ref
  :after org
  :bind (:map org-mode-map ("C-c ]" . org-ref-insert-link))
  :config
  (setq org-ref-default-bibliography
        (list (concat org-nodes-directory "/bibtex.bib"))))

;; Org roam

;; Use org-download-clipboard to paste an image.
(use-package org-download
  :after org-roam)

(use-package ivy-bibtex
  :after org-roam
  :config
  (setq bibtex-completion-bibliography
      (list (concat org-nodes-directory "/bibtex.bib"))))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (setq org-roam-capture-templates
        '(("r" "bibliography reference" plain
          (file "/Users/cperivol/.emacs.d/roam-capture-templates/bibliography.org")
          :target
          (file+head "references/${orb-get-citekey}.org" "#+title: REF: ${title}\n")
          :unnarrowed t)
         ("d" "default" plain "%?" :target
          (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)))
  (require 'org-ref)
  (org-roam-bibtex-mode 1))

(defun orb-get-citekey (node)
  "the-citekey"
  (cdr (assq 'citekey (org-roam-node-properties node))))

(defun ltex-enable ()
  (require 'lsp-ltex)
  (lsp))

(defun clear-lsp-major-modes (client)
  (if-let ((h (gethash client lsp-clients)))
      (setf (lsp--client-major-modes h) '())))

(use-package lsp-ltex
  :ensure t
  :config
  (with-eval-after-load 'lsp-ltex
    (setq lsp-ltex-latex-commands `((,(intern "\\code{}") . "dummy")))
    (setf (lsp--client-major-modes (gethash 'ltex-ls lsp-clients))
          '(plain-tex-mode latex-mode org-mode))
    (clear-lsp-major-modes 'digestif)
    (clear-lsp-major-modes 'texlab)))

(require 'fd-thesis)
(use-package org-roam
  :straight (org-roam
             :type git :flavor melpa
             :host github :repo "org-roam/org-roam"
             :fork (:host github :repo "fakedrake/org-mode"
                          :branch "master"))
  :init
  (setq org-roam-v2-ack t)
  :custom ((org-roam-directory org-nodes-directory)
           (org-return-follows-link t)
           (org-roam-completion-everywhere t))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-x [" . org-roam-visit-parent-node))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (setup-article)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (org-roam-setup))


(defun org-roam-select-node (backlinks)
  "From a list of backlinks get the node to visit."
  (cond
   ((= (length backlinks) 1) (org-roam-backlink-source-node (car backlinks)))
   ((= (length backlinks) 0) (error "No backlinks"))
   (t (let* ((template (org-roam-node--process-display-format org-roam-node-display-template))
             (nodes (mapcar #'(lambda (b) (org-roam-node-read--to-candidate
                                       (org-roam-backlink-source-node b)
                                       template))
                            backlinks))
             (node (completing-read
                    "Backlink to follow: "
                    (lambda (string pred action)
                     (if (eq action 'metadata)
                         '(metadata
                           (annotation-function . (lambda (title)
                                                    (funcall org-roam-node-annotation-function
                                                             (get-text-property 0 'node title))))
                           (category . org-roam-node))
                       (complete-with-action action nodes string pred)))
                    nil t)))
        (cdr (assoc node nodes))))))


(defun org-roam-visit-parent-node ()
  (interactive)
  (require 'org-roam-node)
  (org-roam-node-visit
   (org-roam-select-node
    (org-roam-backlinks-get
     (org-roam-node-at-point 'assert)))))

(use-package org-transclusion
  :ensure t
  :after org
  :straight (org-transclusion
             :type git :flavor melpa
             :host github :repo "nobiot/org-transclusion"
             :fork (:host github :repo "fakedrake/org-transclusion"
                          :branch "title-as-heading"))
 :bind
  (:map org-transclusion-mode-map
        ("C-c i" . org-transclusion-add)
        ("C-c C-i" . org-transclusion-add-all)
   :map org-mode-map
   ("C-c C-i" . org-transclusion-add-all))
  :custom ((org-transclusion-include-first-section t)
           (org-transclusion-enable-recursive-add t))
  :config
  (require 'org-transclusion ))


;; brew install --build-from-source languagetool
(use-package langtool
  :commands (langtool-check
             langtool-check-done
             langtool-switch-default-language
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init
  (setq langtool-default-language "en-US"
        langtool-mother-tongue "en-US"
        langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.2.3/libexec/languagetool-commandline.jar"))

(use-package langtool-ignore-fonts
  :config
  (add-hook 'LaTeX-mode-hook 'langtool-ignore-fonts-minor-mode)
  (add-hook 'org-mode-hook 'langtool-ignore-fonts-minor-mode)
  (langtool-ignore-fonts-add 'markdown-mode '(markdown-code-face)))
