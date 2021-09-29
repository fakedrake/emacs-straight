

(defun org-count-words (start end)
  "Count words in region skipping code blocks"
n  (let ((words 0))
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
  (org-wc-mode 1)
  (yas-minor-mode 1)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "open")))


(use-package org-ref)

;; Org roam

;; Use org-download-clipboard to paste an image.
(use-package org-download
  :after org-roam)

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))


(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))


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

(defun org-roam-setup-captures ()
  (("d" "default" plain "%?" :if-new
    (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
    :unnarrowed t)

   ("t" "A note about the fluidb thesis." ; Description
    plain ; the kind
    "%?"  ; the template (just put the cursor in an empty file)
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                       "#+title: ${title}\n#+filetags: thesis")
    :jump-to-captured t
    :unnoarrowed t)))
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
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (org-roam-setup))
