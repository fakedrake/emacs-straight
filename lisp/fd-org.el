

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

(use-package lsp-ltex
  :ensure t
  :config
  (with-eval-after-load 'lsp-ltex
    (setq lsp-ltex-latex-commands `((,(intern "\\code{}") . "dummy")))
    (setf (lsp--client-major-modes (gethash 'ltex-ls lsp-clients))
          '(plain-tex-mode latex-mode org-mode))
    (setf (lsp--client-major-modes (gethash 'digestif lsp-clients))
          '())
    (setf (lsp--client-major-modes (gethash 'texlab lsp-clients))
          '())))  ; or lsp-deferred