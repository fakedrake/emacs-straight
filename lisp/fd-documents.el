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
  (word-count-msg 1))


(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map ("<C-tab>" . yas-expand))
  :hook (org-mode . fd-org-mode-hook)
  :custom
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "open")))


(use-package org-ref)
