(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-x G" . magit-status)
         ("C-x g c" . magit-dispatch-popup))
  :config
  (magit-auto-revert-mode -1))


;; Define faces.
(defface fd/mode:vc-added
  `(
    (  ((class color))
       (:background "#FFAA55"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that have just been added to
version-control."
  :group 'FD/mode)

(defface fd/mode:vc-edited
  `(
    (  ((class color))
       (:background "#F05B80"  :foreground "black")  )   ; "#F04040" maybe?
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are under version control
but which have been edited."
  :group 'FD/mode)

(defface fd/mode:vc-in-sync
  `(
    (  ((class color))
       (:background "#60CC60"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are under version control
and which are in sync with the respository."
  :group 'FD/mode)

(defface fd/mode:vc-none
  `(
    (  ((class color))
       (:background "#70A0D0"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are not under version
control"
  :group 'FD/mode)

(defface fd/mode:vc-unknown
  `(
    (  ((class color))
       (:background "#FF0000"  :foreground "white")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files whose version-control status
cannot be determined."
  :group 'FD/mode)

(defvar fd-vc-mode-attrs
  '((""  . (" ∅ "  fd/mode:vc-none))
    ("-" . (" = "  fd/mode:vc-in-sync))
    (":" . (" ≢ "  fd/mode:vc-edited))
    ("@" . (" + "  fd/mode:vc-added))
    ("?" . (" ? "  fd/mode:vc-unknown))
    )
  "Lookup table to translate vc-mode character into another string/face."
  )



;; This function helps me understand the version-control status.
(defun fd-mode-line-vc-info ()
  "Return version-control status information about the file in
the current buffer, as a fontified string.

The mode-line variable `vc-mode' is nil if the file is not under
version control, and displays a hyphen or a colon depending on whether
the file has been modified since check-in.  I can never keep those
straight.

This function returns \"NoVC\" if the file is not under version
control.  It displays a string with an = sign if the file is in sync
with its version control, and a string with a > sign if the file has
been modified since its last check-in."
  (let* ((class
          (cond
           ;; If not under version-control
           ((not vc-mode)
            "")

           ;; If under version-control decode the -:@ character
           ((string-match "\\` ?\\(?:CVS\\|Git\\)\\([-:@]\\)\\([^^:~ \x00-\x1F\\\\/]+\\)?" vc-mode)
            (match-string-no-properties 1 vc-mode))

           ;; Otherwise, indicate confusion
           (t
            "?")
           ))

         (branch
          (if (-any-p (lambda (x) (string= x class)) '("-" ":" "@"))
              (concat " " (match-string-no-properties 2 vc-mode))
            ""))

         ;; Fetch properties list for the class character above
         (props (cdr (assoc class fd-vc-mode-attrs)))
         )

    (concat (propertize (car props) 'face (cadr props))
            branch)))
(add-to-list 'mode-line-format '(:eval (fd-mode-line-vc-info)))
(provide 'fd-git)
