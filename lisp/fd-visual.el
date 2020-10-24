;; Visual Settings
(defun fullscreen ()
  "Set the fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; Zoom
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute
   'default nil :height
   (+ (face-attribute 'default :height)
      (* n 5)))
  (message (format "Font size: %d"
		   (face-attribute 'default :height))))


(defun fd-new-buf-extension (dir &optional default path-len)
  "Return either the last two directories, or default"
  (if dir
      (let* ((dirs ))
	(mapconcat
         (lambda (s) (format "%s/" s))
         (reverse (seq-take (reverse (split-string dir "/" t)) 2))
         nil))
    default))

(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (when buffer-file-name
    (add-to-list 'mode-line-buffer-identification
		 '(:eval (fd-new-buf-extension default-directory "unlinked: ")))))

(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#1e2223"))))))
  (buffer-face-set 'default))


(use-package naquadah-theme
  :demand t
  :hook '((find-file . add-mode-line-dirtrack)
          (buffer-list-update-hook . highlight-selected-window))
  :config
  (message "initializing themere")
  (global-set-key (kbd "M-+") #'(lambda nil (interactive) (djcb-zoom 1)))
  (global-set-key (kbd "M--") #'(lambda nil (interactive) (djcb-zoom -2)))

  (let ((comment "IndianRed2"))
    (custom-theme-set-faces
     'naquadah
     `(default ((t (:family "Fira Code" :background "#262B2C" :height 95))))
     `(mode-line ((t (:height 1.1 :background "gray30"))))
     `(highlight ((t (:background nil :underline t :weight bold))))
     `(minibuffer-prompt ((t (:foreground "orange1"))))
     `(region ((t (:background "gray35"))))
     `(hl-line ((t (:background "gray25"))))
     `(linum ((t (:inherit default :background "#0c191C" :foreground "gray50"))))
     `(haskell-debug-newline-face ((t (:inherit linum))))
     `(haskell-debug-trace-number-face ((t (:inherit linum))))
     `(comint-highlight-prompt ((t (:inherit font-lock-function-name-face))))
     ;; Development
     `(font-lock-comment-face ((t (:foreground ,comment))))
     `(font-lock-function-name-face ((t (:foreground "orange1" :bold t))))
     `(font-lock-doc-face ((t (:foreground ,comment))))
     `(font-lock-doc-string-face ((t (:foreground ,comment))))
     `(link ((t (:foreground  "#729fcf" :underline t))))
     `(highlight ((t (:background nil :underline t :weight bold))))
     `(highlight-symbol-face ((t (:underline t))))))
  (setq-default cursor-type 'box)
  (setq-default
   frame-title-format
   (list '((buffer-file-name " %f" (dired-directory
                                    dired-directory
                                    (revert-buffer-function " %b"
                                                            ("%b -
                                                          Dir: "
                                                             default-directory)))))))

  (line-number-mode 1)	; have line numbers and
  (column-number-mode 1)	; column numbers in the mode line
  (mouse-avoidance-mode 'banish)
  (tool-bar-mode -1)	; no tool bar with icons
  (menu-bar-mode -1)
  (scroll-bar-mode -1)	; no scroll bars
  (add-hook 'find-file-hook (lambda () (setq show-trailing-whitespace t)))
  (global-linum-mode 1)	; add line numbers on the left
  (show-paren-mode t))


(provide 'fd-visual)
