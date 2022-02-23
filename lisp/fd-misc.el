;; Miscelaneous settings
;;
;; Do not include anything that requires anything that isnt packaged
;; with emacs here.

;; Server configuration
(use-package s)
(require 'server)
(if (server-running-p)
    (message "Skipping server creation, one already exists")
  (server-start))

;; Configurations
(delete-selection-mode t)
(setq backup-directory-alist (list (cons "." (concat user-emacs-directory "backup/"))))
(setq kill-do-not-save-duplicates t)
(set-input-method 'greek)
(toggle-input-method)
(cua-mode -1)
(setq scroll-step 1)
(global-set-key (kbd "C-z") 'revert-buffer)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd"C-c C-d") 'duplicate-line-or-region)
(global-set-key (kbd"C-c d") 'duplicate-line-or-region)

;; If wou dont have the above path, i feel 'forward is best.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


(defun chmod+x-this ()
  "Add executable permissions to the current file."
  (interactive)
  (if buffer-file-name
      (let ((new-mode (logior #o111 (file-modes buffer-file-name))))
        (set-file-modes buffer-file-name new-mode))
    (message "No such file to make executable.")))

;; Rename files
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(setq require-final-newline t)

(defun file-directory-name (fname)
  "Get the parent dir of the fname. If fname is a dir get the
parent."
  (replace-regexp-in-string "[^/]+$" ""
                            (directory-file-name fname)))

(eval-after-load 'tramp '(setenv "SHELL" (executable-find "bash")))

(defadvice save-buffer (around save-buffer-as-root-around activate)
  "Use sudo to save the current buffer."
  (interactive "p")
  (if (and (buffer-file-name)
	   (file-accessible-directory-p
	    (file-directory-name (buffer-file-name)))
	   (not (file-writable-p (buffer-file-name)))
	   (not (s-starts-with-p buffer-file-name "/sudo")))
      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
	ad-do-it)
    ad-do-it))

(defun move-text-block-internal (start end lines)
  (let* ((text (buffer-substring start end))
         (rel-mark (and (mark) (- (mark) end)))
         (move-mark (and rel-mark mark-active transient-mark-mode))
         (rel-point (- (point) end)))
    (delete-region start end)
    (forward-line lines)
    (insert text)
    ;; Restore mark and point
    (let ((new-end (point)))
      (goto-char (+ new-end rel-point))
      (when move-mark
        (pop-mark)
        (push-mark (+ new-end rel-mark))
        (activate-mark)))))

(defun move-text-internal (arg)
  (if (and mark-active transient-mark-mode)
      (if (> (point) (mark))
	  (move-text-block-internal (mark) (point) arg)
        (move-text-block-internal (point) (mark) arg))
    (move-text-block-internal
     (line-beginning-position) (+ 1 (line-end-position)) arg)))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "C-M-p") 'move-text-up)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "C-M-n") 'move-text-down)

(global-set-key (kbd "C-c f d") 'find-dired)

(when (fboundp 'old-y-or-n-p)
  (defalias 'y-or-n-p 'old-y-or-n-p))

(defalias 'yes-or-no-p 'y-or-n-p
  "Faster yes or no's")

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this-window (selected-window))
	 (other-window (next-window))
	 (this-buffer (window-buffer this-window))
	 (other-buffer (window-buffer other-window)))
    (set-window-buffer other-window this-buffer)
    (set-window-buffer this-window other-buffer)
    (select-window other-window)))

(global-set-key (kbd "C-x TAB") 'swap-buffers-in-windows)
(setq dired-dwim-target t)

(add-hook 'text-mode
	  (lambda ()
	    (define-key text-mode-map (kbd "M-n") 'forward-paragraph)))


(setq auto-save-timeout 0
      auto-save-interval 0)

(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)

(unless (boundp 'pcache-version-constant)
  (setq pcache-version-constant "fake-version"))

;; PATH in emacs
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (error "set-exec-path-from-shell-PATH only works for unix."))
  (let ((path-from-shell
         (car (reverse (split-string (shell-command-to-string "echo -n $PATH") ":")))))
    (setenv "PATH" path-from-shell)
    (setenv "EDITOR" "emacsclient")
    (setenv "EMACS" (s-trim (shell-command-to-string "echo $EMACS")))
    (setq exec-path (split-string path-from-shell path-separator))))

;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)
(global-prettify-symbols-mode 1)
; (set-exec-path-from-shell-PATH)
(global-unset-key (kbd "M-`"))

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(global-set-key (kbd "M-q") 'fill-paragraph)
(global-set-key (kbd "C-h a") 'apropos)

(use-package grep
  :config
  (progn
    (add-to-list 'grep-find-ignored-directories ".cabal-sandbox")
    (add-to-list 'grep-find-ignored-directories "node_modules")))

(setq enable-recursive-minibuffers t)
(setq enable-local-variables :all)

(setq gdb-display-buffer-other-frame-action
  '(display-buffer-reuse-window
    (reusable-frames . nil) ; Only reuse current frame
    (inhibit-same-window . t)))

(add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin/"))
(add-to-list 'exec-path (concat (getenv "HOME") "/.nix-profile/bin"))
(setq pop-up-frames nil)
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" display-buffer-reuse-window
               ((reusable-frames . t))))

(electric-indent-mode -1)
(savehist-mode 1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(unless (eq 'windows-nt system-type)
  (setenv "PATH" (shell-command-to-string "echo -n $PATH")))

(setq reb-re-syntax 'rx)

(rx-define path-base () (+ (in word ?_ ?- ?.)))
(rx-define path-ext (ext)
  (seq symbol-start (? ?/) (+ (path-base) ?/) (path-base) ?. ext symbol-end))
(rx-define path-ext-linum (ext grp-path grp-line)
  (: (group-n grp-path (path-ext ext)) ?: (group-n grp-line (+ (in digit)))))

(rx-define path-ext-linum-maybe-col (ext grp-path grp-linum grp-col)
  (: (path-ext-linum ext grp-path grp-linum) (? ?: (group-n grp-col (+ (in digit))))))

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(use-package helpful
  :demand
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))


(defun fd-indent (begin-rx end-rx)
  (save-excursion
    (let ((offset 0))
      (while (< (point) (point-max))
        (cond
         ((looking-at begin-rx) (indent-line-to offset) (setq offset (+ 2 offset)))
         ((looking-at end-rx) (setq offset (- offset 2)) (indent-line-to offset))
         (t (indent-line-to offset)))
        (forward-line)))))

(use-package dired-hacks)

(setq print-length nil
      print-level nil)

(defun expand-file-name-remote (file)
  "A tramp-friendly version of expand-file-name.  Expand file
relative to the remote part of default-directory."
  (let ((file (expand-file-name file))
        (dir-remote (file-remote-p default-directory)))
    (if (file-remote-p file)
        ;; if file is already remote, return it
        file
      ;; otherwise prepend the remote part (if any) of default-directory
      (concat dir-remote file))))

(defun around-ad-executable-find (orig-fn cmd &optional is-remote)
  (let ((remote-cmd (if is-remote (expand-file-name-remote cmd) cmd)))
    (if (file-executable-p remote-cmd)
        cmd
      (funcall orig-fn cmd is-remote))))
(advice-add 'executable-find :around 'around-ad-executable-find)

;; Galeon is this old browser that usded to be supported by browse-url
;; but now it was discontinued and helm-net has not caught up to this
;; deprecation.
(defmacro browse-url-define-browser (deprecated-browser)
  (message "defining browser %s" deprecated-browser)
  `(progn
     (defcustom ,(intern (format "browse-url-%s-program" deprecated-browser)) ,deprecated-browser
       "The name by which to invoke Galeon."
       :type 'string)

     (defcustom ,(intern (format "browse-url-%s-arguments" deprecated-browser)) nil
       "A list of strings to pass to Galeon as arguments."
       :type '(repeat (string :tag "Argument")))

     (defcustom ,(intern (format "browse-url-%s-startup-arguments" deprecated-browser))
       browse-url-galeon-arguments
       "A list of strings to pass to Galeon when it starts up.
Defaults to the value of `browse-url-galeon-arguments' at the time
`browse-url' is loaded."
       :type '(repeat (string :tag "Argument")))))

(defmacro define-deprecated-browsers ()
  `(progn
     ,@(cl-loop for depr-browser in '("galeon" "mozilla" "netscape")
                collect `(browse-url-define-browser ,depr-browser))))

(define-deprecated-browsers)
