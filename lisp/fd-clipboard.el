;; CLIPBOARD
(require 'simple)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value
      select-enable-clipboard t
      x-select-enable-clipboard t)

(defun clipboard-contents-normal (filename is-directory line-info)
  (let ((linum-sep (if github-prefix "#L" ":"))
	(uri (if github-prefix
		 (concat github-prefix (git-relative-filename filename))
	       filename)))
    (if (and (not is-directory) line-info)
	(concat uri linum-sep (int-to-string (current-line)))
      uri)))

(defun git-root-directory (fname)
  "FNAME is an absolute path of a file. Get the furthest back"
  (if (or (not fname) (file-exists-p (concat fname "/.git"))) fname
    (git-root-directory (mapconcat
			 'identity
			 (reverse (cdr (reverse (split-string fname "/")))) "/"))))

(defun git-relative-filename (filename)
  (car (split-string filename (concat (git-root filename) "/") t)))

(defvar-local github-prefix nil
  "If defined it is the prefix to be added to git")

(defvar clipboard-contents-fn 'clipboard-contents-normal
  "Given LINE-INFO non-nil return a string to be copied to clipboard")

(defun my-put-file-name-on-clipboard (&optional arg)
  "Put the current file name on the clipboard. With prefix copy
have <fname>:<linum>"
  (interactive "P")
  (let* ((directory (equal major-mode 'dired-mode))
	 (filename (file-truename
		    (if directory
			default-directory
		      (buffer-file-name))))
	 (clip (funcall clipboard-contents-fn filename directory arg)))
    (when filename
      (save-excursion
	(with-temp-buffer
	  (insert clip)
	  (clipboard-kill-region (point-min) (point-max)))
	(message (format "Copied '%s'" clip))))))

(global-set-key (kbd "C-x y") 'my-put-file-name-on-clipboard)
