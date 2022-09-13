(defun haskell-collect-imports (&optional buffer)
  (let ((buf (or buffer (current-buffer))) (ret nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^import .*$" nil t)
        (setq ret (cons (buffer-substring (match-beginning 0) (match-end 0)) ret))))
    ret))

(defun haskell-send-imports-internal (process imports done)
  (if (null imports) (and done (funcall done))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list (car imports) process (cdr imports) done)
      :go (lambda (state)
            (message (concat "Importing: " (car state)))
            (haskell-process-send-string (cadr state) (car state)))
      :live (lambda (state response) nil)
      :complete (lambda (state response)
                  (apply 'haskell-send-imports-internal (cdr state)))))))


(defvar-local fd-haskell-last-loaded-file nil)
(defvar-local fd-haskell-loading-status nil)
(defvar-local fd-haskell-restarted nil)
(defun haskell-process-restart-ad (old-fn &rest r)
  "Reset local variables that are continget to the state of the
ghci process."
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (setq-local fd-haskell-last-loaded-file nil)
    (setq-local fd-haskell-loading-status nil))
  (apply old-fn r))

(advice-add 'haskell-process-restart :around #'haskell-process-restart-ad)

(defun haskell-load-or-reload-file ()
  (interactive)
  (let* ((ibuf (haskell-session-interactive-buffer (haskell-session)))
         (src-file (buffer-file-name))
         (last-loaded-file
          (with-current-buffer ibuf fd-haskell-last-loaded-file)))
    (cond
     ((eq fd-haskell-loading-status 'reload)
      (error "Haskell process is reloading file: %s" ))
     ((eq fd-haskell-loading-status 'load)
      (error "Haskell process is reloading file: %s" ))
     ((and (haskell-process) (haskell-process-cmd (haskell-process)))
      (error "Haskell process is busy."))
     ((string= (buffer-file-name) last-loaded-file)
      (haskell-interactive-mode-echo
       (haskell-session) (format "Reloading file: %s" (buffer-file-name)))
      (with-current-buffer ibuf
        (setq-local fd-haskell-loading-status 'reload))
      (call-interactively 'haskell-process-reload)
      (with-current-buffer ibuf
        (setq-local fd-haskell-loading-status 'reload)))
     (t
      (haskell-interactive-mode-echo
       (haskell-session) (format "Loading file: %s" (buffer-file-name)))
      (with-current-buffer ibuf
        (setq-local fd-haskell-last-loaded-file src-file)
        (setq-local fd-haskell-loading-status 'load))
      (call-interactively 'haskell-process-load-file)
      (with-current-buffer ibuf
        (setq-local fd-haskell-loading-status nil))))))

(defun haskell-process-load-file-and-then-imports ()
  (interactive)
  (call-interactively 'haskell-process-load-file)
  (haskell-send-imports (haskell-process)))

(defun haskell-send-imports (process &optional buffer done)
  (haskell-send-imports-internal process (haskell-collect-imports buffer) done))

(defun fd-haskell-load-region ()
  (interactive)
  (if (not (region-active-p)) (call-interactively haskell-process-load-file)
    (let* ((tmp (concat (make-temp-file "haskell-region") ".hs"))
           (haskell-session (haskell-session))
           (cmd (format "load \"%s\"" (replace-regexp-in-string
                                       "\""
                                       "\\\\\""
                                       tmp))))
      (write-region (region-beginning) (region-end) tmp)
      (haskell-process-file-loadish cmd nil buf)
      (delete-file tmp))))

(defun fd-haskell-mode-jump-to-def-or-tag-lenient (&optional _next-p)
  (interactive)
  (flet-wrap
   ((haskell-ident-at-point (old) (or (old) (symbol-at-point))))
   (haskell-mode-jump-to-def-or-tag)))

(defun fd-haskell-interactive-mode-hook ()
  (define-key haskell-interactive-mode-map
    (kbd "M-.") 'fd-haskell-mode-jump-to-def-or-tag-lenient))

(add-hook 'haskell-interactive-mode-hook 'fd-haskell-interactive-mode-hook)

(provide 'fd-haskell-interactive)
