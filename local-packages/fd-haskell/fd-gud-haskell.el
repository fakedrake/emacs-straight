;; GUD
;;
;; M-x gud-ghci<RET>stack ghci

(defun gud-display-frame ()
  "Find and obey the last filename-and-line marker from the debugger.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (flet ((col-pos (col) (save-excursion (beginning-of-line) (+ col (point)))))
    (when gud-last-frame
      (gud-set-buffer)
      ;; gud-last-frame => (file . line)
      (cond
       ((not (listp (cdr gud-last-frame)))
        (gud-display-line (car gud-last-frame) (cdr gud-last-frame)))
       ;; gud-last-frame => (file line begin-column end-column)
       ((and
         (= 4 (length gud-last-frame))
         (every #'numberp (cdr gud-last-frame)))
        (let* ((file (car gud-last-frame))
               (file-buf (find-file-noselect file t))
               (line (cadr gud-last-frame))
               (expr-begin-col (caddr gud-last-frame))
               (expr-end-col (cadddr gud-last-frame)))
          (gud-display-line file line)
          (with-current-buffer file-buf
            (let ((expr-begin (col-pos expr-begin-col))
                  (expr-end  (col-pos expr-end-col))
                  (pulse-delay .30))
              (message (concat "Expr " (buffer-substring expr-begin expr-end)))
              (pulse-momentary-highlight-region expr-begin expr-end)))))
       ;; TODO: gud-last-frame =>
       ;; (file (begin-line . begin-column) (end-line . end-column))
       ;; Anything else
       (t (error "Unknown gud-last-frame format.")))
      (setq gud-last-last-frame gud-last-frame
	    gud-last-frame nil))))

(defun gud-ghci-marker-filter (string)
  (setq gud-marker-acc (if gud-marker-acc (concat gud-marker-acc string) string))

  (let (start)
    ;; Process all complete markers in this chunk.
    (while (string-match
	    "\\(Logged breakpoint at\\|Stopped in [^ \t\r\n]+,\\) \\(?1:[^ \t\r\n]+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)\\(?:-\\(?4:[0-9]+\\)\\|\\)"
	    gud-marker-acc start)
      (setq gud-last-frame
	    (list (match-string 1 gud-marker-acc)
		  (string-to-number (match-string 2 gud-marker-acc))
                  (string-to-number (match-string 3 gud-marker-acc))
                  (string-to-number (match-string 4 gud-marker-acc)))
	    start (match-end 0)))

    ;; Search for the last incomplete line in this chunk
    (while (string-match "\n" gud-marker-acc start)
      (setq start (match-end 0)))

    ;; If the incomplete line APPEARS to begin with another marker, keep it
    ;; in the accumulator.  Otherwise, clear the accumulator to avoid an
    ;; unnecessary concat during the next call.
    (setq gud-marker-acc (substring gud-marker-acc (match-beginning 0))))
  string)

(defun gud-ghci ()
  "Run the gud interpreter."
  (interactive)
  (when (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'ghci)))
    (gdb-restore-windows)
    (error
     "Multiple debugging requires restarting in text command mode"))
   (gud-common-init command-line nil 'gud-ghci-marker-filter)
   (set (make-local-variable 'gud-minor-mode) 'pdb)
   (setq paragraph-start comint-prompt-regexp)
   (gud-def gud-break  ":break %m %l %y" "\C-b" "Set breakpoint at current line.")
   ;; TODO: put _result=... line to minibuffer.
   (gud-def gud-stepi  ":step"           "\C-s" "Step one source line with display.")
   (gud-def gud-step   ":stepmodule"     "\C-n" "Step in the module.")
   (gud-def gud-next   ":steplocal"      "n" "Step in the local scope.")
   (gud-def gud-cont   ":continue"       "\C-r" "Continue with display.")
   (gud-def gud-up     ":back"           "<" "Up one stack frame.")
   (gud-def gud-down   ":forward"        ">" "Down one stack frame.")
   (gud-def gud-run    ":trace %e"       "t" "Trace expression.")
   (gud-def gud-print  ":print %e"       "\C-p" "Evaluate Guile expression at point.")

   (setq gdb-first-prompt t)
   (setq gud-running nil)
   (run-hooks 'gud-ghci-mode-hook))

(defvar gud-ghci-command-name "stack repl")

(defun haskell-debug-parse-stopped-at (string)
  "Parse the location stopped at from the given string.

For example:

Stopped in Main.main, /home/foo/project/src/x.hs:6:25-36

"
  (let ((index (string-match "Stopped in [^ ]+, \\([^:]+\\):\\(.+\\)\n?"
                             string)))
    (when index
      (list :path (match-string 1 string)
            :span (haskell-debug-parse-span (match-string 2 string))
            :types (cdr (haskell-debug-split-string (substring string index)))))))

(provide 'fd-gud-haskell)
