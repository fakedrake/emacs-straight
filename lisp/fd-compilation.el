;;; fd-compilation.el
;; Stuff related to compilation.
(require 's)
(require 'notifications)
(defmacro with-function-args (func-and-args &rest body)
  "FUNC-AND-ARGS is a cons of function symbol and a function that
accepts the same args as the function corresponding to car but
returns non-nil if the function is to be called."
  (lexical-let* ((lexical-binding t)
                 (validate-args (cdr func-and-args))
                 (func-symbol (car func-and-args))
                 (real-func (symbol-function func-symbol)))
    (cl-flet ((wrapped-func (&rest args)
                            (if (apply validate-args args)
                                (apply real-func args)
                              (throw 'func-call-signal 'function-called))))
      `(let ((counter 1) (ret))
         (while (and
                 (> counter 0)
                 ;; Run the body and check that 'function-called was thrown
                 (eq 'function-called
                     (catch 'func-call-signal
                       ;; When evaluating func-symbol use wrapped-func
                       (cl-letf (((symbol-function ,func-symbol) ,(symbol-function #'wrapped-func)))
                         (setq ret (progn ,@body))))))
           (setq counte r (- counter 1)))
         (when (<= counter 0)
           (error (concat "Failed to run body without call to " (symbol-name))))
         ret))))

(defmacro with-no-function (func-sym &rest body)
  "Instead of calling function FUNC-SYM in BODY, retry."
  `(let ((counter 10))
     (while (and
             (> counter 0)
             (eq 'function-called
                 (catch 'func-call-signal
                   (cl-letf (((symbol-function ,func-sym)
                              (lambda (&rest args)
                                (throw 'func-call-signal 'function-called))))
                     ,@body))))
       (setq counter (- counter 1)))
     (when (<= counter 0)
       (error (concat "Failed to run body without call to " (symbol-name))))))

(defun compilation-end-defun (compilation-buffer result)
  (with-current-buffer compilation-buffer
    (if (string= (buffer-name) "*compilation*")
        (notifications-notify
         :title (format "Compilation: %s" result)
         :body (format "Cmd: %s" compile-command))
      (notifications-notify
       :title (format "Finished: %s" (buffer-name))))))

(setq compilation-finish-function 'compilation-end-defun)

(defun mapcdr (fn seq)
  (and seq (cons (funcall fn seq) (mapcdr fn (cdr seq)))))

(defun eval-or-nil (fn val)
  (when (funcall fn val) val))

(defun fd-file-in-directory-p (file dir)
  (let* ((true-dir (file-truename dir))
         (default-directory (concat true-dir "/"))
         (true-file (file-truename file)))
    (and (file-exists-p true-file)
         (file-directory-p true-dir)
         (string-prefix-p true-dir true-file))))

(defun fd-project-root (dir-or-file &optional marker)
  "Get the closest ancestor directory containing
  marker-file. marker-file defaults to \".git\""
  (let ((dir (file-truename
              (if (file-directory-p dir-or-file)
                  dir-or-file
                (file-directory-name dir-or-file))))
        (is-root (or (when (functionp marker) marker)
                     (apply-partially 'fd-file-in-directory-p (or marker ".git")))))
    (car
     (remove-if 'null
                (mapcdr
                 (lambda (dl)
                   ;; Keep only the dirs that contain a .dir-locals.el
                   (let ((ds (concat "/" (s-join "/" (reverse dl)))))
                     (when (funcall is-root ds)
                       ds))) (nreverse (s-split "/" dir t)))))))

(require 'recur)
(defun or-exists (&rest paths)
  (recur-let
   ((paths paths))
   (when paths
     (if (and (car paths) (file-directory-p (car paths)))
         (car paths)
       (recur (cdr paths))))))

(defun fd-compilation-root (filename)
  (file-name-as-directory
   (or-exists (when (boundp 'compile-root) compile-root)
              (fd-project-root filename ".dir-locals.el")
              (fd-project-root filename)
              filename)))

(defun fd-recompile ()
  (interactive)
  (save-excursion
    (hack-local-variables)
    (let ((default-directory
            (fd-compilation-root default-directory)))
      (message "Compiling with %s in %s" compile-command default-directory)
      (compilation-start compile-command t))))

(defun fd-last-error ()
  "Jump to last error."
  (interactive)
  (with-current-buffer (compilation-find-buffer)
    (setq-local compilation-current-error (point-max))
    (with-no-function 'read-file-name (previous-error))))

(defun fd-compile (command directory)
  "Save the compilation to either the closest .dir-locals.el or
the current directory. Then run compilation."
  (interactive (list
                (read-shell-command "Compile command: "
                                    compile-command)
                (read-directory-name "Root directory: "
                                     (fd-compilation-root
                                      (or (buffer-file-name)
                                          default-directory)))))
  (save-excursion
    (fd-save-compilation
     (or (fd-project-root default-directory ".dir-locals.el") default-directory)
     directory command)
    (let ((default-directory (fd-normalize-dir directory)))
      (fd-recompile))))

(defun fd-normalize-dir (directory)
  (if (s-ends-with-p "/" directory) directory (concat directory "/")))

(defun fd-trim (c str)
  (apply 'string
         (nreverse
          (fd-trim-internal
           c (nreverse
              (fd-trim-internal
               c (string-to-list str)))))))

(defun fd-trim-internal (c lst)
  (if (and lst (= (car lst) c)) (fd-trim-internal c (cdr lst)) lst))

(defun fd-path-concat (&rest path-fragments)
  (concat "/"
          (fd-trim ?/
                   (mapconcat
                    (apply-partially 'fd-trim ?/)
                    path-fragments "/"))))

(setq enable-remote-dir-locals t)
(defun fd-save-compilation (project-root compilation-root command)
  (find-file (fd-path-concat project-root ".dir-locals.el"))
  (save-buffer)
  (add-dir-local-variable nil 'compile-command command)
  (add-dir-local-variable nil 'compile-root compilation-root)
  (save-buffer)
  (bury-buffer))

(global-set-key (kbd "C-c r") 'fd-recompile)
(global-set-key (kbd "C-c c c") 'fd-compile)
(global-set-key (kbd "M-g n") 'fd-next-error)
(global-set-key (kbd "C-x `") 'fd-next-error)
(global-set-key (kbd "M-g p") 'fd-previous-error)
(global-set-key (kbd "M-g l") 'fd-last-error)
(global-set-key (kbd "M-g t") 'error-end-of-trace)

(defun fd-next-error (&optional arg reset)
  (interactive)
  (with-no-function 'read-file-name (next-error arg reset)))

(defun fd-previous-error (&optional n)
  (interactive)
  (with-no-function 'read-file-name (previous-error n)))

(defcustom error-trace-max-distance 2
  "Maximum distance between errors of a trace.")

(defun next-error-point (&optional pt reverse)
  "The point of the next error from point. Nil if no error after
this."
  (save-excursion
    (condition-case e
        (progn
          (compilation-next-error (if reverse -1 1) nil (or pt (point)))
          (point))
      ('error nil))))

(defun error-end-of-trace (&optional reverse)
  "Jump to the last error of the next trace. If reverse jump to
the top."
  (interactive)
  (pop-to-buffer (next-error-find-buffer))
  ;; Enter the next trace. Should raise error if we are at the end
  (compilation-next-error 1 nil (or compilation-current-error (point-min)))

  ;; Move to it's end
  (let ((le (internal-end-of-trace (point) reverse)))
    (goto-char le)
    (recenter)
    (compile-goto-error)))

(defun internal-end-of-trace (pt reverse)
  "Find the last error o a trace that we are in."
  (let ((nep (next-error-point pt reverse)))
    ;; There is an error and it is close.
    (if (and nep (<= (count-lines pt nep)
                     error-trace-max-distance))
        (internal-end-of-trace nep reverse)
      pt)))


;; A nice stack for pdb would be:
;;
;; - create buffer with fd-compilation
;; - set inferior-python-mode
;; - use that buffer to initialize gud-pdb
;;
;; To do that I will replace make-comint while running pdb that should
;; create a buffer named

;; (defadvice make-comint (around ad-create-a-gud-buffer first activate)
;;   "if `create-gud-buffer-fn' is non-nil. Use that insetaed of the
;;   standard way. Do not use this outside of the provided macros."
;;   (let ((cbuffer (or gud-buffer
;;                      (funcall 'gud-buffer))))
;;     (if cbuffer
;;         (with-current-buffer cbuffer (rename-buffer (ad-get-arg 1)))
;;       ad-do-it)))

(defvar gud-buffer nil
  "Either a function that evaluates to a buffer or a buffer we
  can switch to. This will override the buffer creation mechanism
  of gud.")

(defmacro gud-recomple (gud-command &rest args)
  (let ((gud-buffer (apply 'fd-recompile args)))
    (funcall gud-command "compilation")))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun fd-next-error-hook ()
  (message "Next!"))

(defun add-images () )

;; (defun add-images ()
;;   (iimage-recenter))

(defun fd-iimage-hook ()
  (add-hook 'compilation-filter-hook 'add-images))

(add-hook 'iimage-mode-hook 'fd-iimage-hook)
(add-hook 'compilation-mode-hook 'turn-on-visual-line-mode)

(defun compilation-send-signal (sig)
  (interactive "nSignal to send: ")
  (if-let* ((proc (get-buffer-process (current-buffer))))
      (signal-process proc sig)
    (error "No process in current buffer.")))

(provide 'fd-compilation)
