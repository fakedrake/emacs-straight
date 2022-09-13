;;; fd-haskell-comint --- Comint interaction with ghci
;;; Commentary:
;;; A lot of this was stolen from python-mode
;;; Code:
(require 'comint)
(require 'tramp)
(require 'tramp-sh)
(autoload 'comint-mode "comint")

(defgroup haskell-comint nil
  "Haskell comint."
  :link '(custom-manual "(haskell-mode)Haskell comint interaction")
  :group 'haskell
  :prefix "haskell-comint-")

(defcustom haskell-shell-internal-buffer-name "Haskell Internal"
  "Default buffer name for the Internal Haskell interpreter."
  :type 'string
  :group 'haskell-comint
  :safe 'stringp)

(defcustom haskell-shell-exec-path nil
  "List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
  :group 'haskell-comint)

(defcustom haskell-shell-remote-exec-path nil
  "List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
`haskell-shell-exec-path' will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Haskell shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
  :group 'haskell-comint)

(defcustom haskell-shell-process-environment nil
  "List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
When this variable is non-nil, values are exported into the
process environment before starting it.  Any variables already
present in the current environment are superseded by variables
set here."
  :type '(repeat string)
  :group 'haskell-comint)

(defcustom haskell-shell-prompt-input-regexps
  '("> ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :group 'haskell-comint
  :version "24.4")

(defcustom haskell-shell-prompt-regexp "> "
  "Regular expression matching top level input prompt of Haskell shell.
It should not contain a caret (^) at the beginning."
  :group 'haskell-comint
  :type 'string)

(defcustom haskell-shell-prompt-block-regexp "| "
  "Regular expression matching block input prompt of Haskell shell.
It should not contain a caret (^) at the beginning."
  :group 'haskell-comint
  :type 'string)

(defcustom haskell-shell-prompt-debug-regexp "> "
  "Regular expression matching debugging input prompt of Haskell shell.
It should not contain a caret (^) at the beginning."
  :group 'haskell-comint
  :type 'string)

(defcustom haskell-shell-first-prompt-hook nil
  "Hook run upon first shell prompt detection.
This is the place for shell setup functions that need to wait for
output.  Since the first prompt is ensured, this helps the
current process to not hang while waiting.  This is useful to
safely attach setup code for long-running processes that
eventually provide a shell."
  :version "25.1"
  :type 'hook
  :group 'haskell-comint)

(defcustom haskell-shell-compilation-regexp-alist
  '(("^\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\(-[0-9]*\\|\\)\\): error:$" 2 3 4 2 1)
    ("^\\(\\(.*\\.l?hs\\):\\([0-9]*\\):\\([0-9]*\\)\\(-[0-9]*\\|\\)\\): warning:" 2 3 4 1 1)
    ("^[[:space:]]+[[:word:]]+, called at \\(\\([[:word:]]*/\\)*[[:word:]]+\\.hs\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) in "
    1 3 4))
  "`compilation-error-regexp-alist' for inferior Haskell."
  :type '(alist string)
  :group 'haskell-comint)

(defcustom haskell-shell-font-lock-enable t
  "Should syntax highlighting be enabled in the Haskell shell buffer?
Restart the Haskell shell after changing this variable for it to take effect."
  :type 'boolean
  :group 'haskell-comint
  :safe 'booleanp)

(defcustom haskell-shell-buffer-name "GHCi"
  "Default buffer name for Haskell interpreter."
  :type 'string
  :group 'haskell-comint
  :safe 'stringp)

(defcustom haskell-shell-interpreter '("stack" "repl")
  "Default Haskell interpreter for shell."
  :type 'string
  :group 'haskell-comint)

(defcustom haskell-shell-interpreter-args
  '("--no-build" "--no-load")
  "Default arguments for the Haskell interpreter."
  :type 'string
  :group 'haskell-comint)

(defun haskell-combine-and-quote-strings (strs)
  (if (stringp strs) (list strs) (combine-and-quote-strings strs)))

(defun haskell-shell-calculate-command ()
  "Calculate the string used to execute the inferior Haskell process."
  (format "%s %s"
          ;; `haskell-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          (haskell-combine-and-quote-strings haskell-shell-interpreter)
          (haskell-combine-and-quote-strings haskell-shell-interpreter-args)))

;;;###autoload
(defun haskell-comint-clear-buffer (&optional process msg)
  "Clear the comint buffer."
  (interactive)
  (with-current-buffer
      (process-buffer
       (or process (haskell-shell-get-process-or-error msg)))
    (comint-clear-buffer)))

;;;###autoload
(defun haskell-comint-restart (&optional process)
  "Restart the comint process."
  (interactive)
  (let ((proc (or process (haskell-shell-get-process))))
    (when proc
      (with-current-buffer (process-buffer proc)
        (comint-kill-subjob))))
  (sleep-for 1)
  (run-haskell-comint))

;;;###autoload
(defun run-haskell-comint (&optional cmd dedicated show)
  "Run an inferior Haskell process.

Argument CMD defaults to `haskell-shell-calculate-command' return
value.  When called interactively with `prefix-arg', it allows
the user to edit such value and choose whether the interpreter
should be DEDICATED for the current buffer.  When numeric prefix
arg is other than 0 or 4 do not SHOW.

For a given buffer and same values of DEDICATED, if a process is
already running for it, it will do nothing.  This means that if
the current buffer is using a global process, the user is still
able to switch it to use a dedicated one.

Runs the hook `haskell-comint-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Haskell: " (haskell-shell-calculate-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (haskell-shell-calculate-command) nil t)))
  (get-buffer-process
   (haskell-shell-make-comint
    (or cmd (haskell-shell-calculate-command))
    (haskell-shell-get-process-name dedicated) show)))

(defun haskell-shell-calculate-process-environment ()
  "Calculate `process-environment' or `tramp-remote-process-environment'.
Prepends `haskell-shell-process-environment', sets extra
haskellpaths from `haskell-shell-extra-haskellpaths' and sets a few
virtualenv related vars.  If `default-directory' points to a
remote host, the returned value is intended for
`tramp-remote-process-environment'."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment)))
    (dolist (env haskell-shell-process-environment)
      (pcase-let ((`(,key ,value) (split-string env "=")))
        (setenv key value)))
    process-environment))

(defmacro haskell-shell--add-to-path-with-priority (pathvar paths)
  "Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun haskell-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      (haskell-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))


(defun haskell-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from `tramp-open-connection-setup-interactive-shell'.
  (let ((env (append (when (fboundp #'tramp-get-remote-locale)
                       ;; Emacs<24.4 compat.
                       (list (tramp-get-remote-locale vec)))
		     (copy-sequence env)))
        (tramp-end-of-heredoc
         (if (boundp 'tramp-end-of-heredoc)
             tramp-end-of-heredoc
           (md5 tramp-end-of-output)))
	unset vars item)
    (while env
      (setq item (split-string (car env) "=" 'omit))
      (setcdr item (mapconcat 'identity (cdr item) "="))
      (if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
	  (push (format "%s %s" (car item) (cdr item)) vars)
	(push (car item) unset))
      (setq env (cdr env)))
    (when vars
      (tramp-send-command
       vec
       (format "while read var val; do export $var=$val; done <<'%s'\n%s\n%s"
	       tramp-end-of-heredoc
	       (mapconcat 'identity vars "\n")
	       tramp-end-of-heredoc)
       t))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " ")) t))))

(defun haskell-shell-calculate-exec-path ()
  "Calculate `exec-path'.
Prepends `haskell-shell-exec-path' and adds the binary directory
for virtualenv if `haskell-shell-virtualenv-root' is set - this
will use the haskell interpreter from inside the virtualenv when
starting the shell.  If `default-directory' points to a remote host,
the returned value appends `haskell-shell-remote-exec-path' instead
of `exec-path'."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       haskell-shell-remote-exec-path
                     exec-path))))
    (haskell-shell--add-to-path-with-priority
     new-path haskell-shell-exec-path)
    new-path))

(defmacro haskell-shell-with-environment (&rest body)
  "Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`haskell-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (haskell-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (haskell-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (haskell-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (haskell-shell-tramp-refresh-remote-path
            ,vec (haskell-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (haskell-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))

(defun haskell-shell-make-comint (cmd proc-name &optional show internal)
  "Create a Haskell shell comint buffer.
CMD is the Haskell command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `haskell-comint-mode' is activated.  When
optional argument SHOW is non-nil the buffer is shown.  When
optional argument INTERNAL is non-nil this process is run on a
buffer with a name that starts with a space, following the Emacs
convention for temporary/internal buffers, and also makes sure
the user is not queried for confirmation when the process is
killed."
  (save-excursion
    (haskell-shell-with-environment
      (let* ((proc-buffer-name
              (format (if (not internal) "*%s*" " *%s*") proc-name)))
        (when (not (comint-check-proc proc-buffer-name))
          (let* ((cmdlist (split-string-and-unquote cmd))
                 (interpreter (car cmdlist))
                 (args (cdr cmdlist))
                 (buffer (apply #'make-comint-in-buffer proc-name proc-buffer-name
                                interpreter nil args))
                 (process (get-buffer-process buffer))
                 ;; Users can override the interpreter and args
                 ;; interactively when calling `run-haskell-comint', let-binding
                 ;; these allows having the new right values in all
                 ;; setup code that is done in `haskell-comint-mode',
                 ;; which is important, especially for prompt detection.
                 (haskell-shell--interpreter interpreter)
                 (haskell-shell--interpreter-args
                  (mapconcat #'identity args " ")))
            (with-current-buffer buffer
              (set (make-local-variable 'haskell-shell--loaded-file) nil)
              (message "$ %s" cmd)
              (haskell-comint-mode))
            (when show (display-buffer buffer))
            (and internal (set-process-query-on-exit-flag process nil))))
        proc-buffer-name))))


(defun haskell-shell-get-process ()
  "Return inferior Haskell process for current buffer."
  (get-buffer-process (haskell-shell-get-buffer)))

(defun haskell-shell-get-buffer ()
  "Return inferior Haskell buffer for current buffer.
If current buffer is in `haskell-comint-mode', return it."
  (if (derived-mode-p 'haskell-comint-mode)
      (current-buffer)
    (let* ((dedicated-proc-name (haskell-shell-get-process-name t))
           (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
           (global-proc-name  (haskell-shell-get-process-name nil))
           (global-proc-buffer-name (format "*%s*" global-proc-name))
           (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
           (global-running (comint-check-proc global-proc-buffer-name)))
      ;; Always prefer dedicated
      (or (and dedicated-running dedicated-proc-buffer-name)
          (and global-running global-proc-buffer-name)))))

(defun haskell-shell-get-process-or-error (&optional interactivep)
  "Return inferior Haskell process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (haskell-shell-get-process)
      (if interactivep
          (user-error
           "Start a Haskell process first with `M-x run-haskell-comint'")
        (error "No inferior Haskell process running"))))

(defun haskell-shell-get-process-name (dedicated)
  "Calculate the appropriate process name for inferior Haskell process.
If DEDICATED is t returns a string with the form
`haskell-shell-buffer-name'[`buffer-name'] else returns the value
of `haskell-shell-buffer-name'."
  (if dedicated
      (format "%s[%s]" haskell-shell-buffer-name (buffer-name))
    haskell-shell-buffer-name))

(defun haskell-shell-get-or-create-process (&optional cmd dedicated show)
  "Get or create an inferior Haskell process for current buffer and return it.
Arguments CMD, DEDICATED and SHOW are those of `run-haskell-comint' and
are used to start the shell.  If those arguments are not
provided, `run-haskell-comint' is called interactively and the user will
be asked for their values."
  (let ((shell-process (haskell-shell-get-process)))
    (when (not shell-process)
      (if (not cmd)
          ;; XXX: Refactor code such that calling `run-haskell-comint'
          ;; interactively is not needed anymore.
          (call-interactively 'run-haskell)
        (run-haskell-comint cmd dedicated show)))
    (or shell-process (haskell-shell-get-process))))


(defvar haskell-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior haskell shell.
Do not set this variable directly, instead use
`haskell-shell-prompt-set-calculated-regexps'.")

(defvar haskell-shell--block-prompt nil
  "Input block prompt for inferior haskell shell.
Do not set this variable directly, instead use
`haskell-shell-prompt-set-calculated-regexps'.")

(defun haskell-shell-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.
Build and set the values for `haskell-shell-input-prompt-regexp'
and `haskell-shell-output-prompt-regexp' using the values from
`haskell-shell-prompt-regexp', `haskell-shell-prompt-block-regexp',
`haskell-shell-prompt-debug-regexp',
`haskell-shell-prompt-input-regexps' and detected prompts from
`haskell-shell-prompt-detect'."
  (when (not haskell-shell--prompt-calculated-input-regexp)
    (let* ((input-prompts nil)
           (build-regexp
            (lambda (prompts)
              (concat "^\\("
                      (mapconcat #'identity
                                 (sort prompts
                                       (lambda (a b)
                                         (let ((length-a (length a))
                                               (length-b (length b)))
                                           (if (= length-a length-b)
                                               (string< a b)
                                             (> (length a) (length b))))))
                                 "\\|")
                      "\\)"))))
      ;; Validate ALL regexps
      (haskell-shell-prompt-validate-regexps)
      ;; Collect all user defined input prompts
      (dolist (prompt (append haskell-shell-prompt-input-regexps
                              (list haskell-shell-prompt-regexp
                                    haskell-shell-prompt-block-regexp
                                    haskell-shell-prompt-debug-regexp)))
        (cl-pushnew prompt input-prompts :test #'string=))
      ;; Set input and output prompt regexps from collected prompts
      (setq haskell-shell--prompt-calculated-input-regexp
            (funcall build-regexp input-prompts)))))

(defun haskell-shell-prompt-validate-regexps ()
  "Validate all user provided regexps for prompts.
Signals `user-error' if any of these vars contain invalid
regexps: `haskell-shell-prompt-regexp',
`haskell-shell-prompt-block-regexp',
`haskell-shell-prompt-debug-regexp',
`haskell-shell-prompt-input-regexps'"
  (dolist (symbol (list 'haskell-shell-prompt-input-regexps
                        'haskell-shell-prompt-regexp
                        'haskell-shell-prompt-block-regexp
                        'haskell-shell-prompt-debug-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (haskell-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in `%s'"
                    regexp symbol)))))

(defun haskell-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))

(defvar haskell-shell--interpreter nil)
(defvar haskell-shell--interpreter-args nil)

(defvar haskell-shell--block-prompt nil
  "Input block prompt for inferior haskell shell.
Do not set this variable directly, instead use
`haskell-shell-prompt-set-calculated-regexps'.")

(defvar haskell-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior haskell shell.
Do not set this variable directly, instead use
`haskell-shell-prompt-set-calculated-regexps'.")


(defvar haskell-shell--first-prompt-received-output-buffer nil)
(defvar haskell-shell--first-prompt-received nil)
(defun haskell-shell-comint-watch-for-first-prompt-output-filter (output)
  "Run `haskell-shell-first-prompt-hook' when first prompt is found in OUTPUT."
  (when (not haskell-shell--first-prompt-received)
    (set (make-local-variable 'haskell-shell--first-prompt-received-output-buffer)
         (concat haskell-shell--first-prompt-received-output-buffer
                 (ansi-color-filter-apply output)))
    (when (haskell-shell-comint-end-of-output-p
           haskell-shell--first-prompt-received-output-buffer)
      (set (make-local-variable 'haskell-shell--first-prompt-received) t)
      (setq-local haskell-shell--first-prompt-received-output-buffer nil)
      (save-current-buffer
        (let ((inhibit-quit nil))
          (run-hooks 'haskell-shell-first-prompt-hook)))))
  output)

(defvar haskell-shell--is-no-show nil)
(defun haskell-shell-comint-non-show-to-type-filter (output)
  "Replace an non-Show value with it's type.
When OUTPUT begins with 'No instance for (?Show[ \n]', skip it
and run :t <expr>."
  (when (string-match "No instance for (?Show[ \n]" output)
    (set (make-local-variable 'haskell-shell--is-no-show) t))
  (cond
   ((and haskell-shell--is-no-show
         (haskell-shell-comint-end-of-output-p output))
    (concat (haskell-shell-send-string-no-output
             (format ":t %s" (comint-previous-input-string 0)))
            (haskell-shell-comint-end-of-output-p output)))
   (haskell-shell--is-no-show "")
   (t (set (make-local-variable 'haskell-shell--is-no-show) nil) output)))

(defun haskell-shell-comint-end-of-output-p (output)
  "Return if OUTPUT ends with input prompt return the input prompt."
  (let ((start
         (string-match
          ;; XXX: It seems on macOS an extra carriage return is attached
          ;; at the end of output, this handles that too.
          (concat
           "\r?\n?"
           ;; Remove initial caret from calculated regexp
           (replace-regexp-in-string
            (rx string-start ?^) ""
            haskell-shell--prompt-calculated-input-regexp)
           (rx eos))
          output)))
    (when start (substring output start))))

(defun haskell-comint-postoutput-scroll-to-bottom (output)
  "Faster version of `comint-postoutput-scroll-to-bottom'.
Avoids `recenter' calls until OUTPUT is completely sent."
  (when (and (not (string= "" output))
             (haskell-shell-comint-end-of-output-p
              (ansi-color-filter-apply output)))
    (comint-postoutput-scroll-to-bottom output))
  output)

(defvar haskell-shell-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  )

(defvar haskell-shell--last-buffer nil)

(defvar haskell-shell-internal-last-output nil
  "Last output captured by the internal shell.")

(define-derived-mode haskell-comint-mode comint-mode "Inferior Haskell"
  "Major mode for Haskell inferior process.
Runs a Haskell interpreter as a subprocess of Emacs, with Haskell
I/O through an Emacs buffer.  Variables `haskell-shell-interpreter'
and `haskell-shell-interpreter-args' control which Haskell
interpreter is run.  Variables
`haskell-shell-prompt-regexp',
`haskell-shell-prompt-block-regexp',
`haskell-shell-font-lock-enable',
`haskell-shell-completion-setup-code',
`haskell-shell-completion-string-code',
`haskell-eldoc-setup-code', `haskell-eldoc-string-code',
`haskell-ffap-setup-code' and `haskell-ffap-string-code' can
customize this mode for different Haskell interpreters.

This mode resets `comint-output-filter-functions' locally, so you
may want to re-add custom functions to it using the
`haskell-comint-mode-hook'.

You can also add additional setup code to be run at
initialization of the interpreter via `haskell-shell-setup-codes'
variable.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (set (make-local-variable 'indent-tabs-mode) nil)
  ;; Users can interactively override default values for
  ;; `haskell-shell-interpreter' and `haskell-shell-interpreter-args'
  ;; when calling `run-haskell-comint'.  This ensures values let-bound in
  ;; `haskell-shell-make-comint' are locally set if needed.
  (set (make-local-variable 'haskell-shell-interpreter)
       (or haskell-shell--interpreter haskell-shell-interpreter))
  (set (make-local-variable 'haskell-shell-interpreter-args)
       (or haskell-shell--interpreter-args haskell-shell-interpreter-args))
  (set (make-local-variable 'haskell-shell--prompt-calculated-input-regexp) nil)
  (set (make-local-variable 'haskell-shell--block-prompt) nil)
  (haskell-shell-prompt-set-calculated-regexps)
  (setq comint-prompt-regexp haskell-shell--prompt-calculated-input-regexp)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'haskell-shell--last-buffer) nil)
  (set (make-local-variable 'comint-preoutput-filter-functions)
       '(haskell-shell-comint-non-show-to-type-filter))
  (set (make-local-variable 'comint-output-filter-functions)
       '(ansi-color-process-output
         haskell-pdbtrack-comint-output-filter-function
         haskell-shell-comint-watch-for-first-prompt-output-filter
         haskell-comint-postoutput-scroll-to-bottom
         comint-watch-for-password-prompt))
  (set (make-local-variable 'compilation-error-regexp-alist)
       haskell-shell-compilation-regexp-alist)
  (add-hook 'completion-at-point-functions
            #'haskell-shell-completion-at-point nil 'local)
  ;; (make-local-variable 'haskell-pdbtrack-buffers-to-kill)
  ;; (make-local-variable 'haskell-pdbtrack-tracked-buffer)
  (make-local-variable 'haskell-shell-internal-last-output)
  (compilation-shell-minor-mode 1)
  (define-key haskell-comint-mode-map (kbd "C-c C-z")
    'haskell-shell-switch-from-shell)
  ;; Some GUD functionality
  (define-key haskell-comint-mode-map (kbd "C-x SPC")
    'haskell-comint-set-breakpoint)
  (define-key haskell-comint-mode-map (kbd "C-c C-k")
    'haskell-comint-clear-buffer)
  (set (make-local-variable 'gud-delete-prompt-marker) (make-marker))
  (set (make-local-variable 'comint-prompt-regexp)
       haskell-shell-prompt-debug-regexp)
  (set (make-local-variable 'gud-minor-mode) 'haskell-comint))

;;; Send output
;;;###autoload
(defvar haskell-shell--loaded-file nil)
(defun haskell-shell-load-file (file-name &optional process msg)
  "Load file FILE-NAME to the shell.  Use PROCESS and show friendly MSG."
  (interactive (list (or (buffer-file-name) (read-file-name "File to load: "))))

  (let* ((proc (or process (haskell-shell-get-process-or-error msg)))
         (buf (process-buffer proc)))
    (if (string= file-name (with-current-buffer buf haskell-shell--loaded-file))
      (haskell-shell-reload-last-file proc)
    (set (make-local-variable 'haskell-shell--loaded-file) file-name)
    (haskell-shell-send-string (format ":load %s" file-name) proc))))

;;;###autoload
(defun haskell-shell-reload-last-file (&optional process msg)
  "Reload previous to the shell.
Use PROCESS and show friendly MSG."
  (interactive)
  (haskell-shell-send-string
   ":reload"
   (or process (haskell-shell-get-process-or-error msg))))

;;;###autoload
(defun haskell-shell-switch-from-shell ()
  "Switch from shell to the last haskell buffer."
  (interactive)
  (pop-to-buffer
   (or haskell-shell--last-buffer
       (find-if (lambda (b) (with-current-buffer b (eq major-mode 'haskell-mode)))
                (buffer-list)))))

;;;###autoload
(defun haskell-shell-switch-to-shell (&optional process msg)
  "Switch to inferior Haskell process buffer using PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list nil t))
  (setq haskell-shell--last-buffer (current-buffer))
  (pop-to-buffer
   (process-buffer (or process (haskell-shell-get-process-or-error msg))) nil t))

(defvar haskell-shell-output-filter-in-progress nil)
(defvar haskell-shell-output-filter-buffer nil)

(defun haskell-shell-output-filter (string)
  "Filter used in `haskell-shell-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`haskell-shell-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   haskell-shell-output-filter-buffer
   (concat haskell-shell-output-filter-buffer string))
  (when (haskell-shell-comint-end-of-output-p
         haskell-shell-output-filter-buffer)
    ;; Output ends when `haskell-shell-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq haskell-shell-output-filter-in-progress nil
          haskell-shell-output-filter-buffer
          (substring haskell-shell-output-filter-buffer
                     0 (match-beginning 0))))
  "")

;;;###autoload
(defun haskell-shell-send-string (string &optional process msg)
  "Send STRING to inferior Haskell PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Haskell command: ") nil t))
  (set (make-local-variable 'haskell-shell--is-no-show) nil)
  (let ((process (or process (haskell-shell-get-process-or-error msg))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun haskell-shell-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let ((process (or process (haskell-shell-get-process-or-error)))
        (comint-preoutput-filter-functions '(haskell-shell-output-filter))
        (haskell-shell-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (haskell-shell-send-string string process)
       (while haskell-shell-output-filter-in-progress
         ;; `haskell-shell-output-filter' takes care of setting
         ;; `haskell-shell-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output process))
       (prog1
           haskell-shell-output-filter-buffer
         (setq haskell-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer process)
       (comint-interrupt-subjob)))))


;;; Shell completion
(defcustom haskell-shell-completion-setup-code
  ""
  "Code used to setup completion in inferior Haskell processes."
  :type 'string
  :group 'haskell-comint)

(defun haskell-util-strip-string (string)
  "Strip STRING whitespace and newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any whitespace ?\r ?\n)))
           (: (* (any whitespace ?\r ?\n)) string-end)))
   ""
   string))

(defconst haskell-string-literal-escapes-regexp
  (concat "[\\]\\(?:"
          (regexp-opt (append
                       (mapcar (lambda (c) (format "%c" c))
                               "abfnrtv\\\"'&") ;; "charesc" escape sequences
                       (mapcar (lambda (c) (format "^%c" c))
                               "ABCDEFGHIJKLMNOPQRSTUVWXYZ@[\\]^_") ;; "cntrl" escape sequences
                       (mapcar (lambda (s) (format "%s" s))
                               (split-string "NUL SOH STX ETX EOT ENQ ACK BEL BS HT LF VT FF CR
                                              SO SI DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN EM SUB ESC
                                              FS GS RS US SP DEL")))) ;; "ascii" (w\o "cntrl") escape sequences
          "\\|" "[\t\n\v\f\r ]+[\\]"  ;; whitespace gaps
          "\\|" "[0-9]+"              ;; decimal escape sequence
          "\\|" "o[0-7]+"             ;; octal escape sequence
          "\\|" "x[0-9a-f]+"          ;; hex escape sequence
          "\\)?") ;; everything else is an invalid escape sequence
  "Regexp for matching escape codes in string literals.")

(defconst haskell-string-literal-decode1-table
  (let ((h (make-hash-table :test 'equal)))
    (mapc (lambda (c) (puthash (concat "\\" (car c)) (cdr c) h))
          '(;; ascii-escapes
            ("NUL" . "\x00") ("SOH" . "\x01") ("STX" . "\x02") ("ETX" . "\x03") ("EOT" . "\x04") ("ENQ" . "\x05")
            ("ACK" . "\x06") ("BEL" . "\x07") ("BS"  . "\x08") ("HT"  . "\x09") ("LF"  . "\x0a") ("VT"  . "\x0b")
            ("FF"  . "\x0c") ("CR"  . "\x0d") ("SO"  . "\x0e") ("SI"  . "\x0f") ("DLE" . "\x10") ("DC1" . "\x11")
            ("DC2" . "\x12") ("DC3" . "\x13") ("DC4" . "\x14") ("NAK" . "\x15") ("SYN" . "\x16") ("ETB" . "\x17")
            ("CAN" . "\x18") ("EM"  . "\x19") ("SUB" . "\x1a") ("ESC" . "\x1b") ("FS"  . "\x1c") ("GS"  . "\x1d")
            ("RS"  . "\x1e") ("US"  . "\x1f") ("SP"  . "\x20")                                   ("DEL" . "\x7f" )
            ;; C-compatible single-char escape sequences
            ("a" . "\x07") ("b" . "\x08") ("f" . "\x0c") ("n" . "\x0a") ("r" . "\x0d") ("t" . "\x09") ("v" . "\x0b")
            ;; trivial escapes
            ("\\" . "\\") ("\"" . "\"") ("'" . "'")
            ;; "empty" escape
            ("&" . "")))
    h)
  "Hash table containing irregular escape sequences and their decoded strings.
Used by `haskell-string-literal-decode1'.")

(defun haskell-string-literal-decode1 (l)
  "Decode a single string literal escape sequence.
L must contain exactly one escape sequence.
This is an internal function used by `haskell-string-literal-decode'."
  (let ((case-fold-search nil))
    (cond
     ((gethash l haskell-string-literal-decode1-table))
     ((string-match "\\`[\\][0-9]+\\'" l)         (char-to-string (string-to-number (substring l 1) 10)))
     ((string-match "\\`[\\]x[[:xdigit:]]+\\'" l) (char-to-string (string-to-number (substring l 2) 16)))
     ((string-match "\\`[\\]o[0-7]+\\'" l)        (char-to-string (string-to-number (substring l 2) 8)))
     ((string-match "\\`[\\]\\^[@-_]\\'" l)       (char-to-string (- (aref l 2) ?@))) ;; "cntrl" escapes
     ((string-match "\\`[\\][\t\n\v\f\r ]+[\\]\\'" l) "") ;; whitespace gap
     (t (error "Invalid escape sequence")))))

(defun haskell-string-literal-decode (estr &optional no-quotes)
  "Decode a Haskell string-literal.
If NO-QUOTES is nil, ESTR must be surrounded by quotes.

This is the dual operation to `haskell-string-literal-encode'."
  (if (and (not no-quotes)
           (string-match-p "\\`\"[^\\\"[:cntrl:]]*\"\\'" estr))
      (substring estr 1 -1) ;; optimized fast-path for trivial strings
    (let ((s (if no-quotes ;; else: do general decoding
                 estr
               (when (string-match-p "\\`\".*\"\\'" estr)
                   (substring estr 1 -1))))
          (case-fold-search nil))
      (and s
           (replace-regexp-in-string
            haskell-string-literal-escapes-regexp
            #'haskell-string-literal-decode1
            s t t)))))

;;;###autoload
(defun haskell-shell-complete-at-point ()
  "Start a completion."
  (interactive)
  (completion-at-point))

(defun haskell-shell-completion-get-completions (process input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  (with-current-buffer (process-buffer process)
    (let ((rawstr
           (haskell-util-strip-string
            (haskell-shell-send-string-no-output
             (format ":complete repl 5 \"%s\"" input) process))))
      (when rawstr
        ;; parse REPL response if any
        (let* ((s1 (split-string rawstr "\r?\n" t))
               (cs (cl-delete-if 'null (mapcar #'haskell-string-literal-decode (cdr s1))))
               (h0 (car s1))) ;; "<limit count> <all count> <unused string>"
          (unless (string-match
                   (rx
                    (group (+ digit))
                    whitespace
                    (group (+ digit))
                    whitespace
                    (group ?" (* nonl) ?"))
                   h0)
            (error "Invalid `:complete' response '%s'" h0))
          (let ((cnt1 (match-string 1 h0))
                (h1 (haskell-string-literal-decode (match-string 3 h0))))
            (unless (= (string-to-number cnt1) (length cs))
              (error "Lengths inconsistent in `:complete' response"))
            (cons h1 cs)))))))


(defun haskell-shell-completion-at-point (&optional process)
  "Function for `completion-at-point-functions' in `haskell-comint-mode'.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  (interactive)
  (setq process (or process (get-buffer-process (current-buffer))))
  (let* ((line-start (if (derived-mode-p 'haskell-comint-mode)
                         ;; Working on a shell buffer: use prompt end.
                         (cdr (haskell-util-comint-last-prompt))
                       (line-beginning-position)))
         (import-statement
          (when (string-match-p
                 (rx (* space) word-start "import" word-end space)
                 (buffer-substring-no-properties line-start (point)))
            (buffer-substring-no-properties line-start (point))))
         (start
          (save-excursion
            (if (re-search-backward "[^[:alnum:]_:]" line-start t 1)
                (progn (forward-char (length (match-string-no-properties 0)))
                       (point))
              line-start)))
         (end (point))
         (prompt-boundaries
          (with-current-buffer (process-buffer process)
            (haskell-util-comint-last-prompt)))
         (prompt
          (with-current-buffer (process-buffer process)
            (when prompt-boundaries
              (buffer-substring-no-properties
               (car prompt-boundaries) (cdr prompt-boundaries)))))
         (completion-fn
          (with-current-buffer (process-buffer process)
            (cond
             ((or (null prompt) (< (point) (cdr prompt-boundaries)))
              #'ignore)
             ;; Even if native completion is enabled, for
             ;; pdb interaction always use the fallback
             ;; mechanism since the completer is changed.
             ;; Also, since pdb interaction is single-line
             ;; based, this is enough.
             ((and (string-match-p haskell-shell-prompt-debug-regexp prompt)
                   (or (equal haskell-shell--block-prompt prompt)
                       (string-match-p haskell-shell-prompt-block-regexp prompt)))
              #'ignore)
             (t #'haskell-shell-completion-get-completions)))))
    (list start end
          (completion-table-dynamic
           (apply-partially completion-fn process)))))

(defvar comint-last-prompt-overlay)     ; Shut up, byte compiler.
(defun haskell-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

;;; Interna
(defun haskell-shell-internal-get-process-name ()
  "Calculate the appropriate process name for Internal Haskell process.
The name is calculated from `haskell-shell-global-buffer-name' and
the `buffer-name'."
  (format "%s[%s]" haskell-shell-internal-buffer-name (buffer-name)))


;;; PDB Track integration

(defcustom haskell-pdbtrack-activate t
  "Non-nil makes Haskell shell enable pdbtracking."
  :type 'boolean
  :group 'haskell-comint
  :safe 'booleanp)

(defvar haskell-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`haskell-pdbtrack-set-tracked-buffer' instead.")

(defvar haskell-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defun haskell-pdbtrack-set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the `haskell-pdbtrack-tracked-buffer' variable.
Returns the tracked buffer."
  (let* ((file-name-prospect (concat (file-remote-p default-directory)
                              file-name))
         (file-buffer (get-file-buffer file-name-prospect)))
    (if file-buffer
        (setq haskell-pdbtrack-tracked-buffer file-buffer)
      (cond
       ((file-exists-p file-name-prospect)
        (setq file-buffer (find-file-noselect file-name-prospect)))
       ((and (not (equal file-name file-name-prospect))
             (file-exists-p file-name))
        ;; Fallback to a locally available copy of the file.
        (setq file-buffer (find-file-noselect file-name-prospect))))
      (when (not (member file-buffer haskell-pdbtrack-buffers-to-kill))
        (add-to-list 'haskell-pdbtrack-buffers-to-kill file-buffer)))
    file-buffer))

(defun haskell-pdbtrack-search-single-line (fmt)
    "Search backward for the formatted regex that will yield a
single line. Make sure that any other groups in fmt are shy groups
ie (?:...) groups. This function returns

  ((point . (MATCH-BEGINNING . MATCH-END))
   (file . FNAME)
   (begin . ((line . LINE) (col . COL)))
   (end . ((line . LINE) (col . COL))))

So both instances of LINE will be the same as the  format is

<fname>:<line>:<begin col>-<end col>"
  (save-excursion
    (save-match-data
      (when (re-search-backward
             (format fmt "\\([^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)-\\([[:digit:]]+\\)")
             haskell--search-bound t)
        (let ((file-name (match-string-no-properties 1))
              (line (string-to-number (match-string-no-properties 2)))
              (begin-col (string-to-number (match-string-no-properties 3)))
              (end-col (string-to-number (match-string-no-properties 4)))
              (match-end (match-end 0))
              (match-beg (match-beginning 0)))
          `((match-range (,match-beg . ,match-end))
            (file-name . ,file-name)
            (begin . ((line . ,line) (col . ,begin-col)))
            (end . ((line . ,line) (col . ,end-col)))))))))

(defconst haskell--search-bound nil)

(defun haskell-pdbtrack-search-multi-line (fmt)
  "Search backward for the formatted regex that will yield a
multiline. Make sure that any other groups in fmt are shy groups
ie (?:...) groups. This function returns

  ((match-range . (MATCH-BEGINNING . MATCH-END))
   (file . FNAME)
   (begin . ((line . LINE) (col . COL)))
   (end . ((line . LINE) (col . COL))))

The format will be

<file name>:(<begin line>,<begin col>)-(<end line>,<end col>).

If nothing is found return nil. POINT is the beginning of the match"
  (let ((regex
         (format
          fmt
          "\\([^:[:space:]]+\\):(\\([[:digit:]]+\\),\\([[:digit:]]+\\))-(\\([[:digit:]]+\\),\\([[:digit:]]+\\))")))
    (save-excursion
      (save-match-data
        (when (re-search-backward
               regex
               haskell--search-bound t)
          (let ((file-name (match-string-no-properties 1))
                (begin-line (string-to-number (match-string-no-properties 2)))
                (begin-col (string-to-number (match-string-no-properties 3)))
                (end-line (string-to-number (match-string-no-properties 4)))
                (end-col (string-to-number (match-string-no-properties 5)))
                (match-end (match-end 0))
                (match-beg (match-beginning 0)))
            `((match-range (,match-beg . ,match-end))
              (file-name . ,file-name)
              (begin . ((line . ,begin-line) (col . ,begin-col)))
              (end . ((line . ,end-line) (col . ,end-col))))))))))

(require 'dash)
(defun haskell-pdbtrack-search-markers (fmts)
  (cl-flet ((bgn (i) (caadr (assq 'match-range i))))
    (let ((markers (-concat
                    (mapcar #'haskell-pdbtrack-search-single-line fmts)
                    (mapcar #'haskell-pdbtrack-search-multi-line fmts)))
          (ret))
      (dolist (m markers)
        (when (and m (or (null ret) (< (bgn ret) (bgn m))))
          (setq ret m)))
      ret)))

(defcustom haskell-pdbtrack-filepos-fmts '("Stopped in .*, %s" "Logged breakpoint at %s" "Stopped at %s")
  "Format the filetype that is emitted by the debugger")

(defun haskell-pdbtrack-comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and haskell-pdbtrack-activate (not (string= output "")))
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (break (with-temp-buffer
                     (insert full-output)
                     (goto-char (point-max))
                     (haskell-pdbtrack-search-markers haskell-pdbtrack-filepos-fmts))))
      (if break
          (haskell--set-pdb-marker break)
        (when haskell-pdbtrack-tracked-buffer
          (with-current-buffer haskell-pdbtrack-tracked-buffer
            (when overlay-arrow-position
              (set-marker overlay-arrow-position nil)))
          (mapc #'(lambda (buffer)
                    (ignore-errors (kill-buffer buffer)))
                haskell-pdbtrack-buffers-to-kill)
          (setq haskell-pdbtrack-tracked-buffer nil
                haskell-pdbtrack-buffers-to-kill nil)))))
  output)



(defun haskell-proftrack-next ()
  (interactive)
  (haskell-proftrack-goto-next)
  (haskell-proftrack-reposition))
(defun haskell-proftrack-previous ()
  (interactive)
  (haskell-proftrack-goto-previous)
  (haskell-proftrack-reposition))
(defun haskell-proftrack-forward ()
  (interactive)
  (forward-line 1)
  (back-to-indentation)
  (haskell-proftrack-reposition))
(defun haskell-proftrack-back ()
  (interactive)
  (forward-line -1)
  (back-to-indentation)
  (haskell-proftrack-reposition))
(defun haskell-proftrack-reposition ()
  (interactive)
  (save-match-data
    (save-excursion
      (end-of-line)
      (let ((haskell--search-bound (save-excursion (beginning-of-line) (point)))
            (haskell-pdbtrack-filepos-fmts
             '("[[:space:]]*[^[:space:]]+[[:space:]]+[^[:space:]]+[[:space:]]+%s")))
        (haskell-pdbtrack-reposition)
        (set-transient-map haskell-proftrack-transient-map)))))

(defun haskell--set-pdb-marker (break)
  (let* ((begin-line-number (cdr (assq 'line (cdr (assq 'begin break)))))
         (end-line-number (cdr (assq 'line (cdr (assq 'end break)))))
         (begin-col (cdr (assq 'col (cdr (assq 'begin break)))))
         (end-col (cdr (assq 'col (cdr (assq 'end break)))))
         (tracked-buffer
          (haskell-pdbtrack-set-tracked-buffer (cdr (assq 'file-name break))))
         (shell-buffer (current-buffer))
         (tracked-buffer-window (get-buffer-window tracked-buffer))
         (tracked-buffer-line-pos))
    (with-current-buffer tracked-buffer
      (let ((begin-pos (save-excursion
                         (goto-char (point-min))
                         (move-beginning-of-line begin-line-number)
                         (forward-char (1- begin-col))
                         (point-marker)))
            (end-pos (save-excursion
                         (goto-char (point-min))
                         (move-beginning-of-line end-line-number)
                         (forward-char end-col)
                         (point-marker))))
          (set (make-local-variable 'overlay-arrow-string) "=>")
      (set (make-local-variable 'overlay-arrow-position) (make-marker))
      (set (make-local-variable 'pulse-delay) 0.1)
      (setq tracked-buffer-line-pos begin-pos)
      (when tracked-buffer-window
        (set-window-point
         tracked-buffer-window tracked-buffer-line-pos))
      (set-marker overlay-arrow-position tracked-buffer-line-pos)
      (pulse-momentary-highlight-region begin-pos end-pos)))
    (pop-to-buffer tracked-buffer)))


(defun haskell-pdbtrack-reposition ()
  "Look upwards for the last stop and reposition the cursor there."
  (interactive)
  (let ((break (haskell-pdbtrack-search-markers haskell-pdbtrack-filepos-fmts)))
  (if break
      (haskell--set-pdb-marker break)
    (error "Couldn't find 'stopped in ...'"))))

(defun haskell-comint-set-breakpoint ()
  (interactive)
  (haskell-shell-send-string
   (format ":break %s %d %d"
           (haskell-declared-module-name)
           (line-number-at-pos)
           (current-column))))
;; (defun haskell-comint-jump-to-prev-filepos ()
;;   (interactive)
;;   (format "\\[%s\\]" haskell-filepos-regex) nil t)
;;       (haskell--set-pdb-marker (match-string-no-properties 1)
;;                                (string-to-number
;;                                 (or (match-string-no-properties 2)
;;                                     (match-string-no-properties 3)))))))

(setq haskell-pdbtrack--last-step-style ":step")
(setq haskell-pdbtrack--code-window nil)
(setq haskell-pdbtrack-functions
      '(("n" . haskell-pdbtrack-next)
        ("f" . haskell-pdbtrack-forward)
        ("b" . haskell-pdbtrack-back)
        ("m" . haskell-pdbtrack-stepmodule)
        ("r" . haskell-pdbtrack-step-dwim)
        ("c" . haskell-pdbtrack-continue)
        ("s" . haskell-pdbtrack-step)))
(setq haskell-proftrack-functions
      '(("f" . haskell-proftrack-forward)
        ("b" . haskell-proftrack-back)
        ("n" . haskell-proftrack-next)
        ("p" . haskell-proftrack-previous)
        ("C-p" . haskell-proftrack-goto-previous)
        ("C-n" . haskell-proftrack-goto-next)
        ("RET" . haskell-proftrack-reposition)))

(setq haskell-proftrack-transient-map
      (let ((map (make-sparse-keymap)))
        (dolist (i haskell-proftrack-functions)
          (define-key map (kbd (car i)) (cdr i)))
        map))

(defun haskell-proftrack-goto-previous (&optional col)
  (interactive)
  (haskell-proftrack-goto-next -1 col))

(defun haskell-proftrack-goto-next (&optional dir col)
  "Go to the next that has at least col. DIR is 1 or nil for
forward and -1 for backward."
  (interactive)
  (push-mark)
  (let ((col (or
              col
              (save-excursion (back-to-indentation) (current-column)))))
    (forward-line (or dir 1))
    (beginning-of-line)
    (forward-char col)
    (while (and (looking-back "^ *") (looking-at " "))
      (forward-line (or dir 1))
      (beginning-of-line)
      (forward-char col)))
  (set-transient-map haskell-pdbtrack-transient-map))

(define-minor-mode haskell-comint-pdbtrack-mode
  "Mode to be active in the haskell files."
  :lighter "haskell-dbg "
  :keymap (let ((map (make-sparse-keymap)))
    (dolist (i haskell-pdbtrack-functions)
      (define-key map (kbd (format "C-x C-a %s" (car i))) (cdr i)))
    map)
  nil)

(setq haskell-proftrack-map
      (let ((map (make-sparse-keymap)))
        (dolist (i haskell-proftrack-functions)
          (define-key map (kbd (format "C-x C-a %s" (car i))) (cdr i)))
        map))

(define-minor-mode haskell-proftrack-mode
  "Track profile lines into the code."
  :lighter "haskell-proftrack "
  :keymap haskell-proftrack-map
  nil)

(setq haskell-pdbtrack-transient-map
      (let ((map (make-sparse-keymap)))
        (dolist (i haskell-pdbtrack-functions)
          (define-key map (kbd (car i)) (cdr i)))
        map))
(defun haskell-pdbtrack-step-dwim (&optional step-style)
  (interactive)
  (setq haskell-pdbtrack--last-step-style
        (or step-style haskell-pdbtrack--last-step-style))
  (haskell-shell-send-string haskell-pdbtrack--last-step-style)
  (set-transient-map haskell-pdbtrack-transient-map))
(defun haskell-pdbtrack-next ()
  (interactive)
  (haskell-pdbtrack-step-dwim ":steplocal"))
(defun haskell-pdbtrack-continue ()
  (interactive)
  (haskell-pdbtrack-step-dwim ":continue"))
(defun haskell-pdbtrack-step ()
  (interactive)
  (haskell-pdbtrack-step-dwim ":step"))
(defun haskell-pdbtrack-stepmodule ()
  (interactive)
  (haskell-pdbtrack-step-dwim ":stepmodule"))
(defun haskell-pdbtrack-forward ()
  (interactive)
  (haskell-pdbtrack-step-dwim ":forward"))
(defun haskell-pdbtrack-back ()
  (interactive)
  (haskell-shell-send-string ":back"))
(defun haskell-pdbtrack-forward ()
  (interactive)
  (haskell-shell-send-string ":back"))

(defun haskell-proftrack-ellipsis ()
  "Return propertized ellipsis content."
  (concat " "
          (propertize yafolding-ellipsis-content 'face 'haskell-proftrack-ellipsis-face)
          " "))

(defun haskell-proftrack-hide-region (beg end)
  (when (> end beg)
    (let ((before-string
           (concat
            (propertize " " 'display '(left-fringe right-triangle))
            (yafolding-ellipsis)))
          (new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'intangible t)
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'modification-hooks
                   (list (lambda (overlay &optional a b c d)
                           (delete-overlay overlay))))
      (overlay-put new-overlay 'before-string before-string)
      (overlay-put new-overlay 'category "haskell-proftrack"))))

(defun haskell-proftrack-unhide-region (beg end)
  "Delete all yafolding overlays between BEG and END."
  (mapcar 'delete-overlay (haskell-proftrack-get-overlays beg end)))
(defun yafolding-get-overlays (beg end)
  "Get all overlays between BEG and END."
  (delq nil
        (mapcar (lambda (overlay)
                  (and (member "haskell-proftrack" (overlay-properties overlay))
                       overlay))
                (overlays-in beg end))))

(provide 'fd-haskell-comint)
;;; fd-haskell-comint.el ends here.
