 ;;; -*- lexical-binding: t -*-

(setq flymake-list-only-diagnostics ())

(add-to-list 'exec-path (expand-file-name "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/"))
                                        ; (use-package eglot)

(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(defun fd/rust-hook ()
  (delete #'flymake-eldoc-function eldoc-documentation-functions)
  (add-to-list 'eldoc-documentation-functions #'flymake-eldoc-function))


(rx-define path-line-col
  (seq
   (group-n 1 (+ (not ?:)))
   ?: (group-n 2 (+ digit))
   (? ?: (group-n 3 (+ digit)))))

(rx-define address
  (seq "0x" (+ (any digit (?a . ?f)))))

(defun compilation-remove-rx-symbol (sym)
  (delq sym compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist-alist
        (assoc-delete-all sym compilation-error-regexp-alist-alist)))

(defun compilation-set-rx-symbol (sym level rx)
  (compilation-remove-rx-symbol sym)
  (add-to-list 'compilation-error-regexp-alist sym)
  (add-to-list
   'compilation-error-regexp-alist-alist
   `(,sym ,rx 1 2 3 ,(pcase level
                       ('error 2)
                       ('warning 1)
                       ('info 0)))))

'(seq space ?# (+ digit) space address space "in" space (+ (not space)) space)
(defun setup-rust-error-regex ()
  (compilation-set-rx-symbol
   'address-sanitizer 'warning
   (rx
    (seq space ?# (+ digit)
         space address
         space "in"
         space (+ (not space))
         space path-line-col)))

  (compilation-set-rx-symbol
   'rust-note 'warning
   (rx
    (seq line-start
         (??
          (or (seq (+ white) "= note" (* not-newline))
              (seq "note: " (* not-newline))))
         (+ white) "at" (+ white)
         path-line-col
         line-end)))
  (compilation-set-rx-symbol
   'rust-arrow-error 'error
   (rx
    (seq line-start "error" (+? anychar)
         "-->" (+ blank)
         path-line-col
         line-end)))
  (compilation-set-rx-symbol
   'note-rust-arrow 'info
   (rx
    (seq line-start "note" (+? anychar)
         (+ white) "-->" (+ white)
         path-line-col
         line-end))))

(use-package rust-mode
  :hook ((rust-mode . fd/rust-hook))
  :config

  (setenv "CARGO_HOME" (or (getenv "CARGO_HOME")
                           (concat (getenv "HOME") "/.cargo")))
  (setup-rust-error-regex)
  (add-to-list 'exec-path (concat (file-name-as-directory (getenv "CARGO_HOME")) "bin"))
  (setq rust-cargo-default-arguments "-- --nocapture")
  (require 'lsp-rust)
  (add-to-list 'eglot-server-programs '(rust-mode "/home/christosp/.cargo/bin/rustup" "run" "nightly" "rust-analyzer"))
  (setq eglot-rust-server 'rust-analyzer))

(use-package rmsbolt
  :config
  (setq rmsbolt--shell "msys2"))


(setq dap-cpptools-extension-version "1.5.1")

(with-eval-after-load "lsp-rust"
  (require 'fd-remote-rust)
  (require 'tramp)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(with-eval-after-load 'lsp-rust
  (require 'dap-cpptools))

(require 'dap-gdb-lldb)
(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :program (executable-find "cargo")
                                     :cwd "${workspaceFolder}"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}"
                                     :target nil)))

;; (use-package exec-path-from-shell
;;  :ensure
;;  :init (exec-path-from-shell-initialize))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1)
  (dap-ui-controls-mode -1))

(require 'cl-lib)
(require 'dash)
(require 's)

(defun async-shell-command-consume-output (output-buffer callback-fn process signal)
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer output-buffer
      (let ((output-string
             (buffer-substring-no-properties
              (point-min)
              (point-max))))
        (funcall callback-fn output-string)))
    (kill-buffer output-buffer)))

(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (let ((output-buffer (generate-new-buffer " *temp*")))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (apply-partially #'async-shell-command-consume-output output-buffer callback))
    output-buffer))

(defun build-cargo-cmd (cargo-args)
  (s-join " "
          (append (list (executable-find "cargo"))
                  cargo-args
                  (list "--no-run" "--message-format=json"))))

(defun json-get-test-executable (str)
  "From the top level obnjct assert that reason =
compiler-artifact and get the executable key."
  (->> str
       (s-lines)
       (-keep (lambda (s)
                (condition-case nil
                    (-let* ((json-object-type 'plist)
                            ((msg &as &plist :reason :executable) (json-read-from-string s)))
                      (when (and executable (string= "compiler-artifact" reason))
                        executable))
                  (error))))
       (funcall
        (lambda (artifact-spec)
          (pcase artifact-spec
            (`() (user-error "No compilation artifacts or obtaining the runnable artifacts failed"))
            (`(,spec) spec)
            (_ (user-error "Multiple compilation artifacts are not supported")))))))

(defun do-dap-debug (label executable-args workspace-root? exec)
  (dap-debug
   `(:type "gdb"
           :request "launch"
           :name ,label
           :args ,executable-args
           :cwd ,workspace-root?
           :sourceLanguages ["rust"]
           :target ,exec
           ,@lsp-rust-analyzer-debug-lens-extra-dap-args)))

(defun do-gdb-debug (label executable-args workspace-root? exec)
  (gud-gdb (format "rust-gdb %s" exec)))

(defun lsp-rust-analyzer-debug (runnable)
  "Select and debug a RUNNABLE action."
  (interactive (list (lsp-rust-analyzer--select-runnable)))
  (-let (((&rust-analyzer:Runnable
           :args (&rust-analyzer:RunnableArgs :cargo-args :workspace-root? :executable-args)
           :label)
          runnable))
    (pcase (aref cargo-args 0)
      ("run" (aset cargo-args 0 "build"))
      ("test" (when (-contains? (append cargo-args ()) "--no-run")
                (cl-callf append cargo-args (list "--no-run")))))
    (let ((cmd (build-cargo-cmd cargo-args)))
      (message "Looking for executable: %s" cmd)
      (async-shell-command-to-string
       cmd
       (lambda (out)
                                        ; Change this to do-dap-debug for dap.
         (do-gdb-debug label executable-args workspace-root? (json-get-test-executable out)))))))


(defun rust--build-and-get-exec (build-arg)
  "Run cargo build --tests and get the resulting executable. Make
sure you have already built everyting with the compilation bells
and whistles because this will block till cargo-build finishes."
  (car
   (last
    (cl-delete-if-not
     'identity
     (mapcar
      (lambda (x) (condition-case
                 nil
                 (gethash "executable" (json-parse-string x :null-object nil))
               (error nil)))
      (let ((cmd (format "cargo build %s --message-format json" build-arg)))
        (message "Getting exe: %s" cmd)
        (butlast
         (split-string (shell-command-to-string cmd) "\n"))))))))

(defun rust--exe-command (build-arg)
  (message "Running cargo build to get the test path...")
  (let ((path (rust--build-and-get-exec build-arg))
        (test (if (region-active-p)
                  (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))
                "")))
    (format "%s --nocapture %s" path test)))

(defun gud-rust-gdb-bench ()
  (interactive)
  (gud-rust-gdb--internal "--benches"))

(defun gud-rust-gdb-test ()
  (interactive)
  (gud-rust-gdb--internal "--tests"))

(defun gud-rust-gdb--internal (build-arg)
  (let ((ec (rust--exe-command build-arg)))
    (message "GDB will run '%s'" ec)
    (gud-gdb (format "rust-gdb --fullname --args %s --test-threads=1" ec))))

(defun valgrind-rust-test ()
  (interactive)
  (compilation-start
   (format "valgrind --track-origins=yes %s"
           (rust--exe-command "--test"))))

(defun valgrind-rust-bench ()
  (interactive)
  (compilation-start
   (format "valgrind --track-origins=yes %s"
           (rust--exe-command "--bench"))))
