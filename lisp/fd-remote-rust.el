(defun lsp-tramp-connect-over-ssh (command filter sentinel name environment-fn workspace)
  (let* ((host "localhost")
         (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
         (command (with-parsed-tramp-file-name buffer-file-name nil
                    (message "[tcp/ssh hack] running LSP %s on %s / %s"
                             command
                             host
                             localname)
                    (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock"
                                                lsp-port))
                           (command (list
                                     "ssh"
                                     "-C"
                                     "-vvvv"
                                     "-L" (format "%s:%s" lsp-port unix-socket)
                                     host
                                     "socat"
                                     (format "unix-listen:%s" unix-socket)
                                     (format "system:'\"cd %s && %s\"'"
                                             (file-name-directory localname)
                                             command)
                                     )))
                      (message "using local command %s" command)
                      command)))
         (final-command (if (consp command) command (list command)))
         (_ (unless (executable-find (cl-first final-command))
              (user-error (format "Couldn't find executable %s"
                                  (cl-first final-command)))))
         (process-environment
          (lsp--compute-process-environment environment-fn))
         (proc (make-process :name name
                             :connection-type 'pty
                             :coding 'no-conversion
                             :command final-command
                             :sentinel sentinel
                             :stderr (format "*%s::stderr*" name)
                             :noquery t))
         (tcp-proc (progn
                     (sleep-for 1) ; prevent a connection before SSH
                                   ; has run socat. Ugh.
                     (lsp--open-network-stream host lsp-port (concat name "::tcp")))))

    ;; TODO: Same :noquery issue (see above)
    (set-process-query-on-exit-flag proc nil)
    (set-process-query-on-exit-flag tcp-proc nil)
    (set-process-filter tcp-proc filter)
    (cons tcp-proc proc)))

(defun lsp-tramp-connection-over-ssh-port-forwarding (command)
  "Like lsp-tcp-connection, but uses SSH portforwarding."
  (list
   :connect (apply-partially 'lsp-tramp-connect-over-ssh command)
   :test? (lambda () t)))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection-over-ssh-port-forwarding
                                   "/home/christosp/.cargo/bin/rustup run nightly rust-analyzer")
                  :major-modes '(rust-mode)
                  :initialization-options '((omitInitBuild . t)
                                            (cmdRun . t))
                  :notification-handlers (ht ("window/progress" 'lsp-clients--rust-window-progress))
                  :action-handlers (ht ("rls.run" 'lsp-rust--rls-run))
                  :remote? t
                  :server-id 'rust-analyzer-remote))

(provide 'fd-remote-rust)
