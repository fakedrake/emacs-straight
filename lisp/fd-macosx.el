;; key bindings
;; Fn screws thing up in macs
(global-set-key (kbd "<S-backspace>") 'backward-kill-word)
(global-set-key (kbd "<end>") 'forward-word)
(global-set-key (kbd "<home>") 'backward-word)

; Frame and window management:
(setq special-display-regexps nil)   ; do not open certain buffers in special windows/frames
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")

 ;; Editing

; (global-smart-spacing-mode -1)  ; not on by default
(remove-hook 'text-mode-hook 'smart-spacing-mode)   ; do not use smart spacing in text modes
(global-visual-line-mode -1)  ; turn off Emacs 23 visual line
(cua-mode nil)
; (transient-mark-mode nil)  ; (must switch off CUA mode as well for this to work)

(setq mac-option-modifier 'meta
      mac-command-modifier 'meta
      mac-function-modifier 'control)
(setq ns-option-modifier 'meta
      ns-command-modifier 'meta
      ns-function-modifier 'control)

(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
(set-face-font 'default "-*-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

(require 'mailcap)
(setcdr (assoc 'viewer
               (assoc "pdf" (assoc "application" mailcap-mime-data))) "open %s")

(set-keyboard-coding-system 'mac-roman)
(set-selection-coding-system 'mac-roman)
(defun s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(setq el-get-emacs
      (concat
       (s-trim-right
        (shell-command-to-string
         "find /usr/local/Cellar/emacs -name 'bin'"))
       "/emacs"))
(setenv "EMACS" el-get-emacs)

(defun copy-from-osx ()
  (let ((default-directory "~"))
    (shell-command-to-string "pbpaste")))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil)
        (lang (getenv "LANG"))
        (default-directory "~"))
    (setenv "LANG" "en_US.UTF-8")
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

(provide 'fd-macosx)
