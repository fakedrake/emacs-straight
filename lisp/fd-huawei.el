(require 'cl-lib)
(cl-delete-if (lambda (x) (equal "plinkh" (car x))) tramp-methods)
(add-to-list 'tramp-methods
             `("plinkh"
               (tramp-login-program        "plink")
               (tramp-login-args           (("-load") ("%h") ("-t")
                                            ("env")
				            (,(format "'TERM=%s'" tramp-terminal-type))
                                            ("'PROMPT_COMMAND='")
                                            (,(format "'PS1=%s'" tramp-initial-end-of-output))
                                            ("/bin/bash")
                                            ("-lc")
                                            ("%l")))
               (tramp-remote-shell         ,tramp-default-remote-shell)
               (tramp-remote-shell-login   ("-l"))
               (tramp-remote-shell-args    ("-c"))))

(remove-hook 'tramp-cleanup-connection-hook #'tramp-recentf-cleanup)
(remove-hook 'tramp-cleanup-all-connection-hook #'tramp-recentf-cleanup-all)


(defun cntlm-host-ip ()
  (when (file-exists-p "/etc/resolv.conf")
    (with-temp-buffer
      (insert-file-contents "/etc/resolv.conf")
      (search-forward-regexp
       "nameserver +\\([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\)")
      (match-string-no-properties 1))))

(defun next-toml-tag (from)
  (save-excursion
    (goto-char from)
    (if (search-forward-regexp "^ *\\[.*\\] *$" nil t)
        (match-beginning 0)
      (point-max))))

(defun fix-routes ()
  "Runs a powershell command that removes the routes that divert
packets from WSL2."
  (start-file-process
   "fix-routes-powershell"
   "*fix-routes-powershell*"
   "powershell.exe"
   "Get-NetIPAddress -InterfaceAlias \"vEthernet (WSL)\" | ForEach-Object { Get-NetRoute -DestinationPrefix \"$($_.IPAddress -replace '\.\d+$', \".0\")/20\" | Where-Object -Property ifIndex -Value $_.ifIndex  -NE } | Remove-NetRoute"))

(defun insert-toml-tag ()
  (goto-char (point-max))
  (insert tag)
  (insert "\n")
  (point))

(defun toml-tag (tag)
  (save-excursion
    (if (search-forward tag nil t)
        (progn
          (goto-char (match-end 0))
          (forward-char))
      (insert-toml-tag tag))
    (point)))

(defun set-cargo-proxy (proxy-url)
  "Set proxy in [http] and [https] to url in the file
~/.cargo/config.toml"
  (with-current-buffer (find-file-noselect "~/.cargo/config.toml")
    (save-excursion
      (beginning-of-buffer)
      (dolist (tag '("[http]" "[https]"))
        (let* ((beg (toml-tag tag))
               (end (next-toml-tag beg)))
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (goto-char (point-min))
              (if (search-forward-regexp "^proxy *=" nil t)
                  (kill-whole-line)
                (goto-char (point-min)))
              (when proxy-url
                (insert (format "proxy = \"%s\"\n" proxy-url)))))))
      (save-buffer))))

(defun set-git-proxy (prx)
  (shell-command (format "git config --global http.proxy %s" prx))
  (shell-command (format "git config --global https.proxy %s" prx)))

(defun set-envionment-proxy (http-url)
  "Set environment variables related to the proxy."
  (setenv "HTTPS_PROXY" http-url)
  (setenv "HTTP_PROXY" http-url)
  (setenv "https_proxy" http-url)
  (setenv "http_proxy" http-url))


(setq huawei-certificate
      "-----BEGIN CERTIFICATE-----
MIID2TCCAsGgAwIBAgIJALQPO9XxFFZmMA0GCSqGSIb3DQEBCwUAMIGCMQswCQYD
VQQGEwJjbjESMBAGA1UECAwJR3VhbmdEb25nMREwDwYDVQQHDAhTaGVuemhlbjEP
MA0GA1UECgwGSHVhd2VpMQswCQYDVQQLDAJJVDEuMCwGA1UEAwwlSHVhd2VpIFdl
YiBTZWN1cmUgSW50ZXJuZXQgR2F0ZXdheSBDQTAeFw0xNjA1MTAwOTAyMjdaFw0y
NjA1MDgwOTAyMjdaMIGCMQswCQYDVQQGEwJjbjESMBAGA1UECAwJR3VhbmdEb25n
MREwDwYDVQQHDAhTaGVuemhlbjEPMA0GA1UECgwGSHVhd2VpMQswCQYDVQQLDAJJ
VDEuMCwGA1UEAwwlSHVhd2VpIFdlYiBTZWN1cmUgSW50ZXJuZXQgR2F0ZXdheSBD
QTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBANk9kMn2ivB+6Lp23PIX
OaQ3Z7YfXvBH5HfecFOo18b9jC1DhZ3v5URScjzkg8bb616WS00E9oVyvcaGXuL4
Q0ztCwOszF0YwcQlnoLBpAqq6v5kJgXLvGfjx+FKVjFcHVVlVJeJviPgGm4/2FLh
odoreBqPRAfLRuSJ5U+VvgYipKMswTXh7fAK/2LkTf1dpWNvRsoXVm692uFkGuNx
dCdUHYCI5rl6TqMXht/ZINiclroQLkd0gJKhDVmnygEjwAJMMiJ5Z+Tltc6WZoMD
lrjETdpkY6e/qPhzutxDJv5XH9nXN33Eu9VgE1fVEFUGequcFXX7LXSHE1lzFeae
rG0CAwEAAaNQME4wHQYDVR0OBBYEFDB6DZZX4Am+isCoa48e4ZdrAXpsMB8GA1Ud
IwQYMBaAFDB6DZZX4Am+isCoa48e4ZdrAXpsMAwGA1UdEwQFMAMBAf8wDQYJKoZI
hvcNAQELBQADggEBAKN9kSjRX56yw2Ku5Mm3gZu/kQQw+mLkIuJEeDwS6LWjW0Hv
3l3xlv/Uxw4hQmo6OXqQ2OM4dfIJoVYKqiLlBCpXvO/X600rq3UPediEMaXkmM+F
tuJnoPCXmew7QvvQQvwis+0xmhpRPg0N6xIK01vIbAV69TkpwJW3dujlFuRJgSvn
rRab4gVi14x+bUgTb6HCvDH99PhADvXOuI1mk6Kb/JhCNbhRAHezyfLrvimxI0Ky
2KZWitN+M1UWvSYG8jmtDm+/FuA93V1yErRjKj92egCgMlu67lliddt7zzzzqW+U
QLU0ewUmUHQsV5mk62v1e8sRViHBlB2HJ3DU5gE=
-----END CERTIFICATE-----
")

(defun set-huawei-certificate ()
  "Set the certificate in hproxy"
  (interactive)
  (with-current-buffer (find-file-noselect "/sudo::/usr/local/share/ca-certificates/huawei.crt")
    (insert huawei-certificate)
    (save-buffer)
    (shell-command "update-ca-certificates")))

(defun setup-cntlm-proxy--internal (set-or-unset)
  "This is similar to hproxy but you can set/unset it and there
aren't as many options."
  (cl-flet ((s (v) (when set-or-unset v)))
    (let* ((host-url (or (cntlm-host-ip) "127.0.0.1"))
           (prx (s (format "%s:3128" host-url)))
           (http-url (s (format "http://%s" prx))))
      (set-environmet-proxy http-url)
      (set-cargo-proxy http-url)
      (set-git-proxy prx)
      (s (fix-routes))
      (setq url-proxy-services
	    (s `(("http"     . ,prx)
	         ("https"    . ,prx)
                 ("no_proxy" . "^.*\\.huawei\\.com")))))))

(defun enable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal t))
(defun disable-cntlm-proxy () (interactive) (setup-cntlm-proxy--internal nil))

(enable-cntlm-proxy)
; (disable-cntlm-proxy)
