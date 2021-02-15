(use-package prescient)
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode 1)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(provide 'fd-selectrum)
