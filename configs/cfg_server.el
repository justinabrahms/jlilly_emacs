;; for use with emacsclient
(server-start)
(add-hook 'server-switch-hook
          (lambda ()
            (local-set-key (kbd "C-x k") 'server-edit)))