(setq erc-nick-uniquifier "_") ;; append _ if nick is taken
(setq erc-auto-query 'buffer) ;; add a whois when someone pms

;; auto join some channels
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#nethack" "#django-social" "#michat")))

;; keep erc from eating ram by truncating chat logs
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
