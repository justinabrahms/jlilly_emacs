(require 'rcirc)
;; add spell checking
(add-hook 'rcirc-mode-hook
          (lambda () (flyspell-mode 1))
          (lambda () (set (make-local-variable 'scroll-conservatively) 8192))
          (lambda () (rcirc-track-minor-mode 1)))

;; color my name
(set-face-foreground 'rcirc-my-nick "red" nil)

(setq rcirc-default-nick "justinlilly"
      rcirc-default-user-name "justinlilly"
      rcirc-default-full-name "Justin Lilly"
      rcirc-startup-channels-alist '(("\\.freenode\\.net$" "#emacs" "#rcirc" "#django-social" "#nycpython" "#pdxpython")))

;; connect to servers
(rcirc) ; freenode is default
