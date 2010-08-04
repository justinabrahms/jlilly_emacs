(require 'rcirc)
(add-hook 'rcirc-mode-hook (lambda ()
                             (flyspell-mode 1)))

(set-face-foreground 'rcirc-my-nick "red" nil)

(setq
 rcirc-time-format "%Y.%m.%d %H:%M "
 rcirc-default-nick "justinlilly"
 rcirc-default-user-name "justinlilly"
 rcirc-default-full-name "Justin Lilly"
 rcirc-server-alist '(("irc.freenode.net" :channels ("#django-social" "#nycpython"))
                      ("irc.efnet.org" :channels ("#avendar") :nick "aunes"))
 rcirc-prompt "%t> "
 rcirc-keywords '("justinlilly" "jlilly" "gencal")
 rcirc-track-minor-mode t)
