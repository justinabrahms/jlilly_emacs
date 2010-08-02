(require 'yasnippet)
(require 'cl)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(loop for dir in '("~/.emacs.d/snippets"
                   "~/.emacs.d/vendor/yasnippet/snippets")
      do (yas/load-directory dir))
