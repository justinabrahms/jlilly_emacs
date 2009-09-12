(require 'yasnippet-bundle)
(add-to-list 'yas/extra-mode-hooks 'html-mode-hook)
(add-to-list 'yas/extra-mode-hooks 'django-mode-hook)
(setq yas/text-popup-function
      'yas/dropdown-list-popup-for-template)
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/snippets")
(yas/load-directory "~/.emacs.d/snippets")