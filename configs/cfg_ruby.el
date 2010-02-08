(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(require 'ruby-block)
(ruby-block-mode t)
