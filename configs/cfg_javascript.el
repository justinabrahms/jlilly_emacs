;;; stolen from starter-kit-js.el
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq
 js2-highlight-level 3
 js2-basic-offset 4
 js2-mirror-mode t
 js2-bounce-indent-p t
 js2-indent-on-enter-key t)