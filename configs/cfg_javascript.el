;; http://www.emacsblog.org/2008/04/04/package-faves-js2-mode/
;; http://steve-yegge.blogspot.com/2008/03/js2-mode-new-javascript-mode-for-emacs.html
(setq load-path (cons "/home/jlilly/.emacs.d/packages/js2-mode/build" load-path))
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))