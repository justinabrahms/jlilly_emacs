;;; stolen from starter-kit-js.el
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq
 js2-highlight-level 3
 js2-basic-offset 4
 js2-mirror-mode t
 js2-bounce-indent-p t
 js2-indent-on-enter-key t)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-hook 'js2-mode-hook 'idle-highlight)

(eval-after-load 'js2-mode
  '(progn
     ;; fixes problem with pretty function font-lock
     (define-key js2-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords 'js2-mode
                             '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                                1 font-lock-warning-face t)))
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                        (0 (progn (compose-region (match-beginning 1)
                                                  (match-end 1) "ƒ")
                                  nil)))))))
