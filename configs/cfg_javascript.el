;;; stolen from starter-kit-js.el
 
(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook 'idle-highlight)
(setq espresso-indent-level 2)
 
(eval-after-load 'espresso
  '(progn (define-key espresso-mode-map "{" 'paredit-open-curly)
          (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
          ;; fixes problem with pretty function font-lock
          (define-key espresso-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords 'espresso-mode
                        '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                           1 font-lock-warning-face t)))
          (font-lock-add-keywords
           'espresso-mode `(("\\(function *\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))))))
