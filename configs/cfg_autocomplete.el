(when (require 'auto-complete nil t) ;; when autocomplete is available
  (require 'auto-complete-yasnippet)
  (require 'auto-complete-css)
  (require 'auto-complete-python)

  (global-auto-complete-mode t)
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (setq ac-auto-start 2)
  (setq ac-dwim t)
  (set-face-background 'ac-menu-face "lightgray")
  (set-face-underline 'ac-menu-face "darkgray")
  (set-face-background 'ac-selection-face "steelblue")
   (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))

  (setq ac-modes
        (append ac-modes
                '(eshell-mode
                  ;org-mode
                  )))
  ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

  (add-hook 'ruby-mode-hookp
            (lambda ()
              (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools))))))
