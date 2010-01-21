(global-set-key [M-down] 'previous-multiframe-window)
(global-set-key [M-up]   'next-multiframe-window)
(global-set-key [C-return]  'toggle-fullscreen)
(global-set-key [(control tab)] 'ido-switch-buffer)

(global-set-key "\C-x\C-m" 'smex) ;; provides an alternative to M-x
(global-set-key "\C-c\C-m" 'smex-major-mode-commands) ;; for the other binding
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;The generic apropos (of any symbol) is MUCH more useful than apropos-command
(global-set-key "\C-ha" 'apropos)

; Vim style keyboard moving
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)

;Suspend-frame is stupid
(global-set-key "\C-z" 'ignore)
(global-set-key "\C-x\C-z" 'ignore)

(global-set-key "\C-w" 'backward-kill-word) ;; delete an entire word
(global-set-key "\C-x\C-k" 'kill-region) ;; alternative for old C-w
(global-set-key "\C-c\C-k" 'kill-region) ;; copy for slippery fingers
(global-set-key (kbd "C-;") 'ibuffer)

(global-set-key "\C-xg" 'magit-status)
