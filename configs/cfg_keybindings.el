(global-set-key [M-down] 'previous-multiframe-window)
(global-set-key [M-up]   'next-multiframe-window)
(global-set-key [C-return]  'toggle-fullscreen)
(global-set-key [(control tab)] 'ido-switch-buffer)

(global-set-key "\C-x\C-m" 'execute-extended-command) ;; provides an alternative to M-x
(global-set-key "\C-c\C-m" 'execute-extended-command) ;; ditto

(global-set-key "\C-w" 'backward-kill-word) ;; delete an entire word
(global-set-key "\C-x\C-k" 'kill-region) ;; alternative for old C-w
(global-set-key "\C-c\C-k" 'kill-region) ;; copy for slippery fingers
(global-set-key [f7] 'linum-mode) ;; toggle linum mode 