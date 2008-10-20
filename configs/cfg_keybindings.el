(global-set-key [M-down] 'previous-multiframe-window)
(global-set-key [M-up]   'next-multiframe-window)

(global-set-key [(control tab)] 'ido-switch-buffer)

(global-set-key [(control shift a)] 'mark-whole-buffer)
(global-set-key "\C-x\C-m" 'execute-extended-command) ;; provides an alternative to M-x
(global-set-key "\C-c\C-m" 'execute-extended-command) ;; ditto
