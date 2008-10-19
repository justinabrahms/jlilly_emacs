;; IDO Rules!
(require 'ido)
(ido-mode t)
(global-set-key [(control tab)] 'ido-switch-buffer)

(custom-set-variables
        '(ido-max-work-directory-list 500)
        '(ido-max-directory-size 90000))

(custom-set-faces
        '(ido-only-match ((((class color)) (:foreground "Blue"))))
        '(ido-first-match ((t (:background "Blue" :foreground "white")))))
