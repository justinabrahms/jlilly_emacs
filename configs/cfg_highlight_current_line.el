(require 'highlight-current-line)

                                        ; good: 4f2f42
                                        ; midnight blue
                                        ; saddle brown
(cond (window-system
       (custom-set-faces
        '(highlight-current-line-face ((t (:background "#4f2f42")))))
       (custom-set-variables
        '(highlight-current-line-globally t nil (highlight-current-line)))))