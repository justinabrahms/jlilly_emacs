;; Configurations for orgmode
(setq org-hide-leading-stars t)
(setq org-return-follows-link t)
;; The abbrev list allows me to insert links like
;; [[foo:google]]
;; which will google for "foo"
(setq org-link-abbrev-alist
      '(("google"  . "http://www.google.com/search?q=")))
(setq org-todo-keywords '("TODO" "WAITING" "IN PROGRESS" "DONE")
      org-todo-interpretation 'sequence)
(setq org-log-done t) ;; timestamp when things are marked completed
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))