(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(require 'slime)
(setq inferior-lisp-program (or
                             (executable-find "sbcl")
                             "/opt/local/bin/sbcl"))
(slime-setup)