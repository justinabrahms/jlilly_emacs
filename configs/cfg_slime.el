(if (executable-find "sbcl")
    (progn
      (eval-after-load "slime"
	'(progn (slime-setup '(slime-repl))))


      (require 'slime)
      (setq inferior-lisp-program (executable-find "sbcl"))
      (slime-setup)))