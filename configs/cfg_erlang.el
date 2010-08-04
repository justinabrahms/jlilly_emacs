;;; Needs a bit of work to properly set paths, but shouldn't error if
;;; erlang isn't installed.

(if (executable-find "erlang")
    (cond
     (setq load-path (cons "/usr/local/Cellar/erlang/R13B02-1/lib/erlang/lib/tools-2.6.4/emacs" load-path))
     (setq erlang-root-dir "/usr/local/Cellar/erlang/R13B02-1/")
     (setq exec-path (cons "/usr/local/Cellar/erlang/R13B02-1/bin" exec-path))
     (require 'erlang-start)))
