(if (file-exists-p "~/.clojure")
    (progn
      ;; clojure mode
      (require 'clojure-mode)
      ;; swank-clojure
      (add-to-list 'load-path "~/src/swank-clojure/src/emacs")
      (setq swank-clojure-jar-path "~/.clojure/clojure.jar"
            swank-clojure-extra-classpaths (list
                                            "~/src/swank-clojure/src/main/clojure"
                                            "~/.clojure/clojure-contrib.jar"))
      (require 'swank-clojure-autoload)))
