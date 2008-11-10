(require 'w3m-load) ;; turns on w3m browser

;; http://www.emacswiki.org/cgi-bin/wiki/BrowseUrl
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t) ;; enable cookie use. Mmm. Cookies.

(setq w3m-use-title-buffer-name t) ;; set the buffer name to website title


;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
