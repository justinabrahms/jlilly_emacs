(require 'w3m-load) ;; turns on w3m browser

;; http://www.emacswiki.org/cgi-bin/wiki/BrowseUrl
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t) ;; enable cookie use. Mmm. Cookies.

;; set buffer name to title or url
(add-hook 'w3m-display-hook
          (lambda (url)
            (rename-buffer
             (format "*w3m: %s*" (or w3m-current-title
                                     w3m-current-url)) t)))

;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

