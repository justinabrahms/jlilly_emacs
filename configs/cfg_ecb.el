;; ECB
;(require 'ecb)
;(setq ecb-tip-of-the-day nil)
;(ecb-activate)
;(ecb-toggle-ecb-windows)
;
;(define-key global-map  [f7]         'ecb-toggle-ecb-windows)
;
;(custom-set-faces
; '(ecb-tag-header-face ((((class color) (background dark)) (:background "SeaGreen1" :foreground "black"))))
; '(ecb-source-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "DeepSkyBlue" :foreground "white"))))
; '(ecb-default-highlight-face ((((class color) (background dark)) (:background "DarkSlateBlue" :foreground "white"))))
; '(ecb-analyse-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "DarkSlateBlue" :foreground "white")))))
;
;;; Excludes some files from ecb
;'(ecb-source-file-regexps (quote ((".*" 
;          ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(pyc\\|elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)") 
;          ("^\\.\\(emacs\\|gnus\\)$")))))
