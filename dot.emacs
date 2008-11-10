;; TODO
;; code following
;; code folding

(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/configs" load-path))
(setq load-path (cons "~/.emacs.d/mmm-mode" load-path))
(setq load-path (cons "~/.emacs.d/color-theme" load-path))
(setq load-path (cons "~/.emacs.d/tramp" load-path))
(setq load-path (cons "~/.emacs.d/python-mode" load-path))
(setq load-path (cons "/Applications/Emacs.app/Contents/Resources/site-lisp" load-path))
(setq load-path (cons "/Applications/Emacs.app/Contents/Resources/lisp" load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp/w3m" load-path))

(defconst emacs-config-dir "~/.emacs.d/configs/" "")

(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat emacs-config-dir file)))
    (message "Loaded config file: %s" file)
    ))

(load-cfg-files '("cfg_generic"
                  "cfg_mmmode"
                  "cfg_python"
                  "cfg_tramp"
                  "cfg_color-theme"
                  "cfg_ido"
                  "cfg_snippets"
                  "cfg_git"
                  "cfg_orgmode"
                  "cfg_linum"
                  "cfg_keybindings"
                  "cfg_erc"
                  "cfg_css-mode"
                  "cfg_javascript"
                  "cfg_server"
                  "cfg_highlight_current_line"
                  "cfg_uniquify"
                  "cfg_calendar"
                  "cfg_twitter"
                  "cfg_browser"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(erc-nick-uniquifier "_")
 '(highlight-current-line-globally t nil (highlight-current-line))
 '(ido-max-directory-size 90000)
 '(ido-max-work-directory-list 500)
 '(mark-holidays-in-calendar t)
 '(rst-level-face-base-light 25))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-analyse-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "DarkSlateBlue" :foreground "white"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "DarkSlateBlue" :foreground "white"))))
 '(ecb-source-face ((((class color) (background dark)) (:inherit ecb-default-highlight-face :background "DeepSkyBlue" :foreground "white"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "SeaGreen1" :foreground "black"))))
 '(flymake-errline ((((class color)) (:background "Red" :weight bold))))
 '(flymake-warnline ((((class color)) (:underline "orange" :slant italic :weight bold))))
 '(highlight-current-line-face ((t (:background "black"))))
 '(ido-first-match ((t (:background "Blue" :foreground "white"))))
 '(ido-only-match ((((class color)) (:foreground "Blue"))))
 '(twitter-time-stamp-face ((t (:slant italic))))
 '(twitter-user-name-face ((t (:weight bold))))
 '(yas/field-highlight-face ((t nil))))
