;; TODO
;; code following
;; code folding

(setq load-path (cons "~/.emacs.d/" load-path))
(setq load-path (cons "~/.emacs.d/configs/" load-path))
(setq load-path (cons "~/.emacs.d/vendor/" load-path))
(setq load-path (cons "~/.emacs.d/packages/pymacs/" load-path))


(defconst emacs-config-dir "~/.emacs.d/configs/")
(defconst emacs-vendor-dir "~/.emacs.d/vendor/")
(defun get-subdirs (directory)
  "Get a list of subdirectories under a given directory"
  (apply 'nconc (mapcar (lambda (fa)
                        (and
                         (eq (cadr fa) t)
                         (not (equal (car fa) "."))
                         (not (equal (car fa) ".."))
                         (list (car fa))))
                        (directory-files-and-attributes directory))))

(defun add-vendors-to-loadpath ()
  "add subdirs of your vendor directory to the load path"
  (dolist (subdir (get-subdirs emacs-vendor-dir))
    (setq load-path (cons (concat emacs-vendor-dir subdir) load-path))
    (message "Added %s to load path" subdir)))

(add-vendors-to-loadpath)


(defun load-cfg-files (filelist)
  (dolist (file filelist)
    (load (expand-file-name
           (concat emacs-config-dir file)))
    (message "Loaded config file: %s" file)))

(load-cfg-files '("cfg_generic"
                   "cfg_python"
                   "cfg_tramp"
                   "cfg_ido"
                   "cfg_snippets"
                   "cfg_html"
                   ;; "cfg_nxhtml"
                   "cfg_lorem"
                   "cfg_scheme"
                   "cfg_git"
                   "cfg_diredx"
                   "cfg_orgmode"
                   "cfg_slime"
                   "cfg_keybindings"
                   "cfg_erc"
                   "cfg_css-mode"
                   "cfg_javascript"
                   "cfg_server"
                   "cfg_uniquify"
                   "cfg_calendar"
                   "cfg_twitter"
                   "cfg_eshell"
                   "cfg_color-theme"
                   "cfg_autocomplete"
                   "cfg_lisp"
                   "cfg_last"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(calendar-mark-holidays-flag t)
 '(egg-enable-tooltip t)
 '(erc-modules (quote (autojoin button completion fill irccontrols match menu netsplit noncommands readonly ring services stamp track)))
 '(highlight-current-line-globally t nil (highlight-current-line))
 '(ido-max-directory-size 90000)
 '(ido-max-work-directory-list 500)
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(quack-default-program "mzscheme")
 '(quack-fontify-style (quote emacs))
 '(quack-global-menu-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-run-scheme-prompt-defaults-to-last-p t)
 '(quack-tabs-are-evil-p t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blue ((t (:foreground "RGB6699ff"))))
 '(border ((t (:background "white" :foreground "black"))))
 '(cursor ((t (:background "yellow" :foreground "black"))))
 '(erc-notice-face ((t (:weight normal))))
 '(flymake-errline ((((class color)) (:background "Red" :weight bold))))
 '(flymake-warnline ((((class color)) (:underline "orange" :slant italic :weight bold))))
 '(highlight-current-line-face ((t (:background "#333333"))))
 '(ido-first-match ((t (:background "Blue" :foreground "white"))))
 '(ido-only-match ((((class color)) (:foreground "Blue"))))
 '(italic ((t (:slant normal))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "green"))))
 '(mmm-init-submode-face ((t (:background "pink" :foreground "black"))))
 '(mode-line-buffer-id ((t (:weight normal))))
 '(mode-line-emphasis ((t (:weight normal))))
 '(mouse ((t (:background "white" :foreground "black"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil)))
 '(paren-face-match-light ((((class color)) (:background "RGB333333"))))
 '(rst-level-1-face ((t (:background "grey85" :foreground "black"))) t)
 '(rst-level-2-face ((t (:background "grey78" :foreground "black"))) t)
 '(twitter-time-stamp-face ((t (:slant italic))))
 '(twitter-user-name-face ((t (:weight bold))))
 '(yas/field-highlight-face ((t nil))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)
