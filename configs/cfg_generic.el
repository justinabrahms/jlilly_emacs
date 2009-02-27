;; ALTER DEFAULT BEHAVIOR
(prefer-coding-system 'utf-8) ;; use UTF-8
(setq user-temporary-file-directory "/tmp")
(setq backup-by-copying t ;; fix for Transmitt to work
      backup-by-copying-when-linked t ;; preserve hard links
      backup-by-copying-when-mismatch t ;; preserve owner:group
      frame-title-format '(buffer-file-name "%f" ("%b")) ;; titlebar = buffer unless filename
      transient-mark-mode t
      save-place t
      indicate-empty-lines t
      color-theme-is-global t
      magit-auto-update t
      magit-collapse-threshold nil

      auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-")

      auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t))

      display-time-day-and-date t ;; shows date in modeline
      inhibit-startup-message t ;; no splash screen
)



(tool-bar-mode -1) ;; hide the excess chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1) ;; show paired parenthasis
(set-default-font
 "-apple-andale mono-medium-r-normal--12-100-72-72-m-100-iso10646-1")
          



(setq-default indent-tabs-mode nil) ;; indent via spaces not tabs
(global-font-lock-mode 1) ;; make pretty fonts?
(toggle-debug-on-error t) ;; show traceback on error
(fset 'yes-or-no-p 'y-or-n-p) ;; allows you to type "y" instead of "yes" on exit
(mouse-avoidance-mode 'cat-and-mouse) ;; mouse jumps away when typing under it
(if (load "mwheel" t)
    (mwheel-install)) ;; turn on the mouse wheel 

;; enable windmove if the package is available
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; stop leaving # files and ~ files strewn about. put them in a temp folder
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

;; One of Apps
(require 'rst) ;; require ReST mode
(require 'textmate) ;; defunkt's textmate.el
(textmate-mode)
(require 'dpaste)
(require 'magit)
;;(require 'egg) ;; will need to choose between this one and magit, I think.
(require 'jira)
(winner-mode t) ;; turn on saved buffer configs
;; DIRED
(require 'dired-details) ;; hide useless permission info in dired
(dired-details-install)
(setq dired-details-hidden-string "")
(require 'smooth-scrolling) ;; stop text from jumping on scroll.

;; VISUAL DISPLAY
(display-time) ;; shows datetime in the mode line


;; full screen toggle using command+[RET]
(defun toggle-fullscreen () 
  (interactive) 
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                               nil 
                                               'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen) 

;; MISC CRAP
(defalias 'qrr 'query-replace-regexp)
