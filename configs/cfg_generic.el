;; ALTER DEFAULT BEHAVIOR
(prefer-coding-system 'utf-8) ;; use UTF-8
(setq backup-by-copying t) ;; fix for Transmitt to work
(setq backup-by-copying-when-linked t) ;; preserve hard links
(setq backup-by-copying-when-mismatch t) ;; preserve owner:group
(global-font-lock-mode 1) ;; make pretty fonts?
(setq-default indent-tabs-mode nil) ;; indent via spaces not tabs
(setq frame-title-format '(buffer-file-name "%f" ("%b"))) ;; titlebar = buffer unless filename
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
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))


;; One of Apps
(require 'rst) ;; require ReST mode
;; DIRED
(require 'dired-details) ;; hide useless permission info in dired
(dired-details-install)
(setq dired-details-hidden-string "")
(require 'smooth-scrolling) ;; stop text from jumping on scroll.

;; VISUAL DISPLAY
(setq display-time-day-and-date t) ;; shows date in modeline 
(display-time) ;; shows datetime in the mode line
(setq inhibit-startup-message t) ;; no splash screen
(tool-bar-mode -1) ;; hide the excess chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default transient-mark-mode t) ;; shows current selected region
;(set-default-font
; "-apple-andale mono-medium-r-normal--10-100-72-72-m-100-iso10646-1")
(show-paren-mode 1) ;; show paired parenthasis


;; full screen toggle using command+[RET]
(defun toggle-fullscreen () 
  (interactive) 
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                               nil 
                                               'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen) 

;; MISC CRAP
(defalias 'qrr 'query-replace-regexp)
