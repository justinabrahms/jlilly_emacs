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

;; VISUAL DISPLAY
(setq display-time-day-and-date t) ;; shows date in modeline 
(display-time) ;; shows datetime in the mode line
(setq inhibit-startup-message t) ;; no splash screen
(tool-bar-mode -1) ;; hide the excess chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default transient-mark-mode t) ;; shows current selected region
(set-default-font
 "-apple-consolas-medium-r-normal--12-120-72-72-m-120-mac-roman")
(show-paren-mode 1) ;; show paired parenthasis


;; full screen toggle using command+[RET]
(defun toggle-fullscreen () 
  (interactive) 
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                               nil 
                                               'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen) 

