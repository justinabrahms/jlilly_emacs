(setq inhibit-startup-message t) ;; no splash screen
(prefer-coding-system 'utf-8) ;; use UTF-8
(setq backup-by-copying-when-linked t) ;; preserve hard links
(setq backup-by-copying-when-mismatch t) ;; preserve owner:group
(global-font-lock-mode 1) ;; make pretty fonts?

(setq-default transient-mark-mode t) ;; shows current selected region
(setq-default indent-tabs-mode nil) ;; indent via spaces not tabs
(setq frame-title-format '(buffer-file-name "%f" ("%b"))) ;; titlebar = buffer unless filename
(show-paren-mode 1) ;; show paired parenthasis
(set-default-font
 "-apple-consolas-medium-r-normal--12-120-72-72-m-120-mac-roman")
(tool-bar-mode -1) ;; hide the excess chrome
(menu-bar-mode -1)
(scroll-bar-mode -1)