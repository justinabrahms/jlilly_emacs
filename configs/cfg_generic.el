;; ALTER DEFAULT BEHAVIOR
(prefer-coding-system 'utf-8) ;; use UTF-8
(setq user-temporary-file-directory "/tmp/")
(setq
 backup-by-copying t ;; fix for Transmitt to work
 backup-by-copying-when-linked t ;; preserve hard links
 backup-by-copying-when-mismatch t ;; preserve owner:group
 frame-title-format '(buffer-file-name "%f" ("%b")) ;; titlebar = buffer unless filename
 transient-mark-mode t
 save-place t
 indicate-empty-lines t
 color-theme-is-global t
 magit-auto-update t
 magit-collapse-threshold nil

 set-mark-command-repeat-pop t ; Mark-ring is navigable by typing C-u C-SPC and then repeating C-SPC forever

 auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-")

 auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t))

 display-time-day-and-date t ;; shows date in modeline
 inhibit-startup-message t ;; no splash screen
 show-trailing-whitespace t ;; show when there is excess space
 user-mail-address "justinlilly@gmail.com"
 version-control t ;; user numbers for backups
 delete-old-versions t ;; silently delete extra backup versions

 ido-default-file-method 'selected-window
 ido-default-buffer-method ' selected-window
 ido-save-directory-list-file "~/.emacs.d/ido.last"
 ido-use-filename-at-point t
 ido-use-url-at-point t
 save-place t
)



(menu-bar-mode -1)
(when (featurep 'x) ;; when its gui..
  (tool-bar-mode -1) ;; hide the excess chrome
  (scroll-bar-mode -1)) ;; and turn of scrollbars
(show-paren-mode +1) ;; show paired parenthasis
(delete-selection-mode +1) ;; delete words if they are selected and you start typing
(auto-compression-mode +1) ;; auto compress/decompress files
(line-number-mode +1)
(auto-fill-mode +1)
(icomplete-mode +1) ;; incremental minibuffer completion
(when (fboundp 'shell-command-completion-mode)
  (shell-command-completion-mode +1))
(when (fboundp 'savehist-mode)
  (savehist-mode +1))
(if (not (fboundp 'ido-mode))
    (iswitchb-mode +1)                  ; fall back on iswitchb
  (ido-mode +1)
  (when (fboundp 'ido-everywhere)       ; not in emacs-goodies-el's ido
    (ido-everywhere +1)))
(mapc (lambda (x)
        (add-hook x 'turn-on-eldoc-mode))
      ;; A major mode supports eldoc iff it defines
      ;; `eldoc-documentation-function'.
      '(emacs-lisp-mode-hook ielm-mode-hook))
(when (fboundp 'paredit-mode)
  (mapc (lambda (x)
          (add-hook x 'enable-paredit-mode))
        '(emacs-lisp-mode-hook ielm-mode-hook
          lisp-mode-hook inferior-lisp-mode-hook
          slime-repl-mode-hook
          scheme-mode-hook inferior-scheme-mode-hook
          haskell-mode-hook literate-haskell-mode-hook inferior-haskell-mode-hook
          python-mode-hook inferior-python-mode-hook)))
(when (fboundp 'slime-mode)
  (add-hook 'lisp-mode-hook 'slime-mode))
(mapc (lambda (major-mode) ; Similar to http://emacswiki.org/wiki/PrettyLambda
        (font-lock-add-keywords major-mode
          `(("(\\(lambda\\)\\>"
             (0 (prog1 ()
                  (compose-region (match-beginning 1)
                                  (match-end 1)
                                  ,(make-char 'greek-iso8859-7 107))))))))
      '(emacs-lisp-mode
        inferior-emacs-lisp-mode
        lisp-mode
        slime-repl-mode
        inferior-lisp-mode
        scheme-mode
        scheme48-mode
        inferior-scheme-mode))
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))
(when (fboundp 'org-mode)
  (add-to-list 'auto-mode-alist '("/TODO\\'" . org-mode)))



(when (string= "mac" window-system)
  ;; Mac-Specific Settings
  (set-default-font
   "-apple-andale mono-medium-r-normal--12-100-72-72-m-100-iso10646-1"))
(when (string= "x" window-system)
  ;; Linux-Specific Settings
  (set-default-font
   "-unknown-DejaVu Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1"))

(setq-default truncate-lines t) ;; truncate lines, not wrap
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
(defconst use-backup-dir t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))

;; One of Apps
(require 'rst) ;; require ReST mode
(require 'textmate) ;; defunkt's textmate.el
(textmate-mode)
(require 'dpaste)
(require 'magit)
(require 'egg) ;; will need to choose between this one and magit, I think.
(require 'jira)
(winner-mode t) ;; turn on saved buffer configs
;; DIRED
(require 'dired-details) ;; hide useless permission info in dired
(dired-details-install)
(setq dired-details-hidden-string "")
(require 'smooth-scrolling) ;; stop text from jumping on scroll.
(require 'smex)
(require 'column-marker)
(require 'gist)
(require 'lorem-ipsum)
(require 'dired-single)
(require 'ectags)

;; VISUAL DISPLAY
(display-time) ;; shows datetime in the mode line


;; full screen toggle using command+[RET]
(defun toggle-fullscreen ()
  (interactive)
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                               nil
                                               'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; Increase/Decrease font size on the fly
;; Taken from: http://is.gd/iaAo
(defun ryan/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun ryan/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'ryan/increase-font-size)
(global-set-key (kbd "C--") 'ryan/decrease-font-size)

;; Functions for configuring window geometry and placement
(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  ; From http://hjiang.net/archives/253
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has 
     80 columns."
    (if (> (window-width w) (* 2 81))
    (let ((w2 (split-window w 82 t)))
      (smart-split-helper w2))))
  (smart-split-helper nil))

;Reload .emacs on the fly
(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))

;; MISC CRAP
(defalias 'qrr 'query-replace-regexp)
