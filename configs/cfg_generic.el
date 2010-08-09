;; alter default behavior
(prefer-coding-system 'utf-8) ;; use utf-8

(setq user-temporary-file-directory "/tmp/")
(require 'saveplace)

(setq
 ispell-program-name "aspell"
 ispell-extra-args '("--sug-mode=ultra")
 backup-by-copying t ;; fix for Transmitt to work
 backup-by-copying-when-linked t ;; preserve hard links
 backup-by-copying-when-mismatch t ;; preserve owner:group
 frame-title-format '(buffer-file-name "%f" ("%b")) ;; titlebar = buffer unless filename
 column-number-mode t
 transient-mark-mode t ;; show selections
 indicate-empty-lines t
 history-length t ;; infinite history
 color-theme-is-global t
 magit-auto-update t
 magit-collapse-threshold nil
 save-place-file (concat user-temporary-file-directory "saveplace") ;; keep my ~/ clean
 truncate-partial-width-windows nil
 set-mark-command-repeat-pop t ; Mark-ring is navigable by typing C-u C-SPC and then repeating C-SPC forever
 auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-")
 auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t))
 display-time-day-and-date t ;; shows date in modeline
 inhibit-startup-message t ;; no splash screen
 show-trailing-whitespace t ;; show when there is excess space
 user-mail-address "justin@justinlilly.com"
 version-control t ;; user numbers for backups
 delete-old-versions t ;; silently delete extra backup versions
 default-tab-width 4 ;; a tab is 4 spaces
 ido-default-file-method 'selected-window
 ido-default-buffer-method ' selected-window
 ido-save-directory-list-file "~/.emacs.d/ido.last"
 ido-use-filename-at-point t
 ido-use-url-at-point t
 browse-url-generic-program "google-chrome"
 browse-url-browser-function 'browse-url-generic
 multi-term-program "/bin/bash")

(menu-bar-mode -1)
(when (featurep 'x) ;; when its gui..
  (tool-bar-mode -1) ;; hide the excess chrome
  (scroll-bar-mode -1)) ;; and turn off scrollbars
(show-paren-mode +1) ;; show paired parenthasis
(delete-selection-mode +1) ;; delete words if they are selected and you start typing
(auto-compression-mode +1) ;; auto compress/decompress files
(line-number-mode +1)
(auto-fill-mode +1)
(global-linum-mode +1) ;; give me some line numbers
(icomplete-mode +1) ;; incremental minibuffer completion
(global-font-lock-mode +1) ;; make pretty fonts?
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
(when (fboundp 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(when (or
       (string= "mac" window-system)
       (string= "ns" window-system))
  ;; Mac-Specific Settings
  (set-default-font
   ;; To get a font list, type xfontsel in console when the X11 server is running
   "-*-Monaco-normal-r-*-*-10-102-120-120-c-*-iso8859-1")
  (setq mac-command-modifier 'meta))

(when (string= "x" window-system)
  ;; Linux-Specific Settings
  (set-default-font
   "-*-andale mono-normal-r-*-*-13-*-*-*-*-*-iso8859-1"))

(setq-default
 org-log-done "note" ;; prompt for an org note when marking things as done
 truncate-lines t ;; truncate lines, not wrap
 save-place t ;; enable save-place globally by default
 indent-tabs-mode nil) ;; indent via spaces not tabs

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
(require 'undo-tree)
(global-undo-tree-mode)
(require 'dpaste)
(require 'jira)
(setq-default jira-url "http://jira.invitemedia.com/rpc/xmlrpc")
(winner-mode t) ;; turn on saved buffer configs
(require 'smooth-scrolling) ;; stop text from jumping on scroll.
(require 'smex)
(require 'column-marker)
(require 'gist)
(require 'browse-kill-ring)
(require 'vernacular-time)
(browse-kill-ring-default-keybindings)
(require 'etags-table)
(require 'etags-select)
(require 'multi-term)
(require 'idle-highlight)
(require 'framemove)
(setq framemove-hook-into-windmove t)
;; (require 'ectags)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; VISUAL DISPLAY
(display-time) ;; shows datetime in the mode line

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

                                        ;Reload .emacs on the fly
(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs.d/init.el")
  (message ".emacs reloaded successfully"))

(setq initial-frame-alist '((top . 1)
                            (left . 1)
                            (width . 100)
                            (height . 45)))

(defun fixme ()
  (interactive)
  (find-grep "find . -type f -exec grep -nH -e \"\\(FIXME\\|TODO\\)\" {} /dev/null \\;"))

;; MISC CRAP
(defalias 'qrr 'query-replace-regexp)
