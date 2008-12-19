;; Completely stolen from Phil Hagelberg's config at http://github.com/technomancy/dotfiles
 
(require 'ansi-color) ;; has to be eval'd before load
 
(eval-after-load 'esh-opt
  ;; Can't eval-after-load eshell since calls provide at the top. eww.
  '(progn
     (require 'em-prompt)
     (require 'em-cmpl)
     (require 'em-term)
 
     (setenv "PAGER" "cat")
 
     (setq eshell-cmpl-cycle-completions nil
           eshell-save-history-on-exit t
           eshell-highlight-prompt nil)
     
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
 
     (defface eshell-branch-face
       `((t (:foreground "DeepSkyBlue3")))
       "Face for VC branch display.")
 
     (add-to-list 'eshell-visual-commands "ssh")
     ;; (add-to-list 'eshell-visual-commands "autotest")
     (add-to-list 'eshell-visual-commands "tail")
 
     (add-to-list 'eshell-command-completions-alist
                  (cons "gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
 
     (defun eshell-handle-ansi-color ()
       (ansi-color-apply-on-region eshell-last-output-start
         eshell-last-output-end))
 
     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)
 
     (add-hook 'eshell-mode-hook
   '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
 
     (setq eshell-prompt-function
           (lambda ()
             (concat (propertize (or (eshell/branch) "") 'face 'eshell-branch-face) " "
                     (propertize (concat (eshell/pwd)
                                         (if (= (user-uid) 0) " ☢ " " $"))
                                 'face 'eshell-prompt) " ")))
 
     (defun eshell/branch ()
       "Return the current git branch, if applicable."
       ;; TODO: should use VC so it works with more than just git
       ;; but VC doesn't return branch info for directories yet
       (let ((branch (shell-command-to-string "git branch")))
         (if (string-match "^\\* \\(.*\\)" branch)
             (match-string 1 branch))))
 
     (defun eshell-make-primary ()
       "Make the current buffer swap names with \"*eshell*\"."
       (interactive)
         (let ((old-name (buffer-name)))
           (switch-to-buffer "*eshell*")
           (rename-buffer "*eshell-temp*")
           (switch-to-buffer old-name)
           (rename-buffer "*eshell*")
           (switch-to-buffer "*eshell-temp*")
           (rename-buffer old-name))
         (switch-to-buffer "*eshell*"))
  
     (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\)/\\'")))
 
(provide 'my-eshell)