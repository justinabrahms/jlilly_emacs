;; DIRED
(require 'dired-details) ;; hide useless permission info in dired
(dired-details-install)
(setq dired-details-hidden-string "")

(require 'dired-single)

(require 'dired-x) 
(setq dired-omit-files 
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files 
              (seq "~" eol)                 ;; backup-files 
              (seq bol "svn" eol)           ;; svn dirs 
              (seq ".pyc" eol)
              (seq ".git" eol)              ;; misc git files
              (seq ".gitignore" eol)
              ))) 
(setq dired-omit-extensions 
      (append dired-latex-unclean-extensions 
              dired-bibtex-unclean-extensions 
              dired-texinfo-unclean-extensions)) 
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))) 
(put 'dired-find-alternate-file 'disabled nil)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
	(function
	 (lambda nil (interactive) (dired-single-buffer "..")))))


;; Pulled from dired-single's docs
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
	;; we're good to go; just add our bindings
	(my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(global-set-key [(f3)] 'dired-single-magic-buffer)
(global-set-key [(control f3)] (function
	(lambda nil (interactive)
        (dired-single-magic-buffer default-directory))))
(global-set-key [(shift f3)] (function
        (lambda nil (interactive)
        (message "Current directory is: %s" default-directory))))
(global-set-key [(meta f3)] 'dired-single-toggle-buffer-name)
