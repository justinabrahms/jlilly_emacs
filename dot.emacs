;; TODO
;; code following
;; interactive interpreter
;; check into code snippets

(setq load-path (cons "~/.elisp" load-path))
(setq load-path (cons "~/.elisp/configs" load-path))
(setq load-path (cons "~/.elisp/ecb" load-path))
(setq load-path (cons "~/.elisp/cedet/common" load-path))
(setq load-path (cons "~/.elisp/cedet/semantic" load-path))
(setq load-path (cons "~/.elisp/cedet/eieio" load-path))
(setq load-path (cons "~/.elisp/cedet/ede" load-path))
(setq load-path (cons "~/.elisp/cedet/speedbar" load-path))
(setq load-path (cons "~/.elisp/mmm-mode" load-path))
(setq load-path (cons "~/.elisp/color-theme" load-path))
(setq load-path (cons "~/.elisp/tramp" load-path))
(setq load-path (cons "~/.elisp/python-mode" load-path))
(setq load-path (cons "/Applications/Emacs.app/Contents/Resources/site-lisp" load-path))
(setq load-path (cons "/Applications/Emacs.app/Contents/Resources/lisp" load-path))

(defconst emacs-config-dir "~/.elisp/configs/" "")

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
;                  "cfg_cedet"
;                  "cfg_ecb"
                  "cfg_git"
                  "cfg_linum"
                  "cfg_keybindings"
                  "cfg_css-mode"
                  "cfg_javascript"
                  "cfg_highlight_current_line"
                  "cfg_browser"))

