;; IDO Rules!
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t) ;; pyf will match pythonfile
(setq ido-use-filename-at-point t)
(setq ido-auto-merge-work-directories-length -1)

(global-set-key [(control tab)] 'ido-switch-buffer)

(custom-set-variables
        '(ido-max-work-directory-list 500)
        '(ido-max-directory-size 90000))

(custom-set-faces
        '(ido-only-match ((((class color)) (:foreground "Blue"))))
        '(ido-first-match ((t (:background "Blue" :foreground "white")))))

;; function below will allow you to run ido to open a recently closed file
(require 'recentf)
(setq recentf-max-saved-items 100)
(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list`"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'ido-choose-from-recent)