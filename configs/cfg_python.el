;;; python-mode site-lisp configuration
(setq load-path (cons "/Users/jlilly/.elisp/python-mode" load-path))
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'doctest-mode "doctest-mode" "Editing mode for Python Doctest examples." t)
;(require 'pycomplete)

; pymacs & rope
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

; django
(define-skeleton django-trans
   	  "django translate template tag"
   	  nil
   	  "{% trans '" _ "' %}")
(define-key global-map  "\C-xt"         'django-trans)

; python doc search
(defun py-doc-search (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string 
                        (format "pydoc entry%s: " 
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "") 
               (if (not word) (error "No pydoc args given")
                 word)                  ;sinon word
             input))))                  ;sinon input
  (shell-command (concat py-python-command " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(add-hook 'python-mode-hook (lambda () (local-set-key "\C-c\C-f" 'py-doc-search)))

;; flymake + pylint
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))