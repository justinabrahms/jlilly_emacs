;;; python-mode site-lisp configuration

;; highlight all characters beyond col #80
(require 'highlight-80+)

(add-hook 'python-mode-hook
          '(lambda () (eldoc-mode 1)) t)
(add-hook 'python-mode-hook
          '(lambda () (highlight-80+-mode 1)) t)

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
;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "epylint" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pylint-init)))
