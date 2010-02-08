(load-file "~/.emacs.d/packages/cedet-1.0pre6/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-excessive-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu
(require 'semantic-ia)                   ; allows name completion, tag & class info, etc.