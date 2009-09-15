;;; pllab.el --- CS HUJI customization for pllab course

;; Maintainer: CSE HUJI emacs guru <emacs@cs.huji.ac.il>
;; Keywords: emacs pllab scheme quack
;; URL: http://www.cs.huji.ac.il/support/emacs/src/lisp/pllab.el
;; $Id$

;; This file is *NOT* part of GNU Emacs

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This file provides some customization for pllab course and scheme programming

;;; Bugs:
;; Please send bug reports to CSE HUJI emacs guru <emacs@cs.huji.ac.il>


;;; Code:
(defconst cse-huji-pllab-revision
  (let ((rcs-rev "$Revision: 1.2 $"))
    (condition-case err
	(save-match-data
	  (string-match "Revision: \\([0-9]+\\.[0-9]+\\)" rcs-rev)
	  (substring rcs-rev (match-beginning 1) (match-end 1)))
      ('error rcs-rev)))
  "Revision number of huji-cse-pllab library.")

(defgroup cse-huji nil
   "CSE HUJI customizations"
   :group 'local)

(defgroup courses nil
  "CSE HUJI courses specific cutomization group"
  :group 'cse-huji)

(defgroup pllab nil
  "CSE HUJI Pllab course specific cutomization group"
  :group 'courses)


;; add to path different elisp files for scheme support
(add-to-list 'load-path (expand-file-name "/cs/share/emacs/site-lisp/scheme"))


;; remove quack from menu bar
;;(setq quack-global-menu-p nil)
;;(add-hook 'after-init-hook 'quack-install-global-menu)
(add-hook 'scheme-mode-hook
	  ( function (lambda ()
		       (local-set-key [(control c) (control c)] 'comment-region )
		       (local-unset-key [(control c) (meta c)])
		       )))


(custom-set-variables
 '(quack-default-program "mzscheme")
 '(quack-fontify-style (quote emacs))
 '(quack-global-menu-p nil)
 '(quack-run-scheme-always-prompts-p nil)
 '(quack-run-scheme-prompt-defaults-to-last-p t)
 '(quack-tabs-are-evil-p t)
)

(custom-set-faces
  ;; Your init file should contain only one such instance.
;  '(quack-pltish-comment-face ((((class color) (background dark)) (:foreground "chocolate1"))))
)


(require 'quack)


;; scheme-send-region adds to input history of scheme buffer
;; and then commands sent by C-c C-r will appear in scheme interpreter buffer history.
;; and you can select them with M-p M-n (look at In/Out section of menu in scheme buffer)
;;
;; idea : emacswiki
(defadvice scheme-send-region (after scheme-store-in-history)
  "The region sent to the scheme process is also stored in the history."
  (let ((history (buffer-substring-no-properties start end)))
    (save-excursion
      (set-buffer scheme-buffer)
      (message history)
      (if (and (funcall comint-input-filter history)
	       (or (null comint-input-ignoredups)
		   (not (ring-p comint-input-ring))
		   (ring-empty-p comint-input-ring)
		   (not (string-equal (ring-ref comint-input-ring 0)
				      history))))
	  (ring-insert comint-input-ring history))
      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil))))
(ad-activate 'scheme-send-region)

(provide 'pllab )

;;; pllab.el ends here
