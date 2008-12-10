;;; tramp-cmds.el --- Interactive commands for Tramp

;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides all interactive commands which are releated
;; to Tramp.

;;; Code:

(require 'tramp)

(defun tramp-list-tramp-buffers ()
  "Return a list of all Tramp connection buffers."
  (append
   (all-completions
    "*tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))
   (all-completions
    "*debug tramp" (mapcar 'list (mapcar 'buffer-name (buffer-list))))))

(defun tramp-list-remote-buffers ()
  "Return a list of all buffers with remote default-directory."
  (delq
   nil
   (mapcar
    (lambda (x)
      (with-current-buffer x
	(when (and (stringp default-directory)
		   (file-remote-p default-directory))
	  x)))
    (buffer-list))))

(defun tramp-cleanup-connection (vec)
  "Flush all connection related objects.
This includes password cache, file cache, connection cache, buffers.
When called interactively, a Tramp connection has to be selected."
  (interactive
   ;; When interactive, select the Tramp remote identification.
   ;; Return nil when there is no Tramp connection.
   (list
    (let ((connections
	   (mapcar
	    (lambda (x)
	      (tramp-make-tramp-file-name
	       (tramp-file-name-method x)
	       (tramp-file-name-user x)
	       (tramp-file-name-host x)
	       (tramp-file-name-localname x)))
	    (tramp-list-connections)))
	  name)

      (when connections
	(setq name
	      (completing-read
	       "Enter Tramp connection: " connections nil t
	       (try-completion "" connections)))
	(when (and name (file-remote-p name))
	  (with-parsed-tramp-file-name name nil v))))))

  (if (not vec)
      ;; Nothing to do.
      (message "No Tramp connection found.")

    ;; Flush password cache.
    (tramp-clear-passwd vec)

    ;; Flush file cache.
    (tramp-flush-directory-property vec "/")

    ;; Flush connection cache.
    (tramp-flush-connection-property (tramp-get-connection-process vec))
    (tramp-flush-connection-property vec)

    ;; Remove buffers.
    (dolist
	(buf (list (get-buffer (tramp-buffer-name vec))
		   (get-buffer (tramp-debug-buffer-name vec))
		   (tramp-get-connection-property vec "process-buffer" nil)))
      (when (bufferp buf) (kill-buffer buf)))))

(defun tramp-cleanup-all-connections ()
  "Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers."
  (interactive)

  ;; Unlock Tramp.
  (setq tramp-locked nil)

  ;; Flush password cache.
  (when (functionp 'password-reset)
    (funcall (symbol-function 'password-reset)))

  ;; Flush file and connection cache.
  (clrhash tramp-cache-data)

  ;; Remove buffers.
  (dolist (name (tramp-list-tramp-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

(defun tramp-cleanup-all-buffers ()
  "Kill all remote buffers."
  (interactive)

  ;; Remove all Tramp related buffers.
  (tramp-cleanup-all-connections)

  ;; Remove all buffers with a remote default-directory.
  (dolist (name (tramp-list-remote-buffers))
    (when (bufferp (get-buffer name)) (kill-buffer name))))

;; Tramp version is useful in a number of situations.

(defun tramp-version (arg)
  "Print version number of tramp.el in minibuffer or current buffer."
  (interactive "P")
  (if arg (insert tramp-version) (message tramp-version)))

;; Make the `reporter` functionality available for making bug reports about
;; the package. A most useful piece of code.

(autoload 'reporter-submit-bug-report "reporter")

(defun tramp-bug ()
  "Submit a bug report to the Tramp developers."
  (interactive)
  (require 'reporter)
  (catch 'dont-send
    (let ((reporter-prompt-for-summary-p t))
      (reporter-submit-bug-report
       tramp-bug-report-address		; to-address
       (format "tramp (%s)" tramp-version) ; package name and version
       (delq nil
	     `(;; Current state
	       tramp-current-method
	       tramp-current-user
	       tramp-current-host

	       ;; System defaults
	       tramp-auto-save-directory        ; vars to dump
	       tramp-default-method
	       tramp-default-method-alist
	       tramp-default-host
	       tramp-default-proxies-alist
	       tramp-default-user
	       tramp-default-user-alist
	       tramp-rsh-end-of-line
	       tramp-default-password-end-of-line
	       tramp-login-prompt-regexp
	       ;; Mask non-7bit characters
	       (tramp-password-prompt-regexp . tramp-reporter-dump-variable)
	       tramp-wrong-passwd-regexp
	       tramp-yesno-prompt-regexp
	       tramp-yn-prompt-regexp
	       tramp-terminal-prompt-regexp
	       tramp-temp-name-prefix
	       tramp-file-name-structure
	       tramp-file-name-regexp
	       tramp-methods
	       tramp-end-of-output
	       tramp-local-coding-commands
	       tramp-remote-coding-commands
	       tramp-actions-before-shell
	       tramp-actions-copy-out-of-band
	       tramp-terminal-type
	       ;; Mask non-7bit characters
	       (tramp-shell-prompt-pattern . tramp-reporter-dump-variable)
	       ,(when (boundp 'tramp-backup-directory-alist)
		  'tramp-backup-directory-alist)
	       ,(when (boundp 'tramp-bkup-backup-directory-info)
		  'tramp-bkup-backup-directory-info)
	       ;; Dump cache.
	       (tramp-cache-data . tramp-reporter-dump-variable)

	       ;; Non-tramp variables of interest
	       ;; Mask non-7bit characters
	       (shell-prompt-pattern . tramp-reporter-dump-variable)
	       backup-by-copying
	       backup-by-copying-when-linked
	       backup-by-copying-when-mismatch
	       ,(when (boundp 'backup-by-copying-when-privileged-mismatch)
		  'backup-by-copying-when-privileged-mismatch)
	       ,(when (boundp 'password-cache)
		  'password-cache)
	       ,(when (boundp 'password-cache-expiry)
		  'password-cache-expiry)
	       ,(when (boundp 'backup-directory-alist)
		  'backup-directory-alist)
	       ,(when (boundp 'bkup-backup-directory-info)
		  'bkup-backup-directory-info)
	       file-name-handler-alist))

       'tramp-load-report-modules	; pre-hook
       'tramp-append-tramp-buffers	; post-hook
       "\
Enter your bug report in this message, including as much detail
as you possibly can about the problem, what you did to cause it
and what the local and remote machines are.

If you can give a simple set of instructions to make this bug
happen reliably, please include those.  Thank you for helping
kill bugs in Tramp.

Another useful thing to do is to put

  (setq tramp-verbose 8)

in the ~/.emacs file and to repeat the bug.  Then, include the
contents of the *tramp/foo* buffer and the *debug tramp/foo*
buffer in your bug report.

--bug report follows this line--
"))))

(defun tramp-reporter-dump-variable (varsym mailbuf)
  "Pretty-print the value of the variable in symbol VARSYM.
Used for non-7bit chars in strings."
  (let* ((reporter-eval-buffer (symbol-value 'reporter-eval-buffer))
	 (val (with-current-buffer reporter-eval-buffer
		(symbol-value varsym))))

    (if (hash-table-p val)
	;; Pretty print the cache.
	(set varsym (read (format "(%s)" (tramp-cache-print val))))
      ;; There are characters to be masked.
      (when (and (boundp 'mm-7bit-chars)
		 (string-match
		  (concat "[^" (symbol-value 'mm-7bit-chars) "]") val))
	(with-current-buffer reporter-eval-buffer
	  (set varsym (format "(base64-decode-string \"%s\""
			      (base64-encode-string val))))))

    ;; Dump variable.
    (funcall (symbol-function 'reporter-dump-variable) varsym mailbuf)

    (unless (hash-table-p val)
      ;; Remove string quotation.
      (forward-line -1)
      (when (looking-at
	     (concat "\\(^.*\\)" "\""                       ;; \1 "
		     "\\((base64-decode-string \\)" "\\\\"  ;; \2 \
		     "\\(\".*\\)" "\\\\"                    ;; \3 \
		     "\\(\")\\)" "\"$"))                    ;; \4 "
	(replace-match "\\1\\2\\3\\4")
	(beginning-of-line)
	(insert " ;; variable encoded due to non-printable characters\n"))
      (forward-line 1))

    ;; Reset VARSYM to old value.
    (with-current-buffer reporter-eval-buffer
      (set varsym val))))

(defun tramp-load-report-modules ()
  "Load needed modules for reporting."

  ;; We load message.el and mml.el from Gnus.
  (if (featurep 'xemacs)
      (progn
	(load "message" 'noerror)
	(load "mml" 'noerror))
    (require 'message nil 'noerror)
    (require 'mml nil 'noerror))
  (when (functionp 'message-mode)
    (funcall (symbol-function 'message-mode)))
  (when (functionp 'mml-mode)
    (funcall (symbol-function 'mml-mode) t)))

(defun tramp-append-tramp-buffers ()
  "Append Tramp buffers and buffer local variables into the bug report."

  (goto-char (point-max))

  ;; Dump buffer local variables.
  (dolist (buffer
	   (delq nil
		 (mapcar
		  '(lambda (b)
		     (when (string-match "\\*tramp/" (buffer-name b)) b))
		  (buffer-list))))
    (let ((reporter-eval-buffer buffer)
	  (buffer-name (buffer-name buffer))
	  (elbuf (get-buffer-create " *tmp-reporter-buffer*")))
      (with-current-buffer elbuf
	(emacs-lisp-mode)
	(erase-buffer)
	(insert "\n(setq\n")
	(lisp-indent-line)
	(funcall (symbol-function 'reporter-dump-variable)
		 'buffer-name (current-buffer))
	(dolist (varsym-or-cons-cell (buffer-local-variables buffer))
	  (let ((varsym (or (car-safe varsym-or-cons-cell)
			    varsym-or-cons-cell)))
	    (when (string-match "tramp" (symbol-name varsym))
	      (funcall
	       (symbol-function 'reporter-dump-variable)
	       varsym (current-buffer)))))
	(lisp-indent-line)
	(insert ")\n"))
      (insert-buffer-substring elbuf)))

  ;; Append buffers only when we are in message mode.
  (when (and
	 (eq major-mode 'message-mode)
	 (boundp 'mml-mode)
	 (symbol-value 'mml-mode))

    (let ((tramp-buf-regexp "\\*\\(debug \\)?tramp/")
	  (buffer-list (funcall (symbol-function 'tramp-list-tramp-buffers)))
	  (curbuf (current-buffer)))

      ;; There is at least one Tramp buffer.
      (when buffer-list
	(switch-to-buffer (list-buffers-noselect nil))
	(delete-other-windows)
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (re-search-forward
	       tramp-buf-regexp (tramp-compat-line-end-position) t)
	      (forward-line 1)
	    (forward-line 0)
	    (let ((start (point)))
	      (forward-line 1)
	      (kill-region start (point)))))
	(insert "
The buffer(s) above will be appended to this message.  If you
don't want to append a buffer because it contains sensitive data,
or because the buffer is too large, you should delete the
respective buffer.  The buffer(s) will contain user and host
names.  Passwords will never be included there.")

	(when (>= tramp-verbose 6)
	  (insert "\n\n")
	  (let ((start (point)))
	    (insert "\
Please note that you have set `tramp-verbose' to a value of at
least 6.  Therefore, the contents of files might be included in
the debug buffer(s).")
	    (add-text-properties start (point) (list 'face 'italic))))

	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(goto-char (point-min))

	(if (y-or-n-p "Do you want to append the buffer(s)? ")
	    ;; OK, let's send.  First we delete the buffer list.
	    (progn
	      (kill-buffer nil)
	      (switch-to-buffer curbuf)
	      (goto-char (point-max))
	      (insert "\n\
This is a special notion of the `gnus/message' package.  If you
use another mail agent (by copying the contents of this buffer)
please ensure that the buffers are attached to your email.\n\n")
	      (dolist (buffer buffer-list)
		(funcall (symbol-function 'mml-insert-empty-tag)
			 'part 'type "text/plain" 'encoding "base64"
			 'disposition "attachment" 'buffer buffer
			 'description buffer))
	      (set-buffer-modified-p nil))

	  ;; Don't send.  Delete the message buffer.
	  (set-buffer curbuf)
	  (set-buffer-modified-p nil)
	  (kill-buffer nil)
	  (throw 'dont-send nil))))))

(defalias 'tramp-submit-bug 'tramp-bug)

(provide 'tramp-cmds)

;;; TODO:

;; * Clean up unused *tramp/foo* buffers after a while.  (Pete Forman)
;; * WIBNI there was an interactive command prompting for Tramp
;;   method, hostname, username and filename and translates the user
;;   input into the correct filename syntax (depending on the Emacs
;;   flavor) (Reiner Steib)
;; * Let the user edit the connection properties interactively.
;;   Something like `gnus-server-edit-server' in Gnus' *Server* buffer.
;; * It's just that when I come to Customize `tramp-default-user-alist'
;;   I'm presented with a mismatch and raw lisp for a value.  It is my
;;   understanding that a variable declared with defcustom is a User
;;   Option and should not be modified by the code.  add-to-list is
;;   called in several places. One way to handle that is to have a new
;;   ordinary variable that gets its initial value from
;;   tramp-default-user-alist and then is added to. (Pete Forman)

;; arch-tag: 190d4c33-76bb-4e99-8b6f-71741f23d98c
;;; tramp-cmds.el ends here
