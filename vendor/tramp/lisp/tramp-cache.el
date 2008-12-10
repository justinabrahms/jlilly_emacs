;;; tramp-cache.el --- file information caching for Tramp

;; Copyright (C) 2000, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@inanna.danann.net>
;;         Michael Albinus <michael.albinus@gmx.de>
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

;; An implementation of information caching for remote files.

;; Each connection, identified by a vector [method user host
;; localname] or by a process, has a unique cache. We distinguish 3
;; kind of caches, depending on the key:
;;
;; - localname is NIL.  This are reusable properties.  Examples:
;;   "remote-shell" identifies the POSIX shell to be called on the
;;   remote host, or "perl" is the command to be called on the remote
;;   host, when starting a Perl script.  These properties are saved in
;;   the file `tramp-persistency-file-name'.
;;
;; - localname is a string.  This are temporary properties, which are
;;   related to the file localname is referring to.  Examples:
;;   "file-exists-p" is t or nile, depending on the file existence, or
;;   "file-attributes" caches the result of the function
;;  `file-attributes'.
;;
;; - The key is a process.  This are temporary properties related to
;;   an open connection.  Examples: "scripts" keeps shell script
;;   definitions already sent to the remote shell, "last-cmd-time" is
;;   the time stamp a command has been sent to the remote process.

;;; Code:

;; Pacify byte-compiler.
(eval-when-compile
  (require 'cl)
  (autoload 'tramp-message "tramp")
  (autoload 'tramp-tramp-file-p "tramp")
  ;; We cannot autoload macro `with-parsed-tramp-file-name', it
  ;; results in problems of byte-compiled code.
  (autoload 'tramp-dissect-file-name "tramp")
  (autoload 'tramp-file-name-method "tramp")
  (autoload 'tramp-file-name-user "tramp")
  (autoload 'tramp-file-name-host "tramp")
  (autoload 'tramp-file-name-localname "tramp")
  (autoload 'time-stamp-string "time-stamp"))

;;; -- Cache --

(defvar tramp-cache-data (make-hash-table :test 'equal)
  "Hash table for remote files properties.")

(defcustom tramp-persistency-file-name
  (cond
   ;; GNU Emacs.
   ((and (boundp 'user-emacs-directory)
	 (stringp (symbol-value 'user-emacs-directory))
	 (file-directory-p (symbol-value 'user-emacs-directory)))
    (expand-file-name "tramp" (symbol-value 'user-emacs-directory)))
   ((and (not (featurep 'xemacs)) (file-directory-p "~/.emacs.d/"))
    "~/.emacs.d/tramp")
   ;; XEmacs.
   ((and (boundp 'user-init-directory)
	 (stringp (symbol-value 'user-init-directory))
	 (file-directory-p (symbol-value 'user-init-directory)))
    (expand-file-name "tramp" (symbol-value 'user-init-directory)))
   ((and (featurep 'xemacs) (file-directory-p "~/.xemacs/"))
    "~/.xemacs/tramp")
   ;; For users without `~/.emacs.d/' or `~/.xemacs/'.
   (t "~/.tramp"))
  "File which keeps connection history for Tramp connections."
  :group 'tramp
  :type 'file)

(defun tramp-get-file-property (vec file property default)
  "Get the PROPERTY of FILE from the cache context of VEC.
Returns DEFAULT if not set."
  ;; Unify localname.
  (setq vec (copy-sequence vec))
  (aset vec 3 (tramp-run-real-handler 'directory-file-name (list file)))
  (let* ((hash (or (gethash vec tramp-cache-data)
		   (puthash vec (make-hash-table :test 'equal)
			    tramp-cache-data)))
	 (value (if (hash-table-p hash)
		    (gethash property hash default)
		  default)))
    (tramp-message vec 8 "%s %s %s" file property value)
    value))

(defun tramp-set-file-property (vec file property value)
  "Set the PROPERTY of FILE to VALUE, in the cache context of VEC.
Returns VALUE."
  ;; Unify localname.
  (setq vec (copy-sequence vec))
  (aset vec 3 (tramp-run-real-handler 'directory-file-name (list file)))
  (let ((hash (or (gethash vec tramp-cache-data)
		  (puthash vec (make-hash-table :test 'equal)
			   tramp-cache-data))))
    (puthash property value hash)
    (tramp-message vec 8 "%s %s %s" file property value)
    value))

(defun tramp-flush-file-property (vec file)
  "Remove all properties of FILE in the cache context of VEC."
  ;; Unify localname.
  (setq vec (copy-sequence vec))
  (aset vec 3 (tramp-run-real-handler 'directory-file-name (list file)))
  (tramp-message vec 8 "%s" file)
  (remhash vec tramp-cache-data))

(defun tramp-flush-directory-property (vec directory)
  "Remove all properties of DIRECTORY in the cache context of VEC.
Remove also properties of all files in subdirectories."
  (let ((directory (tramp-run-real-handler
		    'directory-file-name (list directory))))
  (tramp-message vec 8 "%s" directory)
    (maphash
     '(lambda (key value)
	(when (and (stringp key)
		   (string-match directory (tramp-file-name-localname key)))
	  (remhash key tramp-cache-data)))
     tramp-cache-data)))

;; Reverting or killing a buffer should also flush file properties.
;; They could have been changed outside Tramp.  In eshell, "ls" would
;; not show proper directory contents when a file has been copied or
;; deleted before.
(defun tramp-flush-file-function ()
  "Flush all Tramp cache properties from buffer-file-name."
  (let ((bfn (if (stringp (buffer-file-name))
		 (buffer-file-name)
	       default-directory)))
    (when (tramp-tramp-file-p bfn)
      (let* ((v (tramp-dissect-file-name bfn))
	     (localname (tramp-file-name-localname v)))
	(tramp-flush-file-property v localname)))))

(add-hook 'before-revert-hook 'tramp-flush-file-function)
(add-hook 'eshell-pre-command-hook 'tramp-flush-file-function)
(add-hook 'kill-buffer-hook 'tramp-flush-file-function)
(add-hook 'tramp-cache-unload-hook
	  '(lambda ()
	     (remove-hook 'before-revert-hook
			  'tramp-flush-file-function)
	     (remove-hook 'eshell-pre-command-hook
			  'tramp-flush-file-function)
	     (remove-hook 'kill-buffer-hook
			  'tramp-flush-file-function)))

;;; -- Properties --

(defun tramp-get-connection-property (key property default)
  "Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a vector.
If the value is not set for the connection, returns DEFAULT."
  ;; Unify key by removing localname from vector.  Work with a copy in
  ;; order to avoid side effects.
  (when (vectorp key)
    (setq key (copy-sequence key))
    (aset key 3 nil))
  (let* ((hash (gethash key tramp-cache-data))
	 (value (if (hash-table-p hash)
		   (gethash property hash default)
		 default)))
    (tramp-message key 7 "%s %s" property value)
    value))

(defun tramp-set-connection-property (key property value)
  "Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a vector.
PROPERTY is set persistent when KEY is a vector."
  ;; Unify key by removing localname from vector.  Work with a copy in
  ;; order to avoid side effects.
  (when (vectorp key)
    (setq key (copy-sequence key))
    (aset key 3 nil))
  (let ((hash (or (gethash key tramp-cache-data)
		  (puthash key (make-hash-table :test 'equal)
			    tramp-cache-data))))
    (puthash property value hash)
    ;; This function is called also during initialization of
    ;; tramp-cache.el.  `tramp-message� is not defined yet at this
    ;; time, so we ignore the corresponding error.
    (condition-case nil
	(tramp-message key 7 "%s %s" property value)
      (error nil))
    value))

(defun tramp-flush-connection-property (key)
  "Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a vector."
  ;; Unify key by removing localname from vector.  Work with a copy in
  ;; order to avoid side effects.
  (when (vectorp key)
    (setq key (copy-sequence key))
    (aset key 3 nil))
  (remhash key tramp-cache-data))

(defun tramp-cache-print (table)
  "Print hash table TABLE."
  (when (hash-table-p table)
    (let (result)
      (maphash
       '(lambda (key value)
	  (let ((tmp (format
		      "(%s %s)"
		      (if (processp key)
			  (prin1-to-string (prin1-to-string key))
			(prin1-to-string key))
		      (if (hash-table-p value)
			  (tramp-cache-print value)
			(if (bufferp value)
			    (prin1-to-string (prin1-to-string value))
			  (prin1-to-string value))))))
	    (setq result (if result (concat result " " tmp) tmp))))
       table)
      result)))

(defun tramp-list-connections ()
  "Return a list of all known connection vectors according to `tramp-cache'."
    (let (result)
      (maphash
       '(lambda (key value)
	  (when (and (vectorp key) (null (aref key 3)))
	    (add-to-list 'result key)))
       tramp-cache-data)
      result))

(defun tramp-dump-connection-properties ()
  "Write persistent connection properties into file `tramp-persistency-file-name'."
  ;; We shouldn't fail, otherwise (X)Emacs might not be able to be closed.
  (condition-case nil
      (when (and (hash-table-p tramp-cache-data)
		 (not (zerop (hash-table-count tramp-cache-data)))
		 (stringp tramp-persistency-file-name))
	(let ((cache (copy-hash-table tramp-cache-data)))
	  ;; Remove temporary data.
	  (maphash
	   '(lambda (key value)
	      (if (and (vectorp key) (not (tramp-file-name-localname key)))
		  (progn
		    (remhash "process-name" value)
		    (remhash "process-buffer" value))
		(remhash key cache)))
	   cache)
	  ;; Dump it.
	  (with-temp-buffer
	    (insert
	     ";; -*- emacs-lisp -*-"
	     ;; `time-stamp-string' might not exist in all (X)Emacs flavors.
	     (condition-case nil
		 (progn
		   (format
		    " <%s %s>\n"
		    (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
		    tramp-persistency-file-name))
	       (error "\n"))
	     ";; Tramp connection history.  Don't change this file.\n"
	     ";; You can delete it, forcing Tramp to reapply the checks.\n\n"
	     (with-output-to-string
	       (pp (read (format "(%s)" (tramp-cache-print cache))))))
	    (write-region
	     (point-min) (point-max) tramp-persistency-file-name))))
    (error nil)))

(add-hook 'kill-emacs-hook 'tramp-dump-connection-properties)
(add-hook 'tramp-cache-unload-hook
	  '(lambda ()
	     (remove-hook 'kill-emacs-hook
			  'tramp-dump-connection-properties)))

(defun tramp-parse-connection-properties (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection
history."
  (let (res)
    (maphash
     '(lambda (key value)
	(if (and (vectorp key)
		 (string-equal method (tramp-file-name-method key))
		 (not (tramp-file-name-localname key)))
	    (push (list (tramp-file-name-user key)
			(tramp-file-name-host key))
		  res)))
     tramp-cache-data)
    res))

;; Read persistent connection history.
(when (and (stringp tramp-persistency-file-name)
	   (zerop (hash-table-count tramp-cache-data)))
  (condition-case err
      (with-temp-buffer
	(insert-file-contents tramp-persistency-file-name)
	(let ((list (read (current-buffer)))
	      element key item)
	  (while (setq element (pop list))
	    (setq key (pop element))
	    (while (setq item (pop element))
	      (tramp-set-connection-property key (pop item) (car item))))))
    (file-error
     ;; Most likely because the file doesn't exist yet.  No message.
     (clrhash tramp-cache-data))
    (error
     ;; File is corrupted.
     (message "Tramp persistency file '%s' is corrupted: %s"
	      tramp-persistency-file-name (error-message-string err))
     (clrhash tramp-cache-data))))

(provide 'tramp-cache)

;; arch-tag: ee1739b7-7628-408c-9b96-d11a74b05d26
;;; tramp-cache.el ends here
