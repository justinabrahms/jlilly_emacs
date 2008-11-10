;;; twitter.el --- Simple Emacs-based client for Twitter

;; Author: Neil Roberts
;; Keywords: twitter

;; Copyright 2008 Neil Roberts
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A Twitter client for emacs that can view your friends timeline and
;; publish new statuses.

;;; Your should add the following to your Emacs configuration file:

;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xt" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; Tell it your username and password by customizing the group
;; "twitter".

;; you can view the statuses by pressing C-x t and you can start
;; editing a message with M-x twitter-status-edit RET. Once the
;; message is finished press C-c C-c to publish.

;;; Code:
(require 'url)
(require 'url-http)
(require 'xml)

(defgroup twitter nil "Twitter status viewer"
  :group 'applications)

(defgroup twitter-faces nil "Faces for displaying Twitter statuses"
  :group 'twitter)

(defface twitter-user-name-face
  '((t (:weight bold :background "light gray")))
  "face for user name headers"
  :group 'twitter-faces)

(defface twitter-time-stamp-face
  '((t (:slant italic :background "light gray")))
  "face for time stamp headers"
  :group 'twitter-faces)

(defface twitter-status-overlong-face
  '((t (:foreground "red")))
  "face used for characters in overly long Twitter statuses.
The face is also used in the mode line if the character count
remaining drops to negative.")

(defconst twitter-friends-timeline-url
  "http://twitter.com/statuses/friends_timeline.xml"
  "URL used to receive the friends timeline")

(defconst twitter-status-update-url
  "http://twitter.com/statuses/update.xml"
  "URL used to update Twitter status")

(defcustom twitter-username nil
  "Username to use for connecting to Twitter.
If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'twitter)

(defcustom twitter-password nil
  "Password to use for connecting to Twitter.
If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'twitter)

(defcustom twitter-maximum-status-length 140
  "Maximum length to allow in a Twitter status update."
  :type 'integer
  :group 'twitter)

(defvar twitter-status-edit-remaining-length ""
  "Characters remaining in a Twitter status update.
This is displayed in the mode line.")

(put 'twitter-status-edit-remaining-length 'risky-local-variable t)

(defvar twitter-status-edit-overlay nil
  "Overlay used to highlight overlong status messages.")

(defvar twitter-status-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'twitter-status-post)
    map)
  "Keymap for `twitter-status-edit-mode'.")

(defun twitter-retrieve-url (url cb)
  "Wrapper around url-retrieve.
Optionally sets the username and password if twitter-username and
twitter-password are set."
  (when (and twitter-username twitter-password)
    (let ((server-cons
	   (or (assoc "twitter.com:80" url-http-real-basic-auth-storage)
	       (car (push (cons "twitter.com:80" nil) url-http-real-basic-auth-storage)))))
      (unless (assoc "Twitter API" server-cons)
	(setcdr server-cons (cons (cons "Twitter API"
					(base64-encode-string (concat twitter-username
								      ":" twitter-password)))
				  (cdr server-cons))))))
  (url-retrieve url cb))

(defun twitter-get-friends-timeline ()
  "Fetch and display the friends timeline.
The results are formatted and displayed in a buffer called
*Twitter friends timeline*"
  (interactive)
  (twitter-retrieve-url twitter-friends-timeline-url
			'twitter-fetched-friends-timeline))

(defun twitter-fetched-friends-timeline (status &rest cbargs)
  "Callback handler for fetching the Twitter friends timeline."
  (let ((result-buffer (current-buffer)) doc)
    ;; Make sure the temporary results buffer is killed even if the
    ;; xml parsing raises an error
    (unwind-protect
	(progn
	  ;; Skip the mime headers
	  (goto-char (point-min))
	  (re-search-forward "\n\n")
	  ;; Parse the rest of the document
	  (setq doc (xml-parse-region (point) (point-max))))
      (kill-buffer result-buffer))
    ;; Get a clean buffer to display the results
    (let ((buf (get-buffer-create "*Twitter friends timeline*")))
      (with-current-buffer buf
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (kill-all-local-variables)
	  ;; If the GET failed then display an error instead
	  (if (plist-get status :error)
	      (twitter-show-error doc)
	    ;; Otherwise process each status node
	    (mapcar 'twitter-format-status-node (xml-get-children (car doc) 'status))))
	(goto-char (point-min)))
      (view-buffer buf))))

(defun twitter-get-node-text (node)
  "Return the text of XML node NODE.
All of the text elements are concatenated together and returned
as a single string."
  (let (text-parts)
    (dolist (part (xml-node-children node))
      (when (stringp part)
	(push part text-parts)))
    (apply 'concat (nreverse text-parts))))

(defun twitter-get-attrib-node (node attrib)
  "Get the text of a child attribute node.
If the children of XML node NODE are formatted like
<attrib1>data</attrib1> <attrib2>data</attrib2> ... then this
fuction will return the text of the child node named ATTRIB or
nil if it isn't found."
  (let ((child (xml-get-children node attrib)))
    (if (consp child)
	(twitter-get-node-text (car child))
      nil)))

(defun twitter-show-error (doc)
  "Show a Twitter error message.
DOC should be the XML parsed document returned in the error
message. If any information about the error can be retrieved it
will also be displayed."
  (insert "An error occured while trying to process a Twitter request.\n\n")
  (let (error-node)
    (if (and (consp doc)
	     (consp (car doc))
	     (eq 'hash (caar doc))
	     (setq error-node (xml-get-children (car doc) 'error)))
	(insert (twitter-get-node-text (car error-node)))
      (xml-print doc))))	

(defun twitter-format-status-node (status-node)
  "Insert the contents of a Twitter status node.
The status is formatted with text properties and insterted into
the current buffer."
  (let ((user-node (xml-get-children status-node 'user)) val)
    (when user-node
      (setq user-node (car user-node))
      (when (setq val (twitter-get-attrib-node user-node 'name))
	(insert (propertize val 'face 'twitter-user-name-face))))
    (when (setq val (twitter-get-attrib-node status-node 'created_at))
      (when (< (+ (current-column) (length val)) fill-column)
	(setq val (concat (make-string (- fill-column
					  (+ (current-column) (length val))) ? )
			  val)))
      (insert (propertize val 'face 'twitter-time-stamp-face)))
    (insert "\n")
    (when (setq val (twitter-get-attrib-node status-node 'text))
      (fill-region (prog1 (point) (insert val)) (point)))
    (insert "\n\n")))

(defun twitter-status-post ()
  "Update your Twitter status.
The contents of the current buffer are used for the status. The
current buffer is then killed. If there is too much text in the
buffer then you will be asked for confirmation."
  (interactive)
  (when (or (<= (buffer-size) twitter-maximum-status-length)
	    (y-or-n-p (format (concat "The message is %i characters long. "
				      "Are you sure? ") (buffer-size))))
    (message "Sending status...")
    (let ((url-request-method "POST")
	  (url-request-data (concat "status="
				    (url-hexify-string (buffer-substring
							(point-min) (point-max))))))
      (twitter-retrieve-url twitter-status-update-url 'twitter-status-callback))))

(defun twitter-status-callback (status)
  "Function called after Twitter status has been sent."
  (let ((errmsg (plist-get status :error)))
    (when errmsg
      (signal (car errmsg) (cdr errmsg)))
    (kill-buffer "*Twitter Status*")
    (message "Succesfully updated Twitter status.")))

(defun twitter-status-edit ()
  "Edit your twitter status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[twitter-status-post] when you are finished editing to send the
message."
  (interactive)
  (pop-to-buffer "*Twitter Status*")
  (twitter-status-edit-mode))

(defun twitter-status-edit-update-length ()
  "Updates the character count in Twitter status buffers.
This should be run after the text in the buffer is changed. Any
characters after the maximum status update length are
hightlighted in the face twitter-status-overlong-face and the
character count on the mode line is updated."
  ;; Update the remaining characters in the mode line
  (let ((remaining (- twitter-maximum-status-length
		      (buffer-size))))
    (setq twitter-status-edit-remaining-length
	  (concat " "
		  (if (>= remaining 0)
		      (number-to-string remaining)
		    (propertize (number-to-string remaining)
				'face 'twitter-status-overlong-face))
		  " ")))
  (force-mode-line-update)
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) twitter-maximum-status-length)
      (let ((start (+ (point-min) twitter-maximum-status-length)))
	(if (null twitter-status-edit-overlay)
	    (overlay-put (setq twitter-status-edit-overlay
			       (make-overlay start (point-max)))
			 'face 'twitter-status-overlong-face)
	  (move-overlay twitter-status-edit-overlay
			start (point-max))))
    ;; Buffer is not too long so just hide the overlay
    (when twitter-status-edit-overlay
      (delete-overlay twitter-status-edit-overlay))))

(defun twitter-status-edit-after-change (begin end old-size)
  (twitter-status-edit-update-length))

(define-derived-mode twitter-status-edit-mode text-mode "Twitter Status Edit"
  "Major mode for updating your Twitter status."
  ;; Schedule to update the character count after altering the buffer
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'twitter-status-edit-after-change)
  ;; Add the remaining character count to the mode line
  (make-local-variable 'twitter-status-edit-remaining-length)
  ;; Copy the mode line format list so we can safely edit it without
  ;; affecting other buffers
  (setq mode-line-format (copy-sequence mode-line-format))
  ;; Add the remaining characters variable after the mode display
  (let ((n mode-line-format))
    (catch 'found
      (while n
	(when (eq 'mode-line-modes (car n))
	  (setcdr n (cons 'twitter-status-edit-remaining-length
			  (cdr n)))
	  (throw 'found nil))
	(setq n (cdr n)))))
  ;; Make a buffer-local reference to the overlay for overlong
  ;; messages
  (make-local-variable 'twitter-status-edit-overlay)
  ;; Update the mode line immediatly
  (twitter-status-edit-update-length))

(provide 'twitter)

;;; twitter.el ends here
