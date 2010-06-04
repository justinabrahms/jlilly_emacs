;;; jira.el --- Connect to JIRA issue tracking software

;; Copyright (C) 2007  Dave Benjamin

;; Author: Dave Benjamin <dave@ramenlabs.com>
;; Version: 0.2.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Modify the variable `jira-url' to indicate the XML-RPC URL for your
;; JIRA installation.

;; Use `M-x jira-login RET' to log in and `M-x jira-logout RET' to log out.
;; Or, just run a command and you'll be prompted for your username and
;; password the first time.

;; The following commands can be used interactively:

;; M-x jira-list-projects
;; M-x jira-list-filters
;; M-x jira-list-issues
;; M-x jira-search-issues
;; M-x jira-search-project-issues
;; M-x jira-show-issue
;; M-x jira-send-region-as-comment

;; All of the XML-RPC API is wrapped, though not all of the API is exposed
;; via interactive functions. For API details, see:

;; http://confluence.atlassian.com/pages/viewpage.action?pageId=1035
;; http://www.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/xmlrpc/XmlRpcService.html

;;; Code:

(require 'cl)
(require 'xml-rpc)

(defvar jira-url nil
  "URL to JIRA XML-RPC server")

(defvar jira-token nil
  "JIRA token used for authentication")

(defun jira-login (username password)
  "Logs the user into JIRA."
  (interactive (list (read-string "Username: ")
                     (read-passwd "Password: ")))
  (setq jira-token (jira-call-noauth 'jira1.login username password)))

(defun jira-logout ()
  "Logs the user out of JIRA"
  (interactive)
  (jira-call 'jira1.logout)
  (setq jira-token nil))

(defun jira-list-projects ()
  "Displays a list of all available JIRA projects"
  (interactive)
  (let ((projects (jira-get-projects)))
    (jira-with-jira-buffer
     (insert (number-to-string (length projects)) " JIRA projects found:\n\n")
     (dolist (project projects)
       (insert (format "%-12s %s\n"
                       (cdr (assoc "key" project))
                       (cdr (assoc "name" project))))))))

(defun jira-list-filters ()
  "Displays a list of all saved JIRA filters"
  (interactive)
  (let ((filters (jira-get-saved-filters)))
    (jira-with-jira-buffer
     (insert (number-to-string (length filters)) " JIRA filters found:\n\n")
     (dolist (filter filters)
       (insert (format "%-8s %s\n"
                       (cdr (assoc "id" filter))
                       (cdr (assoc "name" filter))))))))

(defun jira-list-issues (filter-id)
  "Displays a list of issues matching a filter"
  (interactive
   (list (let ((filter-alist (jira-get-filter-alist)))
           (cdr (assoc (completing-read "Filter: " filter-alist nil t)
                filter-alist)))))
    (when filter-id
      (let ((filter (jira-get-filter filter-id))
            (issues (jira-get-issues-from-filter filter-id)))
        (jira-with-jira-buffer
         (insert "Filter:\n" (cdr (assoc "name" filter))
                 " (" (cdr (assoc "id" filter)) ")\n\n")
         (when (cdr (assoc "description" filter))
           (insert "Description:\n")
           (let ((start (point)))
             (insert (cdr (assoc "description" filter)) "\n\n")
             (fill-region start (point))))
         (jira-display-issues issues)))))

(defun jira-search-issues (text)
  "Displays a list of issues maching a fulltext search"
  (interactive "sSearch: ")
  (let ((issues (jira-get-issues-from-text-search text)))
    (jira-with-jira-buffer
     (insert "Search: " text "\n\n")
     (jira-display-issues issues))))

(defun jira-search-project-issues (project text max-results)
  "Displays a list of issues within a project matching a fulltext search"
  (interactive
   (let ((project-keys
          (mapcar (lambda (project)
                    (cdr (assoc "key" project)))
                  (jira-get-projects))))
     (list
      (completing-read "Project Key: " project-keys nil t)
      (read-string "Search: ")
      (read-number "Max Results: " 20))))
  (let ((issues (jira-get-issues-from-text-search-with-project
                 (list project) (if (equal text "") " " text) max-results)))
    (jira-with-jira-buffer
     (insert "Project Key: " project "\n"
             "Search: " text "\n"
             "Max Results: " (number-to-string max-results) "\n\n")
     (jira-display-issues issues))))

(defun jira-show-issue (issue-key)
  "Displays details about a particular issue."
  (interactive "sIssue Key: ")
  (let ((issue (jira-get-issue issue-key))
        (comments (jira-get-comments issue-key)))
    (jira-with-jira-buffer
     (setq truncate-lines nil)
     (insert "JIRA issue details for " issue-key ":\n\n")
     (dolist (pair issue)
       (unless (equal (car pair) "customFieldValues")
         (insert (format "%16s %s\n"
                         (car pair)
                         (jira-strip-cr (format "%s" (cdr pair)))))))
     (when comments
       (insert "\nComments:\n")
       (dolist (comment comments)
         (insert "\n"
                 (cdr (assoc "author" comment)) " "
                 (cdr (assoc "created" comment)) "\n")
         (insert (jira-strip-cr (cdr (assoc "body" comment))) "\n"))))))

(defun jira-send-region-as-comment (start end issue-key)
  "Send the currently selected region as an issue comment"
  (interactive "r\nsIssue Key: ")
  (jira-add-comment issue-key (buffer-substring start end)))

(defun jira-get-filter (filter-id)
  "Returns a filter given its filter ID."
  (flet ((id-match (filter)
                   (equal filter-id (cdr (assoc "id" filter)))))
    (find-if 'id-match (jira-get-saved-filters))))

(defun jira-get-filter-alist ()
  "Returns an association list mapping filter names to IDs"
  (mapcar (lambda (filter)
            (cons (cdr (assoc "name" filter))
                  (cdr (assoc "id" filter))))
          (jira-get-saved-filters)))

(defun jira-get-status-abbrevs ()
  "Returns an association list of status IDs to abreviated names"
  (flet ((pair (status)
               (cons (cdr (assoc "id" status))
                     (substring (replace-regexp-in-string
                                 " *" "" (cdr (assoc "name" status)))
                                0 3))))
    (mapcar 'pair (jira-get-statuses))))

(defun jira-display-issues (issues)
  "Inserts a list of issues into the current buffer"
  (let ((status-abbrevs (jira-get-status-abbrevs))
        (last-status))
    (insert (number-to-string (length issues))
            " JIRA issues found:\n")
    (dolist (issue issues)
      (let ((status (cdr (assoc "status" issue)))
            (priority (cdr (assoc "priority" issue))))
        (when (not (equal last-status status))
          (setq last-status status)
          (insert "\n"))
        (insert (format "%-16s %-10s %s %5s %s\n"
                        (cdr (assoc "key" issue))
                        (cdr (assoc "assignee" issue))
                        (cdr (assoc status status-abbrevs))
                        (if priority
                            (make-string (- 6 (string-to-number priority))
                                         ?*)
                          "")
                        (cdr (assoc "summary" issue))))))))

(defun jira-add-comment (issue-key comment)
  "Adds a comment to an issue"
  (jira-call 'jira1.addComment issue-key comment))

(defun jira-create-issue (r-issue-struct)
  "Creates an issue in JIRA from a Hashtable object."
  (jira-call 'jira1.createIssue r-issue-struct))

(defun jira-get-comments (issue-key)
  "Returns all comments associated with the issue"
  (jira-call 'jira1.getComments issue-key))

(defun jira-get-components (project-key)
  "Returns all components available in the specified project"
  (jira-call 'jira1.getComponents project-key))

(defun jira-get-issue (issue-key)
  "Gets an issue from a given issue key."
  (jira-call 'jira1.getIssue issue-key))

(defun jira-get-issues-from-filter (filter-id)
  "Executes a saved filter"
  (jira-call 'jira1.getIssuesFromFilter filter-id))

(defun jira-get-issues-from-text-search (search-terms)
  "Find issues using a free text search"
  (jira-call 'jira1.getIssuesFromTextSearch search-terms))

(defun jira-get-issues-from-text-search-with-project
  (project-keys search-terms max-num-results)
  "Find issues using a free text search, limited to certain projects"
  (jira-call 'jira1.getIssuesFromTextSearchWithProject
             project-keys search-terms max-num-results))

(defun jira-get-issue-types ()
  "Returns all visible issue types in the system"
  (jira-call 'jira1.getIssueTypes))

(defun jira-get-priorities ()
  "Returns all priorities in the system"
  (jira-call 'jira1.getPriorities))

(defun jira-get-projects ()
  "Returns a list of projects available to the user"
  (jira-call 'jira1.getProjects))

(defun jira-get-resolutions ()
  "Returns all resolutions in the system"
  (jira-call 'jira1.getResolutions))

(defun jira-get-saved-filters ()
  "Gets all saved filters available for the currently logged in user"
  (jira-call 'jira1.getSavedFilters))

(defun jira-get-server-info ()
  "Returns the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jira-call 'jira1.getServerInfo))

(defun jira-get-statuses ()
  "Returns all statuses in the system"
  (jira-call 'jira1.getStatuses))

(defun jira-get-sub-task-issue-types ()
  "Returns all visible subtask issue types in the system"
  (jira-call 'jira1.getSubTaskIssueTypes))

(defun jira-get-user (username)
  "Returns a user's information given a username"
  (jira-call 'jira1.getUser username))

(defun jira-get-versions (project-key)
  "Returns all versions available in the specified project"
  (jira-call 'jira1.getVersions project-key))

(defun jira-update-issue (issue-key field-values)
  "Updates an issue in JIRA from a Hashtable object."
  (jira-call 'jira1.updateIssue issue-key field-values))

(defun jira-ensure-token ()
  "Makes sure that a JIRA token has been set, logging in if necessary."
  (unless jira-token
    (jira-login (read-string "Username: ")
                (read-passwd "Password: "))))

(defun jira-call (method &rest params)
  "Calls an XML-RPC method on the JIRA server (low-level)"
  (jira-ensure-token)
  (apply 'jira-call-noauth method jira-token params))

(defun jira-call-noauth (method &rest params)
  "Calls an XML-RPC method on the JIRA server without authentication (low-level)"
  (let ((url-version "Exp")             ; hack due to status bug in xml-rpc.el
        (server-url jira-url))
    (apply 'xml-rpc-method-call server-url method params)))

(defun jira-strip-cr (string)
  "Removes carriage returns from a string"
  (when string (replace-regexp-in-string "\r" "" string)))

(defmacro jira-with-jira-buffer (&rest body)
  "Sends all output and buffer modifications to a temporary buffer."
  `(with-output-to-temp-buffer "*jira*"
     (with-current-buffer standard-output
       (setq truncate-lines t)
       ,@body)))

(provide 'jira)

;;; jira.el ends here
