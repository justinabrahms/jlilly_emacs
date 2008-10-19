;;; js2-build.el -- Build script for js2-mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Keywords:  javascript languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(defvar js2-mode-directory
  (expand-file-name "./"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      command-line-default-directory))
  "Directory where js2-mode is installed. ")

(add-to-list 'load-path js2-mode-directory)

(require 'js2-util)

(defconst js2-build-js2-mode-files
  '("js2-vars"
    "js2-util"
    "js2-scan"
    "js2-messages"
    "js2-ast"
    "js2-highlight"
    "js2-browse"
    "js2-parse"
    "js2-indent"
    "js2-mode")
  "Files used to produce the `js2-mode' final build.
The order of the files in the list is significant.")

(defconst js2-build-directory
  (concat js2-mode-directory "build/")
  "Where `js2-build-js2-mode' build artifacts are deposited.")
  
(defun js2-build-js2-mode ()
  "Concatenates and byte-compiles the js2-mode files.
Rewrites some of the code on the fly."
  (interactive)
  (when (get-buffer "js2.el")
    (save-excursion
      (set-buffer "js2.el")
      (set-buffer-modified-p nil)
      (kill-buffer "js2.el")))
  (save-some-buffers)
  (unless (file-exists-p js2-build-directory)
    (make-directory js2-build-directory))
  (let* ((root default-directory)
         filename
         bufname
         buf
         (outfile (concat js2-build-directory "js2.el"))
         output)
    (setq output (find-file-noselect outfile))
    (save-excursion
      (set-buffer output)
      (setq buffer-undo-list t)
      (erase-buffer)
      (dolist (file (reverse js2-build-js2-mode-files))
        (setq bufname (concat file ".el")
              buf (get-buffer bufname)
              filename (concat root bufname))
        (message "inserting %s" filename)
        (with-buffer output
          (insert-file-contents filename)))
      (js2-build-fixup-output)
      (save-buffer))
    (copy-file outfile (concat js2-build-directory "js2-"
                               (js2-build-version-number)
                               ".el")
               t) ; overwrite
    (if (get-buffer "*Compile-Log*")
        (kill-buffer "*Compile-Log*"))
    (byte-compile-file outfile)
    (pop-to-buffer "js2.el")
    (toggle-read-only 1)
    (pop-to-buffer "*Compile-Log*")
    (message "finished compilation")))

(defun js2-build-version-number ()
  (let* ((s (split-string (current-time-string)))  ; "Mon Mar 24 19:09:09 2008"
         (month (second s))
         (day (third s))
         (year (fifth s)))
    (setq month (cdr (assoc month '(("Jan" . "01")
                                    ("Feb" . "02")
                                    ("Mar" . "03")
                                    ("Apr" . "04")
                                    ("May" . "05")
                                    ("Jun" . "06")
                                    ("Jul" . "07")
                                    ("Aug" . "08")
                                    ("Sep" . "09")
                                    ("Oct" . "10")
                                    ("Nov" . "11")
                                    ("Dec" . "12")))))
    (format "%s%s%02d" year month (string-to-number day))))

(defun js2-build-fixup-output ()
  (let ((version (js2-build-version-number))  ; horks match data
        beg end)
    (delete-trailing-whitespace)
    (goto-char (point-min))
    ;; remove requires of js2 stuff
    (while (re-search-forward "^(require 'js2-.+).*\n" nil t)
      (replace-match ""))

    ;; update version number
    (goto-char (point-min))
    (re-search-forward "(defvar js2-mode-version \\([0-9]+\\)")
    (replace-match version t t nil 1)
    ;; update ELPA version number
    (goto-char (point-min))
    (re-search-forward "^;; Version: \\(2[0-9]+\\)")
    (replace-match version t t nil 1)

    ;; move js2-mode documentation to top
    (goto-char (point-min))
    (re-search-forward "^;;; js2-mode.el")
    (setq beg (match-beginning 0))
    (re-search-forward "^;;; Code:\n")
    (setq end (match-end 0))
    (copy-region-as-kill beg end)
    (delete-region beg end)
    (goto-char (point-min))
    (yank)

    ;; remove remaining gpl file headers
    (goto-char (point-min))
    (re-search-forward "^;+ Code:")  ; skip first header
    (while (re-search-forward "^;; This program is free software;" nil t)
      (setq beg (match-beginning 0))
      (if (re-search-forward "^;; MA 02111-1307 USA\n+" nil t)
          (delete-region beg (match-end 0))))

    ;; update various dev-mode flags
    (dolist (var '("js2-mode-dev-mode-p"
                   "js2-allow-rhino-new-expr-initializer"
                   "js2-allow-member-expr-as-function-name"))
      (goto-char (point-min))
      (re-search-forward (concat "(def\\(?:var\\|custom\\) " var " \\(t\\|nil\\)"))
      (replace-match "nil" t t nil 1))

    ;; add "this file auto-generated" header stuff

    ;; `byte-compile-dynamic' has a bug in Emacs 21
    ;; I'm not sure if `byte-compile-dynamic' is worth it in any case:
    ;;  - it gives users a scary risky-local-variable prompt
    ;;  - our mode pretty much loads everything anyway
    ;; leaving it out for now.
    ;;
    ;; ";;; -*- eval: (set (make-local-variable 'byte-compile-dynamic)"
    ;;   "(> emacs-major-version 21)); -*-\n\n"

    (goto-char (point-min))
    (insert ";;; js2.el -- an improved JavaScript editing mode\n"
            ";;;\n"
            ";;; This file was auto-generated on "
            (current-time-string)
            " from files:\n;;;  "
            (mapconcat (lambda (s)
                         (concat s ".el"))
                       js2-build-js2-mode-files "\n;;;  ")
            "\n\n")

    ;; Close the file per the coding comment guidelines.  Also for ELPA.
    (goto-char (point-max))
    (insert "\n\n;;; js2.el ends here\n")))

(provide 'js2-build)

;;; js2-build.el ends here
