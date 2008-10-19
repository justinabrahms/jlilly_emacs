;;; js2-mode.el --- an improved JavaScript editing mode

;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Version: 20080403
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

;;; Commentary:

;; This JavaScript editing mode supports:
;;
;;  - the full JavaScript language through version 1.7
;;  - support for most Rhino and SpiderMonkey extensions from 1.5 to 1.7
;;  - accurate syntax highlighting using a recursive-descent parser
;;  - syntax-error and strict-mode warning reporting
;;  - "bouncing" line indentation to choose among alternate indentation points
;;  - smart line-wrapping within comments (Emacs 22+) and strings
;;  - code folding:
;;    - show some or all function bodies as {...}
;;    - show some or all block comments as /*...*/
;;  - context-sensitive menu bar and popup menus
;;  - code browsing using the `imenu' package
;;  - typing helpers (e.g. inserting matching braces/parens)
;;  - many customization options
;;
;; It is only compatible with GNU Emacs versions 21 and higher (not XEmacs).
;;
;; Installation:
;;
;;  - put `js2.el' somewhere in your emacs load path
;;  - M-x byte-compile-file RET <path-to-js2.el> RET
;;    Note:  it will refuse to run unless byte-compiled
;;  - add these lines to your .emacs file:
;;    (autoload 'js2-mode "js2" nil t)
;;    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;
;; To customize how it works:
;;   M-x customize-group RET js2-mode RET
;;
;; The variable `js2-mode-version' is a date stamp.  When you upgrade
;; to a newer version, you must byte-compile the file again.
;;
;; Notes:
;;
;; This mode is different in many ways from standard Emacs language editing
;; modes, inasmuch as it attempts to be more like an IDE.  If this drives
;; you crazy, it IS possible to customize it to be more like other Emacs
;; editing modes.  Please customize the group `js2-mode' to see all of the
;; configuration options.
;;
;; Some of the functionality does not work in Emacs 21 -- upgrading to
;; Emacs 22 or higher will get you better results.  If you byte-compiled
;; js2.el with Emacs 21, you should re-compile it for Emacs 22.
;;
;; Unlike cc-engine based language modes, js2-mode's line-indentation is not
;; customizable.  It is a surprising amount of work to support customizable
;; indentation.  The current compromise is that the tab key lets you cycle among
;; various likely indentation points, similar to the behavior of python-mode.
;;
;; This mode does not yet work with "multi-mode" modes such as mmm-mode
;; and mumamo, although it could possibly be made to do so with some effort.
;; This means that js2-mode is currently only useful for editing JavaScript
;; files, and not for editing JavaScript within <script> tags or templates.
;;
;; This code is part of a larger project, in progress, to enable writing
;; Emacs customizations in JavaScript.
;;
;; Please email bug reports and suggestions to the author, or submit them
;; at http://code.google.com/p/js2-mode/issues

;; TODO:
;;  - add unreachable-code warning (error?) using the inconsistent-return analysis
;;  - labeled stmt length is now 1
;;  - "anonymous function does not always return a value" - use getter/setter name
;;  - extend js2-missing-semi-one-line-override to handle catch (e) {return x}
;;  - set a text prop on autoinserted delimiters and don't biff user-entered ones
;;  - when inserting magic curlies, look for matching close-curly before inserting
;;  - get more use out of the symbol table:
;;    - jump to declaration (put hyperlinks on all non-decl var usages?)
;;    - rename variable/function
;;    - warn on unused var
;;  - add some dabbrev-expansions for built-in keywords like finally, function
;;  - add at least some completion support, e.g. for built-ins
;;  - code formatting

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'imenu)
(require 'cc-cmds)  ; for `c-fill-paragraph'

(require 'js2-vars)
(require 'js2-parse)
(require 'js2-messages)
(require 'js2-indent)

;;;###autoload (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;###autoload
(defun js2-mode ()
  "Major mode for editing JavaScript code."
  (interactive)
  (js2-mode-check-compat)
  (kill-all-local-variables)
  (set-syntax-table js2-mode-syntax-table)
  (use-local-map js2-mode-map)
  (setq major-mode 'js2-mode
        mode-name "JavaScript-IDE"
        comment-start "//"  ; used by comment-region; don't change it
        comment-end "")
  (setq local-abbrev-table js2-mode-abbrev-table)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js2-indent-line)
  (set (make-local-variable 'indent-region-function) #'js2-indent-region)
  (set (make-local-variable 'fill-paragraph-function) #'js2-fill-paragraph)
  (set (make-local-variable 'before-save-hook) #'js2-before-save)
  (set (make-local-variable 'next-error-function) #'js2-next-error)
  (set (make-local-variable 'beginning-of-defun-function) #'js2-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'js2-end-of-defun)
  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'js2-mode 'find-tag-default-function #'js2-mode-find-tag)

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-buffer-is-cc-mode t
        c-comment-prefix-regexp js2-comment-prefix-regexp
        c-paragraph-start js2-paragraph-start
        c-paragraph-separate "$"
        comment-start-skip js2-comment-start-skip
        c-syntactic-ws-start js2-syntactic-ws-start
        c-syntactic-ws-end js2-syntactic-ws-end
        c-syntactic-eol js2-syntactic-eol)
  (if js2-emacs22
      (c-setup-paragraph-variables))

  ;; We do our own syntax highlighting based on the parse tree.
  ;; However, we want minor modes that add keywords to highlight properly
  ;; (examples:  doxymacs, column-marker).  We do this by not letting
  ;; font-lock unfontify anything, and telling it to fontify after we
  ;; re-parse and re-highlight the buffer.  (We currently don't do any
  ;; work with regions other than the whole buffer.)
  (dolist (var '(font-lock-unfontify-buffer-function
                 font-lock-unfontify-region-function))
    (set (make-local-variable var) (lambda (&rest args) t)))

  ;; Don't let font-lock do syntactic (string/comment) fontification.
  (set (make-local-variable #'font-lock-syntactic-face-function)
       (lambda (state) nil))

  ;; Experiment:  make reparse-delay longer for longer files.
  (if (plusp js2-dynamic-idle-timer-adjust)
      (setq js2-idle-timer-delay
            (* js2-idle-timer-delay
               (/ (point-max) js2-dynamic-idle-timer-adjust))))

  (add-hook 'change-major-mode-hook #'js2-mode-exit nil t)
  (add-hook 'after-change-functions #'js2-mode-edit nil t)
  (setq imenu-create-index-function #'js2-mode-create-imenu-index)
  (imenu-add-to-menubar (concat "IM-" mode-name))
  (when js2-mirror-mode
    (js2-enter-mirror-mode))
  (add-to-invisibility-spec '(js2-outline . t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'forward-sexp-function) #'js2-mode-forward-sexp)
  (setq js2-mode-functions-hidden nil
        js2-mode-comments-hidden nil
        js2-mode-buffer-dirty-p t
        js2-mode-parsing nil)
  (js2-reparse)
  (run-hooks 'js2-mode-hook))

(defun js2-mode-check-compat ()
  "Signal an error if we can't run with this version of Emacs."
  (if (and js2-mode-must-byte-compile
           (not (byte-code-function-p (symbol-function 'js2-mode))))
      (error "You must byte-compile js2-mode before using it."))
  (if (and (boundp 'running-xemacs)
           running-xemacs)
      (error "js2-mode is not compatible with XEmacs"))
  (unless (>= emacs-major-version 21)
    (error "js2-mode requires GNU Emacs version 21 or higher")))

(defun js2-mode-exit ()
  (interactive)
  (when js2-mode-node-overlay
    (delete-overlay js2-mode-node-overlay)
    (setq js2-mode-node-overlay nil))
  (js2-remove-overlays)
  (setq js2-mode-ast nil)
  (remove-hook 'change-major-mode-hook #'js2-mode-exit t)
  (remove-from-invisibility-spec '(js2-outline . t))
  (js2-mode-show-all)
  (js2-with-unmodifying-text-property-changes
    (js2-clear-face (point-min) (point-max))))

(defun js2-before-save ()
  "Clean up whitespace before saving file.
You can disable this by customizing `js2-cleanup-whitespace'."
  (when js2-cleanup-whitespace
    (let ((col (current-column)))
      (delete-trailing-whitespace)
      ;; don't change trailing whitespace on current line
      (unless (eq (current-column) col)
        (indent-to col)))))
      
(defsubst js2-mode-reset-timer ()
  (if js2-mode-parse-timer
      (cancel-timer js2-mode-parse-timer))
  (setq js2-mode-parsing nil)
  (setq js2-mode-parse-timer
        (run-with-idle-timer js2-idle-timer-delay nil #'js2-reparse)))

(defun js2-mode-edit (beg end len)
  "Schedule a new parse after buffer is edited."
  (setq js2-mode-buffer-dirty-p t)
  (js2-mode-hide-overlay)
  (js2-mode-reset-timer))

(defun js2-mode-run-font-lock ()
  "Run `font-lock-fontify-buffer' after parsing/highlighting.
This is intended to allow modes that install their own font-lock keywords
to work with js2-mode.  In practice it never seems to work for long.
Hopefully the Emacs maintainers can help figure out a way to make it work."
  (when (and (boundp 'font-lock-keywords)
             font-lock-keywords
             (boundp 'font-lock-mode)
             font-lock-mode)
    ;; TODO:  font-lock and jit-lock really really REALLY don't want to
    ;; play nicely with js2-mode.  They go out of their way to fail to
    ;; provide any option for saying "look, fontify the goddamn buffer
    ;; with just the keywords already".  Argh.
    (setq font-lock-defaults (list font-lock-keywords 'keywords-only))
    (let (font-lock-verbose)
      (font-lock-default-fontify-buffer))))

(defun js2-reparse (&optional force)
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it.  If FORCE is nil, then the
buffer will only rebuild its `js2-mode-ast' if the buffer is dirty."
  (let (time
        interrupted-p
        (js2-compiler-strict-mode js2-mode-show-strict-warnings))
    (unless js2-mode-parsing
      (setq js2-mode-parsing t)
      (unwind-protect
          (when (or js2-mode-buffer-dirty-p force)
            (js2-remove-overlays)
            (js2-with-unmodifying-text-property-changes
              (setq js2-mode-buffer-dirty-p nil
                    js2-mode-fontifications nil
                    js2-mode-deferred-properties nil)
              (if js2-mode-verbose-parse-p
                  (message "parsing..."))
              (setq time
                    (js2-time
                     (setq interrupted-p
                           (catch 'interrupted
                             (setq js2-mode-ast (js2-parse))
                             (js2-mode-fontify-regions)
                             (js2-mode-remove-suppressed-warnings)
                             (js2-mode-show-warnings)
                             (js2-mode-show-errors)
                             (js2-mode-run-font-lock)  ; note:  doesn't work
                             (if (>= js2-highlight-level 1)
                                 (js2-highlight-jsdoc js2-mode-ast))
                             nil))))
              (if interrupted-p
                  (progn
                    ;; unfinished parse => try again
                    (setq js2-mode-buffer-dirty-p t)
                    (js2-mode-reset-timer))
                (if js2-mode-verbose-parse-p
                    (message "Parse time: %s" time)))))
        ;; finally
        (setq js2-mode-parsing nil)
        (unless interrupted-p
          (setq js2-mode-parse-timer nil))))))

(defun js2-mode-show-node ()
  "Debugging aid:  highlight selected AST node on mouse click."
  (interactive)
  (let ((node (js2-node-at-point))
        beg
        end)
    (when js2-mode-show-overlay
      (if (null node)
          (message "No node found at location %s" (point))
        (setq beg (js2-node-abs-pos node)
              end (+ beg (js2-node-len node)))
        (if js2-mode-node-overlay
            (move-overlay js2-mode-node-overlay beg end)
          (setq js2-mode-node-overlay (make-overlay beg end))
          (overlay-put js2-mode-node-overlay 'face 'highlight))
        (js2-with-unmodifying-text-property-changes
          (put-text-property beg end 'point-left #'js2-mode-hide-overlay))
        (message "%s, parent: %s"
                 (js2-node-short-name node)
                 (if (js2-node-parent node)
                     (js2-node-short-name (js2-node-parent node))
                   "nil"))))))

(defun js2-mode-hide-overlay (&optional p1 p2)
  "Remove the debugging overlay when the point moves."
  (when js2-mode-node-overlay
    (let ((beg (overlay-start js2-mode-node-overlay))
          (end (overlay-end js2-mode-node-overlay)))
      ;; Sometimes we're called spuriously.
      (unless (and p2
                   (>= p2 beg)
                   (<= p2 end))
        (js2-with-unmodifying-text-property-changes
          (remove-text-properties beg end '(point-left nil)))
        (delete-overlay js2-mode-node-overlay)
        (setq js2-mode-node-overlay nil)))))

(defun js2-mode-reset ()
  "Debugging helper; resets everything."
  (interactive)
  (js2-mode-exit)
  (js2-mode))

(defsubst js2-mode-show-warn-or-err (e face)
  "Highlight a warning or error E with FACE.
E is a list of ((MSG-KEY MSG-ARG) BEG END)."
  (let* ((key (first e))
         (beg (second e))
         (end (+ beg (third e)))
         ;; Don't inadvertently go out of bounds.
         (beg (max (point-min) (min beg (point-max))))
         (end (max (point-min) (min end (point-max))))
         (js2-highlight-level 3)    ; so js2-set-face is sure to fire
         (ovl (make-overlay beg end)))
    (overlay-put ovl 'face face)
    (overlay-put ovl 'js2 t)
    (put-text-property beg end 'help-echo (js2-get-msg key))
    (put-text-property beg end 'point-entered #'js2-echo-error)))

(defun js2-remove-overlays ()
  "Remove overlays from buffer that have a `js2' property."
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (overlay-get o 'js2)
          (delete-overlay o))))))

(defun js2-mode-fontify-regions ()
  "Apply fontifications recorded during parsing."
  ;; We defer clearing faces as long as possible to eliminate flashing.
  (js2-clear-face (point-min) (point-max))
  ;; have to reverse the recorded fontifications so that errors and
  ;; warnings overwrite the normal fontifications
  (dolist (f (nreverse js2-mode-fontifications))
    (put-text-property (first f) (second f) 'face (third f)))
  (setq js2-mode-fontifications nil)
  (dolist (p js2-mode-deferred-properties)
    (apply #'put-text-property p))
  (setq js2-mode-deferred-properties nil))

(defun js2-mode-show-errors ()
  "Highlight syntax errors."
  (when js2-mode-show-parse-errors
    (dolist (e (js2-ast-root-errors js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-error-face))))

(defun js2-mode-remove-suppressed-warnings ()
  "Take suppressed warnings out of the AST warnings list.
This ensures that the counts and `next-error' are correct."
  (setf (js2-ast-root-warnings js2-mode-ast)
        (js2-delete-if
         (lambda (e)
           (let ((key (caar e)))
             (or
              (and (not js2-strict-trailing-comma-warning)
                   (string-match "trailing\\.comma" key))
              (and (not js2-strict-cond-assign-warning)
                   (string= key "msg.equal.as.assign"))
              (and js2-missing-semi-one-line-override
                   (string= key "msg.missing.semi")
                   (let* ((beg (second e))
                          (node (js2-node-at-point beg))
                          (fn (js2-mode-find-parent-fn node))
                          (body (and fn (js2-function-node-body fn)))
                          (lc (and body (js2-node-abs-pos body)))
                          (rc (and lc (+ lc (js2-node-len body)))))
                     (and fn
                          (or (null body)
                              (save-excursion
                                (goto-char beg)
                                (and (js2-same-line lc)
                                     (js2-same-line rc))))))))))
         (js2-ast-root-warnings js2-mode-ast))))

(defun js2-mode-show-warnings ()
  "Highlight strict-mode warnings."
  (when js2-mode-show-strict-warnings
    (dolist (e (js2-ast-root-warnings js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-warning-face))))

(defun js2-echo-error (old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (if msg
        (message msg))))

(defalias #'js2-echo-help #'js2-echo-error)

(defun js2-enter-key ()
  "Handle user pressing the Enter key."
  (interactive)
  (let ((parse-status (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
    (cond
     ;; check if we're inside a string
     ((nth 3 parse-status)
      (js2-mode-split-string parse-status))
     ;; check if inside a block comment
     ((nth 4 parse-status)
      (js2-mode-extend-comment))
     (t
      ;; should probably figure out what the mode-map says we should do
      (if js2-indent-on-enter-key
          (let ((js2-bounce-indent-flag nil))
            (js2-indent-line)))
      (insert "\n")
      (if js2-enter-indents-newline
          (let ((js2-bounce-indent-flag nil))
            (js2-indent-line)))))))

(defun js2-mode-split-string (parse-status)
  "Turn a newline in mid-string into a string concatenation."
  (let* ((col (current-column))
         (quote-char (nth 3 parse-status))
         (quote-string (string quote-char))
         (string-beg (nth 8 parse-status))
         (indent (save-match-data
                   (or
                    (save-excursion
                      (back-to-indentation)
                      (if (looking-at "\\+")
                          (current-column)))
                    (save-excursion
                      (goto-char string-beg)
                      (if (looking-back "\\+\\s-+")
                          (goto-char (match-beginning 0)))
                      (current-column))))))
    (insert quote-char "\n")
    (indent-to indent)
    (insert "+ " quote-string)
    (when (eolp)
      (insert quote-string)
      (backward-char 1))))

(defun js2-mode-extend-comment ()
  "When inside a comment block, add comment prefix."
  (let (star single col first-line needs-close)
    (save-excursion
      (back-to-indentation)
      (cond
       ((looking-at "\\*[^/]")
        (setq star t
              col (current-column)))
       ((looking-at "/\\*")
        (setq star t
              first-line t
              col (1+ (current-column))))
       ((looking-at "//")
        (setq single t
              col (current-column)))))
    ;; Heuristic for whether we need to close the comment:
    ;; if we've got a parse error here, assume it's an unterminated
    ;; comment.
    (setq needs-close
          (or
           (eq (get-text-property (1- (point)) 'point-entered)
               'js2-echo-error)
           ;; The heuristic above doesn't work well when we're
           ;; creating a comment and there's another one downstream,
           ;; as our parser thinks this one ends at the end of the
           ;; next one.  (You can have a /* inside a js block comment.)
           ;; So just close it if the next non-ws char isn't a *.
           (and first-line
                (eolp)
                (save-excursion
                  (skip-syntax-forward " ")
                  (not (eq (char-after) ?*))))))
    (insert "\n")
    (cond
     (star
      (indent-to col)
      (insert "* ")
      (if (and first-line needs-close)
          (save-excursion
            (insert "\n")
            (indent-to col)
            (insert "*/"))))
     (single
      (when (save-excursion
              (and (zerop (forward-line 1))
                   (looking-at "\\s-*//")))
        (indent-to col)
        (insert "// "))))))

(defun js2-fill-string (beg quote)
  "Line-wrap a single-line string into a multi-line string.
BEG is the string beginning, QUOTE is the quote char."
  (let* ((squote (string quote))
         (end (if (re-search-forward (concat "[^\\]" squote)
                                     (point-at-eol) t)
                  (1+ (match-beginning 0))
                (point-at-eol)))
         (tag (make-marker))
         (fill-column (- fill-column 4)))  ; make room
    (unwind-protect
        (progn
          (move-marker tag end)
          (fill-paragraph nil)
          (goto-char beg)
          (while (not (js2-same-line tag))
            (goto-char (point-at-eol))
            (insert squote)
            (when (zerop (forward-line 1))
              (back-to-indentation)
              (if (looking-at (concat squote "\\s-*$"))
                  (progn
                    (setq end (point-at-eol))
                    (forward-line -1)
                    (delete-region (point-at-eol) end))
                (insert "+ " squote)))))
      (move-marker tag nil))))

(defun js2-fill-paragraph (arg)
  "Fill paragraph after point.  Prefix ARG means justify as well.
Has special handling for filling in comments and strings."
  (let* ((parse-status (save-excursion
                         (parse-partial-sexp (point-min) (point))))
         (quote-char (or (nth 3 parse-status)
                         (save-match-data
                           (if (looking-at "[\"\']")
                               (char-after))))))
    (cond
     (quote-char
      (js2-fill-string (or (nth 8 parse-status)
                           (point))
                       quote-char)
      t) ; or fill-paragraph does evil things afterwards
     ((nth 4 parse-status)  ; in block comment?
      (js2-fill-comment parse-status arg))
     (t
      (fill-paragraph arg)))))

(defun js2-fill-comment (parse-status arg)
  "Fill-paragraph in a block comment."
  (let* ((beg (nth 8 parse-status))
         (end (save-excursion
                (goto-char beg)
                (re-search-forward "[^\\]\\*/" nil t)))
         indent
         end-marker)
    (when end
      (setq end-marker (make-marker))
      (move-marker end-marker end))
    (when (and end js2-mode-squeeze-spaces)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[ \t][ \t]+" nil t)
            (replace-match " ")))))
    ;; `c-fill-paragraph' doesn't indent the continuation stars properly
    ;; if the comment isn't left-justified.  They align to the first star
    ;; on the first continuation line after the comment-open, so we make
    ;; sure the first continuation line has the proper indentation.
    (save-excursion
      (goto-char beg)
      (setq indent (1+ (current-column)))
      (goto-char (point-at-eol))
      (skip-chars-forward " \t\r\n")
      (indent-line-to indent)

      ;; Invoke `c-fill-paragraph' from the first continuation line,
      ;; since it provides better results.  Otherwise if you're on the
      ;; last line, it doesn't prefix with stars the way you'd expect.
      ;; TODO:  write our own fill function that works in Emacs 21
      (c-fill-paragraph arg))

    ;; last line is typically indented wrong, so fix it
    (when end-marker
      (save-excursion
        (goto-char end-marker)
        (js2-indent-line)))))

(defun js2-beginning-of-line ()
  "Toggles point between bol and first non-whitespace char in line.
Also moves past comment delimiters when inside comments."
  (interactive)
  (let (node beg)
    (cond
     ((bolp)
      (back-to-indentation))
     ((looking-at "//")
      (skip-chars-forward "/ \t"))
     ((and (eq (char-after) ?*)
           (setq node (js2-comment-at-point))
           (memq (js2-comment-node-format node) '(jsdoc block))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      (skip-chars-forward "\* \t"))
     (t
      (goto-char (point-at-bol))))))

(defun js2-end-of-line ()
  "Toggles point between eol and last non-whitespace char in line."
  (interactive)
  (if (eolp)
      (skip-chars-backward " \t")
    (goto-char (point-at-eol))))

(defun js2-enter-mirror-mode()
  "Turns on mirror mode, where quotes, brackets etc are mirrored automatically
  on insertion."
  (interactive)
  (define-key js2-mode-map (read-kbd-macro "{")  'js2-mode-match-curly)
  (define-key js2-mode-map (read-kbd-macro "}")  'js2-mode-magic-close-paren)
  (define-key js2-mode-map (read-kbd-macro "\"") 'js2-mode-match-double-quote)
  (define-key js2-mode-map (read-kbd-macro "'")  'js2-mode-match-single-quote)
  (define-key js2-mode-map (read-kbd-macro "(")  'js2-mode-match-paren)
  (define-key js2-mode-map (read-kbd-macro ")")  'js2-mode-magic-close-paren)
  (define-key js2-mode-map (read-kbd-macro "[")  'js2-mode-match-bracket)
  (define-key js2-mode-map (read-kbd-macro "]")  'js2-mode-magic-close-paren))

(defun js2-leave-mirror-mode()
  "Turns off mirror mode."
  (interactive)
  (dolist (key '("{" "\"" "'" "(" ")" "[" "]"))
    (define-key js2-mode-map (read-kbd-macro key) 'self-insert-command)))

(defsubst js2-mode-inside-string ()
  "Return non-nil if inside a string.
Actually returns the quote character that begins the string."
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
      (nth 3 parse-state)))

(defsubst js2-mode-inside-comment-or-string ()
  "Return non-nil if inside a comment or string."
  (or
   (let ((comment-start
          (save-excursion
            (goto-char (point-at-bol))
            (if (re-search-forward "//" (point-at-eol) t)
                (match-beginning 0)))))
     (and comment-start
          (<= comment-start (point))))
   (let ((parse-state (save-excursion
                        (parse-partial-sexp (point-min) (point)))))
     (or (nth 3 parse-state)
         (nth 4 parse-state)))))

(defun js2-mode-match-curly (arg)
  "Insert matching curly-brace."
  (interactive "p")
  (insert "{")
  (if current-prefix-arg
      (save-excursion
        (insert "}"))
    (unless (or (not (looking-at "\\s-*$"))
                (js2-mode-inside-comment-or-string))
      (undo-boundary)

      ;; absolutely mystifying bug:  when inserting the next "\n",
      ;; the buffer-undo-list is given two new entries:  the inserted range,
      ;; and the incorrect position of the point.  It's recorded incorrectly
      ;; as being before the opening "{", not after it.  But it's recorded
      ;; as the correct value if you're debugging `js2-mode-match-curly'
      ;; in edebug.  I have no idea why it's doing this, but incrementing
      ;; the inserted position fixes the problem, so that the undo takes us
      ;; back to just after the user-inserted "{".
      (insert "\n")
      (ignore-errors
        (incf (cadr buffer-undo-list)))

      (js2-indent-line)
      (save-excursion
        (insert "\n}")
        (let ((js2-bounce-indent-flag (js2-code-at-bol-p)))
          (js2-indent-line))))))
    
(defun js2-mode-match-bracket ()
  "Insert matching bracket."
  (interactive)
  (insert "[")
  (unless (or (not (looking-at "\\s-*$"))
              (js2-mode-inside-comment-or-string))
    (save-excursion
      (insert "]"))
    (when js2-auto-indent-flag
      (let ((js2-bounce-indent-flag (js2-code-at-bol-p)))
        (js2-indent-line)))))

(defun js2-mode-match-paren ()
  "Insert matching paren unless already inserted."
  (interactive)
  (insert "(")
  (unless (or (not (looking-at "\\s-*$"))
              (js2-mode-inside-comment-or-string))
    (save-excursion
      (insert ")"))
    (when js2-auto-indent-flag
      (let ((js2-bounce-indent-flag (js2-code-at-bol-p)))
        (js2-indent-line)))))

(defsubst js2-match-quote (quote-string)
  (let ((start-quote (js2-mode-inside-string)))
    (cond
     ;; inside a comment - don't do quote-matching, since we can't
     ;; reliably figure out if we're in a string inside the comment
     ((js2-comment-at-point)
      (insert quote-string))
     ((not start-quote)
      ;; not in string => insert matched quotes
      (insert quote-string)
      ;; exception:  if we're just before a word, don't double it.
      (unless (looking-at "[^ \t\r\n]")
        (save-excursion
          (insert quote-string))))
     ((looking-at quote-string)
      (if (looking-back "[^\\]\\\\")
          (insert quote-string)
        (forward-char 1)))
     ((and js2-mode-escape-quotes
           (save-excursion
             (save-match-data
               (re-search-forward quote-string (point-at-eol) t))))
      ;; inside terminated string, escape quote (unless already escaped)
      (insert (if (looking-back "[^\\]\\\\")
                  quote-string
                (concat "\\" quote-string))))
     (t
      (insert quote-string)))))        ; else terminate the string
      
(defun js2-mode-match-single-quote ()
  "Insert matching single-quote."
  (interactive)
  (let ((parse-status (parse-partial-sexp (point-min) (point))))
    ;; don't match inside comments, since apostrophe is more common
    (if (nth 4 parse-status)
        (insert "'")
      (js2-match-quote "'"))))

(defun js2-mode-match-double-quote ()
  "Insert matching double-quote."
  (interactive)
  (js2-match-quote "\""))

(defun js2-mode-magic-close-paren ()
  "Skip over close-paren rather than inserting, where appropriate.
Uses some heuristics to try to figure out the right thing to do."
  (interactive)
  (let* ((parse-status (parse-partial-sexp (point-min) (point)))
         (open-pos (nth 1 parse-status))
         (close last-input-char)
         (open (cond
                ((eq close 41)  ; close-paren
                 40)            ; open-paren
                ((eq close 93)  ; close-bracket
                 91)            ; open-bracket
                ((eq close ?})
                 ?{)
                (t nil))))
    (if (and (looking-at (string close))
             (eq open (char-after open-pos))
             (js2-same-line open-pos))
        (forward-char 1)
      (insert (string close)))
    (blink-matching-open)))

(defun js2-mode-wait-for-parse (callback)
  "Invoke CALLBACK when parsing is finished.
If parsing is already finished, calls CALLBACK immediately."
  (if (not js2-mode-buffer-dirty-p)
      (funcall callback)
    (push callback js2-mode-pending-parse-callbacks)
    (add-hook 'js2-parse-finished-hook #'js2-mode-parse-finished)))

(defun js2-mode-parse-finished ()
  "Invoke callbacks in `js2-mode-pending-parse-callbacks'."
  ;; We can't let errors propagate up, since it prevents the
  ;; `js2-parse' method from completing normally and returning
  ;; the ast, which makes things mysteriously not work right.
  (unwind-protect
      (dolist (cb js2-mode-pending-parse-callbacks)
        (condition-case err
            (funcall cb)
          (error (message "%s" err))))
    (setq js2-mode-pending-parse-callbacks nil)))

(defun js2-mode-flag-region (from to flag)
  "Hide or show text from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden.
Returns the created overlay if FLAG is non-nil."
  (remove-overlays from to 'invisible 'js2-outline)
  (when flag
    (let ((o (make-overlay from to)))
      (overlay-put o 'invisible 'js2-outline)
      (overlay-put o 'isearch-open-invisible
                   'js2-isearch-open-invisible)
      o)))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `js2-mode-flag-region').
(defun js2-isearch-open-invisible (overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (js2-mode-show-element))

(defun js2-mode-invisible-overlay-bounds (&optional pos)
  "Return cons cell of bounds of folding overlay at POS.
Returns nil if not found."
  (let ((overlays (overlays-at (or pos (point))))
        o)
    (while (and overlays
                (not o))
      (if (overlay-get (car overlays) 'invisible)
          (setq o (car overlays))
        (setq overlays (cdr overlays))))
    (if o
        (cons (overlay-start o) (overlay-end o)))))

(defun js2-mode-function-at-point (&optional pos)
  "Return the innermost function node enclosing current point.
Returns nil if point is not in a function."
  (let ((node (js2-node-at-point pos)))
    (while (and node (not (js2-function-node-p node)))
      (setq node (js2-node-parent node)))
    (if (js2-function-node-p node)
        node)))

(defun js2-mode-toggle-element ()
  "Hide or show the foldable element at the point."
  (interactive)
  (let (comment fn pos)
    (save-excursion
      (save-match-data
        (cond
         ;; /* ... */ comment?
         ((js2-block-comment-p (setq comment (js2-comment-at-point)))
          (if (js2-mode-invisible-overlay-bounds
               (setq pos (+ 3 (js2-node-abs-pos comment))))
              (progn
                (goto-char pos)
                (js2-mode-show-element))
            (js2-mode-hide-element)))

         ;; //-comment?
         ((save-excursion
            (back-to-indentation)
            (looking-at js2-mode-//-comment-re))
          (js2-mode-toggle-//-comment))
        
         ;; function?
         ((setq fn (js2-mode-function-at-point))
          (setq pos (and (js2-function-node-body fn)
                         (js2-node-abs-pos (js2-function-node-body fn))))
          (goto-char (1+ pos))
          (if (js2-mode-invisible-overlay-bounds)
              (js2-mode-show-element)
            (js2-mode-hide-element)))
         (t
          (message "Nothing at point to hide or show")))))))

(defun js2-mode-hide-element ()
  "Fold/hide contents of a block, showing ellipses.
Show the hidden text with \\[js2-mode-show-element]."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-element))
  (let (node body beg end)
    (cond
     ((js2-mode-invisible-overlay-bounds)
      (message "already hidden"))
     (t
      (setq node (js2-node-at-point))
      (cond
       ((js2-block-comment-p node)
        (js2-mode-hide-comment node))
       (t
        (while (and node (not (js2-function-node-p node)))
          (setq node (js2-node-parent node)))
        (if (and node
                 (setq body (js2-function-node-body node)))
            (progn
              (setq beg (js2-node-abs-pos body)
                    end (+ beg (js2-node-len body)))
              (js2-mode-flag-region (1+ beg) (1- end) 'hide))
          (message "No collapsable element found at point"))))))))

(defun js2-mode-show-element ()
  "Show the hidden element at current point."
  (interactive)
  (let ((bounds (js2-mode-invisible-overlay-bounds)))
    (if bounds
        (js2-mode-flag-region (car bounds) (cdr bounds) nil)
      (message "Nothing to un-hide"))))

(defun js2-mode-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (js2-mode-flag-region (point-min) (point-max) nil))

(defun js2-mode-toggle-hide-functions ()
  (interactive)
  (if js2-mode-functions-hidden
      (js2-mode-show-functions)
    (js2-mode-hide-functions)))

(defun js2-mode-hide-functions ()
  "Hides all non-nested function bodies in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-functions))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-functions-hidden t)
    (js2-visit-ast js2-mode-ast #'js2-mode-function-hider)))

(defun js2-mode-function-hider (n endp)
  (when (not endp)
    (let ((tt (js2-node-type n))
          body beg end)
      (cond
       ((and (= tt js2-FUNCTION)
             (setq body (js2-function-node-body n)))
        (setq beg (js2-node-abs-pos body)
              end (+ beg (js2-node-len body)))
        (js2-mode-flag-region (1+ beg) (1- end) 'hide)
        nil)   ; don't process children of function
       (t
        t))))) ; keep processing other AST nodes

(defun js2-mode-show-functions ()
  "Un-hide any folded function bodies in the buffer."
  (interactive)
  (setq js2-mode-functions-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (and (overlay-get o 'invisible)
                   (not (overlay-get o 'comment)))
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-hide-comment (n)
  (let* ((head (if (eq (js2-comment-node-format n) 'jsdoc)
                   3                    ; /**
                 2))                    ; /*
         (beg (+ (js2-node-abs-pos n) head))
         (end (- (+ beg (js2-node-len n)) head 2))
         (o (js2-mode-flag-region beg end 'hide)))
    (overlay-put o 'comment t)))

(defun js2-mode-toggle-hide-comments ()
  "Folds all block comments in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-comments-hidden
      (js2-mode-show-comments)
    (js2-mode-hide-comments)))

(defun js2-mode-hide-comments ()
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-comments))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-comments-hidden t)
    (dolist (n (js2-ast-root-comments js2-mode-ast))
      (let ((format (js2-comment-node-format n)))
        (when (js2-block-comment-p n)
          (js2-mode-hide-comment n))))
    (js2-mode-hide-//-comments)))

(defsubst js2-mode-extend-//-comment (direction)
  "Find start or end of a block of similar //-comment lines.
DIRECTION is -1 to look back, 1 to look forward.
INDENT is the indentation level to match.
Returns the end-of-line position of the furthest adjacent
//-comment line with the same indentation as the current line.
If there is no such matching line, returns current end of line."
  (let ((pos (point-at-eol))
        (indent (current-indentation)))
    (save-excursion
      (save-match-data
        (while (and (zerop (forward-line direction))
                    (looking-at js2-mode-//-comment-re)
                    (eq indent (length (match-string 1))))
          (setq pos (point-at-eol)))
      pos))))

(defun js2-mode-hide-//-comments ()
  "Fold adjacent 1-line comments, showing only snippet of first one."
  (let (beg end)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward js2-mode-//-comment-re nil t)
          (setq beg (point)
                end (js2-mode-extend-//-comment 1))
          (unless (eq beg end)
            (overlay-put (js2-mode-flag-region beg end 'hide)
                         'comment t))
          (goto-char end)
          (forward-char 1))))))

(defun js2-mode-toggle-//-comment ()
  "Fold or un-fold any multi-line //-comment at point.
Caller should have determined that this line starts with a //-comment."
  (let* ((beg (point-at-eol))
         (end beg))
    (save-excursion
      (goto-char end)
      (if (js2-mode-invisible-overlay-bounds)
          (js2-mode-show-element)
        ;; else hide the comment
        (setq beg (js2-mode-extend-//-comment -1)
              end (js2-mode-extend-//-comment 1))
        (unless (eq beg end)
          (overlay-put (js2-mode-flag-region beg end 'hide)
                       'comment t))))))

(defun js2-mode-show-comments ()
  "Un-hide any hidden comments, leaving other hidden elements alone."
  (interactive)
  (setq js2-mode-comments-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'comment)
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-display-warnings-and-errors ()
  "Turn on display of warnings and errors."
  (interactive)
  (setq js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings t)
  (js2-reparse 'force))

(defun js2-mode-hide-warnings-and-errors ()
  "Turn off display of warnings and errors."
  (interactive)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (js2-reparse 'force))

(defun js2-mode-toggle-warnings-and-errors ()
  "Toggle the display of warnings and errors.
Some users don't like having warnings/errors reported while they type."
  (interactive)
  (setq js2-mode-show-parse-errors (not js2-mode-show-parse-errors)
        js2-mode-show-strict-warnings (not js2-mode-show-strict-warnings))
  (if (interactive-p)
      (message "warnings and errors %s"
               (if js2-mode-show-parse-errors
                   "enabled"
                 "disabled")))
  (js2-reparse 'force))

(defun js2-mode-customize ()
  (interactive)
  (customize-group 'js2-mode))

(defun js2-mode-forward-sexp (&optional arg)
  "Move forward across one statement or balanced expression.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-forward-sexp))
  (let (node end (start (point)))
    (cond
     ;; backward-sexp
     ;; could probably make this "better" for some cases:
     ;;  - if in statement block (e.g. function body), go to parent
     ;;  - infix exprs like (foo in bar) - maybe go to beginning
     ;;    of infix expr if in the right-side expression?
     ((and arg (minusp arg))
      (dotimes (i (- arg))
        (js2-backward-sws)
        (forward-char -1)  ; enter the node we backed up to
        (setq node (js2-node-at-point (point) t))
        (goto-char (if node
                       (js2-node-abs-pos node)
                     (point-min)))))
    (t
     ;; forward-sexp
     (js2-forward-sws)
     (dotimes (i arg)
       (js2-forward-sws)
       (setq node (js2-node-at-point (point) t)
             end (if node (+ (js2-node-abs-pos node)
                             (js2-node-len node))))
       (goto-char (or end (point-max))))))))

(defun js2-next-error (&optional arg reset)
  "Move to next parse error.
Typically invoked via \\[next-error].
ARG is the number of errors, forward or backward, to move.
RESET means start over from the beginning."
  (interactive "p")
  (if (or (null js2-mode-ast)
          (and (null (js2-ast-root-errors js2-mode-ast))
               (null (js2-ast-root-warnings js2-mode-ast))))
      (message "No errors")
    (when reset
      (goto-char (point-min)))
    (let* ((errs (copy-sequence
                  (append (js2-ast-root-errors js2-mode-ast)
                          (js2-ast-root-warnings js2-mode-ast))))
           (continue t)
           (start (point))
           (count (or arg 1))
           (backward (minusp count))
           (sorter (if backward '> '<))
           (stopper (if backward '< '>))
           (count (abs count))
           all-errs
           err)
      ;; sort by start position
      (setq errs (sort errs (lambda (e1 e2)
                              (funcall sorter (second e1) (second e2))))
            all-errs errs)
      ;; find nth error with pos > start
      (while (and errs continue)
        (when (funcall stopper (cadar errs) start)
          (setq err (car errs))
          (if (zerop (decf count))
              (setq continue nil)))
        (setq errs (cdr errs)))
      (if err
          (goto-char (second err))
        ;; wrap around to first error
        (goto-char (second (car all-errs)))
        ;; if we were already on it, echo msg again
        (if (= (point) start)
            (js2-echo-error (point) (point)))))))

(defun js2-mouse-3 ()
  "Make right-click move the point to the click location.
This makes right-click context menu operations a bit more intuitive.
The point will not move if the region is active, however, to avoid
destroying the region selection."
  (interactive)
  (when (and js2-move-point-on-right-click
             (not mark-active))
    (let ((e last-input-event))
      (ignore-errors
        (goto-char (cadadr e))))))

(defun js2-mode-create-imenu-index ()
  "Return an alist for `imenu--index-alist'."
  ;; This is built up in `js2-parse-record-imenu' during parsing.
  (when js2-mode-ast
    ;; if we have an ast but no recorder, they're requesting a rescan
    (unless js2-imenu-recorder
      (js2-reparse 'force))
    (prog1
        (js2-build-imenu-index)
      (setq js2-imenu-recorder nil
            js2-imenu-function-map nil))))

(defun js2-mode-find-tag ()
  "Replacement for `find-tag-default'.
`find-tag-default' returns a ridiculous answer inside comments."
  (let (beg end)
    (js2-with-underscore-as-word-syntax
      (save-excursion
        (if (and (not (looking-at "[A-Za-z0-9_$]"))
                 (looking-back "[A-Za-z0-9_$]"))
            (setq beg (progn (forward-word -1) (point))
                  end (progn (forward-word 1) (point)))
          (setq beg (progn (forward-word 1) (point))
                end (progn (forward-word -1) (point))))
        (replace-regexp-in-string
         "[\"']" ""
         (buffer-substring-no-properties beg end))))))

(defun js2-mode-forward-sibling ()
  "Move to the end of the sibling following point in parent.
Returns non-nil if successful, or nil if there was no following sibling."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-after (point) parent))
      (goto-char (+ (js2-node-abs-pos sib)
                    (js2-node-len sib))))))

(defun js2-mode-backward-sibling ()
  "Move to the beginning of the sibling node preceding point in parent.
Parent is defined as the enclosing script or function."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-before (point) parent))
      (goto-char (js2-node-abs-pos sib)))))

(defun js2-beginning-of-defun ()
  "Go to line on which current function starts, and return non-nil.
If we're not in a function, go to beginning of previous script-level element."
  (interactive)
  (let ((parent (js2-node-parent-script-or-fn (js2-node-at-point)))
        pos sib)
    (cond
     ((and (js2-function-node-p parent)
           (not (eq (point) (setq pos (js2-node-abs-pos parent)))))
      (goto-char pos))
     (t
      (js2-mode-backward-sibling)))))

(defun js2-end-of-defun ()
  "Go to the char after the last position of the current function.
If we're not in a function, skips over the next script-level element."
  (interactive)
  (let ((parent (js2-node-parent-script-or-fn (js2-node-at-point))))
    (if (not (js2-function-node-p parent))
        ;; punt:  skip over next script-level element beyond point
        (js2-mode-forward-sibling)
      (goto-char (+ 1 (+ (js2-node-abs-pos parent)
                         (js2-node-len parent)))))))

(defun js2-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point.

Interactively, if this command is repeated,
or (in Transient Mark mode) if the mark is active,
it marks the next defun after the ones already marked."
  (interactive "p")
  (let (extended)
    (when (and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (and transient-mark-mode mark-active)))
      (let ((sib (save-excursion
                   (goto-char (mark))
                   (if (js2-mode-forward-sibling)
                       (point))))
            node)
        (if sib
            (progn
              (set-mark sib)
              (setq extended t))
          ;; no more siblings - try extending to enclosing node
          (goto-char (mark t)))))
   (when (not extended)
     (let ((node (js2-node-at-point (point) t)) ; skip comments
           ast fn stmt parent beg end)
       (when (js2-ast-root-p node)
         (setq ast node
               node (or (js2-node-find-child-after (point) node)
                        (js2-node-find-child-before (point) node))))
       ;; only mark whole buffer if we can't find any children
       (if (null node)
           (setq node ast))
       (if (js2-function-node-p node)
           (setq parent node)
         (setq fn (js2-mode-find-enclosing-fn node)
               stmt (if (or (null fn)
                            (js2-ast-root-p fn))
                        (js2-mode-find-first-stmt node))
               parent (or stmt fn)))
       (setq beg (js2-node-abs-pos parent)
             end (+ beg (js2-node-len parent)))
       (push-mark beg)
       (goto-char end)
       (exchange-point-and-mark)))))

(defun js2-narrow-to-defun ()
  "Narrow to the function enclosing point."
  (interactive)
  (let* ((node (js2-node-at-point (point) t))  ; skip comments
         (fn (if (js2-script-node-p node)
                 node
               (js2-mode-find-enclosing-fn node)))
         (beg (js2-node-abs-pos fn)))
    (unless (js2-ast-root-p fn)
      (narrow-to-region beg (+ beg (js2-node-len fn))))))

(defalias 'js2r 'js2-mode-reset)

(provide 'js2-mode)

;;; js2-mode.el ends here
