;;; js2-indent.el --- indentation for js2-mode
;;
;; Copyright (C) 2008 Steve Yegge
;; Author:  Steve Yegge (steve.yegge@gmail.com)
;; Maintainer:  Steve Yegge (steve.yegge@gmail.com)

;; Commentary:
;;
;; This indenter is based on Karl Landström's "javascript.el" indenter.
;; Karl cleverly deduces that the desired indentation level is often a
;; function of paren/bracket/brace nesting depth, which can be determined
;; quickly via the built-in `parse-partial-sexp' function.  His indenter
;; then does some equally clever checks to see if we're in the context of a
;; substatement of a possibly braceless statement keyword such as if, while,
;; or finally.  This approach yields pretty good results.
;;
;; The indenter is often "wrong", however, and needs to be overridden.
;; The right long-term solution is probably to emulate (or modify)
;; cc-engine, but it's thousands upon thousands of lines of code.  Even
;; if you were to assume the accurate parse tree from `js2-parse' is
;; present, indentation is still thousands of lines of code (I've been
;; down that path) to handle every possible syntactic edge case, and in
;; any case, relying on the parse tree is undesirable because parsing is
;; slow.  So you might as well go the cc-engine approach, but it's a
;; huge pile of work that I'm just not up for any time soon.
;;
;; In the meantime, the compromise solution is that we offer a
;; "bounce indenter", configured with `js2-bounce-indent-flag', which
;; cycles the current line indent among various likely guess points.
;; This approach is far from perfect, but should at least make it
;; slightly easier to move the line towards its desired indentation
;; when manually overriding Karl's heuristic nesting guesser.
;;
;; I've made miscellaneous tweaks to Karl's code to handle some Ecma
;; extensions such as `let' and Array comprehensions, and will likely
;; make further tweaks to it, but major kudos to Karl for coming up with
;; the initial approach, which packs a lot of punch for so little code.

;;; Code:

(defconst js-possibly-braceless-keyword-re
  (regexp-opt
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with" "let")
   'words)
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js-indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

;; This function has horrible results if you're typing an array
;; such as [[1, 2], [3, 4], [5, 6]].  Bounce indenting -really- sucks
;; in conjunction with electric-indent, so just disabling it.
(defsubst js2-code-at-bol-p ()
  "Return t if the first character on line is non-whitespace."
  nil)
;;        (not (memq (char-after (point-at-bol))
;;                   '(? ?\t)))))

(defun js2-insert-and-indent (key)
  "Run command bound to key and indent current line. Runs the command
bound to KEY in the global keymap and indents the current line."
  (interactive (list (this-command-keys)))
  (let ((cmd (lookup-key (current-global-map) key)))
    (if (commandp cmd)
        (call-interactively cmd)))
  ;; don't do the electric keys inside comments or strings,
  ;; and don't do bounce-indent with them.
  (let ((parse-state (parse-partial-sexp (point-min) (point)))
        (js2-bounce-indent-flag (js2-code-at-bol-p)))
    (unless (or (nth 3 parse-state)
                (nth 4 parse-state))
      (indent-according-to-mode))))

(defun js-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-forward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-forward 
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))

(defun js-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments. Invokes
`re-search-forward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(js-re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js-re-search-backward'."
  (let ((parse)
        (saved-point (point-min)))
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (parse-partial-sexp saved-point (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse))) 
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse) 
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))

(defun js-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments. Invokes
`re-search-backward' but treats the buffer as if strings and
comments have been removed."
  (let ((saved-point (point))
        (search-expr 
         (cond ((null count)
                '(js-re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(js-re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(js-re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js-looking-at-operator-p ()
  "Return non-nil if text after point is an operator (that is not
a comma)."
  (save-match-data
    (and (looking-at js-indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (js-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                    (looking-at "?")))))))

(defun js-continued-expression-p ()
  "Returns non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js-looking-at-operator-p)
        (and (js-re-search-backward "\n" nil t)
	     (progn 
	       (skip-chars-backward " \t")
	       (backward-char)
	       (and (js-looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "\\*\\|++\\|--\\|/[/*]"))))))))))

(defun js-end-of-do-while-loop-p ()
  "Returns non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\<while\\>")
	(if (save-excursion 
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion 
	      (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
	  (js-re-search-backward "\\<do\\>" (point-at-bol) t)
	  (or (looking-at "\\<do\\>")
	      (let ((saved-indent (current-indentation)))
		(while (and (js-re-search-backward "^[ \t]*\\<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "[ \t]*\\<do\\>")
		     (not (js-re-search-forward 
			   "\\<while\\>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))

(defun js-ctrl-statement-indentation ()
  "Returns the proper indentation of the current line if it
starts the body of a control statement without braces, else
returns nil."
  (let (forward-sexp-function)  ; temporarily unbind it
    (save-excursion
      (back-to-indentation)
      (when (save-excursion
              (and (not (js2-same-line (point-min)))
                   (not (looking-at "{"))
                   (js-re-search-backward "[[:graph:]]" nil t)
                   (not (looking-at "[{([]"))
                   (progn
                     (forward-char) 
                     ;; scan-sexps sometimes throws an error
                     (ignore-errors (backward-sexp))
                     (when (looking-at "(") (backward-word 1))
                     (and (save-excursion
                            (skip-chars-backward " \t}" (point-at-bol))
                            (bolp))
                          (looking-at js-possibly-braceless-keyword-re)
                          (not (js-end-of-do-while-loop-p))))))
        (save-excursion
          (goto-char (match-beginning 0))
          (+ (current-indentation) js2-basic-offset))))))

(defun js2-indent-in-array-comp (parse-status)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (when (nth 1 parse-status)
      (save-excursion
        (goto-char (nth 1 parse-status))
        (when (looking-at "\\[")
          (forward-char 1)
          (js2-forward-sws)
          (if (looking-at "[[{]")
              (let (forward-sexp-function) ; use lisp version
                (forward-sexp)             ; skip destructuring form
                (js2-forward-sws)
                (if (and (/= (char-after) ?,) ; regular array
                         (looking-at "for"))
                    (match-beginning 0)))
            ;; to skip arbitrary expressions we need the parser,
            ;; so we'll just guess at it.
            (if (re-search-forward "[^,]* \\(for\\) " end t)
                (match-beginning 1))))))))

(defun js2-array-comp-indentation (parse-status for-kwd)
  (if (js2-same-line for-kwd)
      ;; first continuation line
      (save-excursion
        (goto-char (nth 1 parse-status))
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))
      
(defun js-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let ((ctrl-stmt-indent (js-ctrl-statement-indentation))
          (same-indent-p (looking-at "[]})]\\|\\<case\\>\\|\\<default\\>"))
          (continued-expr-p (js-continued-expression-p))
          (bracket (nth 1 parse-status))
          beg)
      (cond
       ;; indent array comprehension continuation lines specially
       ((and bracket
             (not (js2-same-line bracket))
             (setq beg (js2-indent-in-array-comp parse-status))
             (>= (point) (save-excursion
                           (goto-char beg)
                           (point-at-bol)))) ; at or after first loop?
        (js2-array-comp-indentation parse-status beg))

       (ctrl-stmt-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (let ((p (parse-partial-sexp (point-at-bol) (point))))
            (when (save-excursion (skip-chars-backward " \t)")
                                  (looking-at ")"))
              (backward-list))
            (if (nth 1 p)
                (progn (goto-char (1+ (nth 1 p)))
                       (skip-chars-forward " \t"))
              (back-to-indentation))
            (cond (same-indent-p
                   (current-column))
                  (continued-expr-p
                   (+ (current-column) (* 2 js2-basic-offset)))
                  (t
                   (+ (current-column) js2-basic-offset)))))
         (t
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

       (continued-expr-p js2-basic-offset)
       (t 0)))))

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (offset (save-excursion
                   (goto-char beg)
                   (if (looking-at "/\\*")
                       (+ 1 (current-column))
                     0))))
    (unless first-line
      (indent-line-to offset))))

(defun js2-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js2-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defsubst js2-current-indent (&optional pos)
  "Return column of indentation on current line.
If POS is non-nil, go to that point and return indentation for that line."
  (save-excursion
    (if pos
        (goto-char pos))
    (back-to-indentation)
    (current-column)))

(defsubst js2-arglist-close ()
  "Return non-nil if we're on a line beginning with a close-paren/brace."
  (save-match-data
    (save-excursion
      (goto-char (point-at-bol))
      (js2-forward-sws)
      (looking-at "[])}]"))))

(defsubst js2-indent-looks-like-label-p ()
  (goto-char (point-at-bol))
  (js2-forward-sws)
  (looking-at (concat js2-mode-identifier-re ":")))

(defun js2-indent-in-objlit-p (parse-status)
  "Return non-nil if this looks like an object-literal entry."
  (let ((start (nth 1 parse-status)))
    (and
     start
     (save-excursion
       (and (zerop (forward-line -1))
            (not (< (point) start))     ; crossed a {} boundary
            (js2-indent-looks-like-label-p)))
     (save-excursion
       (js2-indent-looks-like-label-p)))))

;; if prev line looks like foobar({ then we're passing an object
;; literal to a function call, and people pretty much always want to
;; de-dent back to the previous line, so move the 'basic-offset'
;; position to the front.
(defsubst js2-indent-objlit-arg-p (parse-status)
  (save-excursion
    (back-to-indentation)
    (js2-backward-sws)
    (and (eq (1- (point)) (nth 1 parse-status))
         (eq (char-before) ?{)
         (progn
           (forward-char -1)
           (skip-chars-backward " \t")
           (eq (char-before) ?\()))))

(defsubst js2-indent-case-block-p ()
  (save-excursion
    (back-to-indentation)
    (js2-backward-sws)
    (goto-char (point-at-bol))
    (skip-chars-forward " \t")
    (save-match-data
      (looking-at "case\\s-.+:"))))

(defsubst js2-syntax-bol ()
  "Return the point at the first non-whitespace char on the line.
Returns `point-at-bol' if the line is empty."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (point)))

(defun js2-bounce-indent (normal-col parse-status)
  "Cycle among alternate computed indentation positions.
PARSE-STATUS is the result of `parse-partial-sexp' from the beginning
of the buffer to the current point.  NORMAL-COL is the indentation
column computed by the heuristic guesser based on current paren,
bracket, brace and statement nesting."
  (let ((cur-indent (js2-current-indent))
        (old-buffer-undo-list buffer-undo-list)
        ;; Emacs 21 only has `count-lines', not `line-number-at-pos'
        (current-line (save-excursion
                        (forward-line 0)  ; move to bol
                        (1+ (count-lines (point-min) (point)))))
        positions
        pos
        anchor
        arglist-cont
        same-indent
        prev-line-col
        basic-offset
        computed-pos)
    ;; temporarily don't record undo info, if user requested this
    (if js2-mode-indent-inhibit-undo
        (setq buffer-undo-list t))
    (unwind-protect
        (progn
          ;; first likely point:  indent from beginning of previous code line
          (push (setq basic-offset
                      (+ (save-excursion
                           (back-to-indentation)
                           (js2-backward-sws)
                           (back-to-indentation)
                           (setq prev-line-col (current-column)))
                         js2-basic-offset))
                positions)

          ;; second likely point:  indent from assign-expr RHS.  This
          ;; is just a crude guess based on finding " = " on the previous
          ;; line containing actual code.
          (setq pos (save-excursion
                      (save-match-data
                        (forward-line -1)
                        (goto-char (point-at-bol))
                        (when (re-search-forward "\\s-+\\(=\\)\\s-+"
                                                 (point-at-eol) t)
                          (goto-char (match-end 1))
                          (skip-chars-forward " \t\r\n")
                          (current-column)))))
          (when pos
            (incf pos js2-basic-offset)
            (unless (member pos positions)
              (push pos positions)))

          ;; third likely point:  same indent as previous line of code.
          ;; Make it the first likely point if we're not on an
          ;; arglist-close line and previous line ends in a comma, or
          ;; both this line and prev line look like object-literal
          ;; elements.
          (setq pos (save-excursion
                      (goto-char (point-at-bol))
                      (js2-backward-sws)
                      (back-to-indentation)
                      (prog1
                          (current-column)
                        ;; while we're here, look for trailing comma
                        (if (save-excursion
                              (goto-char (point-at-eol))
                              (js2-backward-sws)
                              (eq (char-before) ?,))
                            (setq arglist-cont (1- (point)))))))
          (when pos
            (if (and (or arglist-cont
                         (js2-indent-in-objlit-p parse-status))
                     (not (js2-arglist-close)))
                (setq same-indent pos))
            (unless (member pos positions)
              (push pos positions)))

          ;; fourth likely position:  first preceding code with less indentation
          ;; than the immediately preceding code line.
          (setq pos (save-excursion
                      (js2-backward-sws)
                      (back-to-indentation)
                      (setq anchor (current-column))
                      (while (and (zerop (forward-line -1))
                                  (>= (progn
                                        (back-to-indentation)
                                        (current-column))
                                      anchor)))
                      (setq pos (current-column))))
          (unless (member pos positions)
            (push pos positions))

          ;; put nesting-heuristic position first in list, sort rest
          (setq positions (nreverse (sort positions '<)))
          (setq positions (cons normal-col (delete normal-col positions)))

          ;; comma-list continuation lines:  prev line indent takes precedence
          (if same-indent
              (setq positions
                    (cons same-indent
                          (sort (delete same-indent positions) '<))))

          ;; common special cases where we want to indent in from previous line
          (if (or (js2-indent-case-block-p)
                  (js2-indent-objlit-arg-p parse-status))
              (setq positions
                    (cons basic-offset
                          (delete basic-offset positions))))

          ;; record whether we're already sitting on one of the alternatives
          (setq pos (member cur-indent positions))
          (cond
           ;; case 0:  we're one one of the alternatives and this is the
           ;; first time they've pressed TAB on this line (best-guess).
           ((and js2-mode-indent-ignore-first-tab
                 pos
                 ;; first time pressing TAB on this line?
                 (not (eq js2-mode-last-indented-line current-line)))
            ;; do nothing
            (setq computed-pos nil))
           ;; case 1:  only one computed position => use it
           ((null (cdr positions))
            (setq computed-pos 0))
           ;; case 2:  not on any of the computed spots => use main spot
           ((not pos)
            (setq computed-pos 0))
           ;; case 3:  on last position:  cycle to first position
           ((null (cdr pos))
            (setq computed-pos 0))
           ;; case 4:  on intermediate position:  cycle to next position
           (t
            (setq computed-pos (js2-position (second pos) positions))))

          ;; see if any hooks want to indent; otherwise we do it
          (loop with result = nil
                for hook in js2-indent-hook
                while (null result)
                do
                (setq result (funcall hook positions computed-pos))
                finally do
                (unless (or result (null computed-pos))
                  (indent-line-to (nth computed-pos positions)))))

      ;; finally
      (if js2-mode-indent-inhibit-undo
          (setq buffer-undo-list old-buffer-undo-list))
      ;; see commentary for `js2-mode-last-indented-line'
      (setq js2-mode-last-indented-line current-line))))
      
(defsubst js2-1-line-comment-continuation-p ()
  "Return t if we're in a 1-line comment continuation.
If so, we don't ever want to use bounce-indent."
  (save-excursion
    (save-match-data
      (and (progn
             (forward-line 0)
             (looking-at "\\s-*//"))
           (progn
             (forward-line -1)
             (forward-line 0)
             (when (looking-at "\\s-*$")
               (js2-backward-sws)
               (forward-line 0))
             (looking-at "\\s-*//"))))))
       
(defun js2-indent-line ()
  "Indent the current line as JavaScript source text."
  (interactive)
  (let (parse-status
        current-indent
        offset
        indent-col
        moved
        ;; don't whine about errors/warnings when we're indenting.
        ;; This has to be set before calling parse-partial-sexp below.
        (inhibit-point-motion-hooks t))
    (setq parse-status (save-excursion
                          (parse-partial-sexp (point-min)
                                              (point-at-bol)))
          offset (- (point) (save-excursion
                               (back-to-indentation)
                               (setq current-indent (current-column))
                               (point))))
    (js2-with-underscore-as-word-syntax
     (if (nth 4 parse-status)
         (js2-lineup-comment parse-status)
       (setq indent-col (js-proper-indentation parse-status))
       ;; see comments below about js2-mode-last-indented-line
       (when 
           (cond
            ;; bounce-indenting is disabled during electric-key indent.
            ;; It doesn't work well on first line of buffer.
            ((and js2-bounce-indent-flag
                  (not (js2-same-line (point-min)))
                  (not (js2-1-line-comment-continuation-p)))
             (js2-bounce-indent indent-col parse-status)
             (setq moved t))
            ;; just indent to the guesser's likely spot
            ((/= current-indent indent-col)
             (indent-line-to indent-col)
             (setq moved t)))
         (when (and moved (plusp offset))
           (forward-char offset)))))))

(defun js2-indent-region (start end)
  "Indent the region, but don't use bounce indenting."
  (let ((js2-bounce-indent-flag nil)
        (indent-region-function nil))
    (indent-region start end nil)))  ; nil for byte-compiler

(provide 'js2-indent)

;;; js2-indent.el ends here
