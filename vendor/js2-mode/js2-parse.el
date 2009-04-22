;;; js2-parse.el --- JavaScript parser

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

;; Commentary:

;; This is based on Rhino's parser and tries to follow its code
;; structure as closely as practical, so that changes to the Rhino
;; parser can easily be propagated into this code.  However, Rhino
;; does not currently generate a usable AST representation, at least
;; from an IDE perspective, so we build our own more suitable AST.

;; The AST node structures are defined in `js2-ast.el'.
;; Every parser function that creates and returns an AST node has
;; the following responsibilities:

;;   1) set the node start to the absolute buffer start position
;;   2) set the node length to include any closing chars (RC, SEMI)
;;   3) fix up any child-node starts to be relative to this node
;;   4) set any field positions (e.g. keywords) relative to this node
;;   5) report any child nodes with `js2-node-add-children'
;;      (note that this call fixes up start positions by default)

;; The resulting AST has all node start positions relative to the
;; parent nodes; only the root has an absolute start position.

;; Note: fontification is done inline while parsing.  It used to be
;; done in a second pass over the AST, but doing it inline is about
;; twice as fast.  Most of the fontification happens when tokens are
;; scanned, and the parser has a few spots that perform extra
;; fontification.  In addition to speed, a second benefit of inline
;; parsing is that if a long parse is interrupted, everything parsed
;; so far is still fontified.

;; The editing mode that uses this parser, `js2-mode', directs the
;; parser to check periodically for user input.  If user input
;; arrives, the parse is abandoned, except for the highlighting that
;; has occurred so far, and a re-parse is rescheduled for when Emacs
;; becomes idle again.  This works pretty well, but could be better.
;; In particular, when the user input has not resulted in changes to
;; the buffer (for instance, navigation input), the parse tree built
;; so far should not be discarded, and the parse should continue where
;; it left off.  It will be some work to create what amounts to a
;; continuation, but it should not be unreasonably difficult.

;; TODO:
;; - make non-editing input restart parse at previous continuation
;; - in Eclipse, sibling nodes never overlap start/end ranges
;;   - for getters, prop name and function nodes overlap
;;   - should write a debug tree visitor to look for overlaps
;; - mark array and object literals as "destructuring" (node prop?)
;;   so we can syntax-highlight them properly.
;; - figure out a way not to store value in string/name nodes
;;   - needs a solution for synthetic nodes

;;; Code

(eval-and-compile
  (require 'cl))  ; for delete-if

(require 'js2-util)
(require 'js2-ast)       ; node classes
(require 'js2-scan)      ; tokenizer
(require 'js2-highlight) ; syntax coloring
(require 'js2-browse)    ; imenu support

(defconst js2-version "1.7.0"
  "Version of JavaScript supported, plus minor js2 version.")

(defmacro js2-record-face (face)
  "Record a style run of FACE for the current token."
  `(js2-set-face js2-token-beg js2-token-end ,face 'record))

(defsubst js2-node-end (n)
  "Computes the absolute end of node N.
Use with caution!  Assumes `js2-node-pos' is -absolute-, which
is only true until the node is added to its parent; i.e., while parsing."
  (+ (js2-node-pos n)
     (js2-node-len n)))

(defsubst js2-record-comment ()
  (push (make-js2-comment-node :len (- js2-token-end js2-token-beg)
                               :format js2-ts-comment-type)
        js2-scanned-comments)
  (when js2-parse-ide-mode
    (js2-record-face 'font-lock-comment-face)
    (when (memq js2-ts-comment-type '(html preprocessor))
      ;; Tell cc-engine the bounds of the comment.
      (put-text-property js2-token-beg (1- js2-token-end) 'c-in-sws t))))

;; This function is called depressingly often, so it should be fast.
;; Most of the time it's looking at the same token it peeked before.
(defsubst js2-peek-token ()
  "Returns the next token without consuming it.
If previous token was consumed, calls scanner to get new token.
If previous token was -not- consumed, returns it (idempotent).

This function will not return a newline (js2-EOL) - instead, it
gobbles newlines until it finds a non-newline token, and flags
that token as appearing just after a newline.

This function will also not return a js2-COMMENT.  Instead, it
records comments found in `js2-scanned-comments'.  If the token
returned by this function immediately follows a jsdoc comment,
the token is flagged as such.

Note that this function always returned the un-flagged token!
The flags, if any, are saved in `js2-current-flagged-token'."
  (if (/= js2-current-flagged-token js2-EOF) ; last token not consumed
      js2-current-token  ; most common case - return already-peeked token
    (let ((tt (js2-get-token))          ; call scanner
          saw-eol
          face)
      ;; process comments and whitespace
      (while (or (= tt js2-EOL)
                 (= tt js2-COMMENT))
        (if (= tt js2-EOL)
            (setq saw-eol t)
          (setq saw-eol nil)
          (if js2-record-comments
              (js2-record-comment)))
        (setq tt (js2-get-token)))  ; call scanner

      (setq js2-current-token tt
            js2-current-flagged-token (if saw-eol
                                          (logior tt js2-ti-after-eol)
                                        tt))
      ;; perform lexical fontification as soon as token is scanned
      (when js2-parse-ide-mode
        (cond
         ((minusp tt)
          (js2-record-face 'js2-error-face))
         ((setq face (aref js2-kwd-tokens tt))
          (js2-record-face face))
         ((and (= tt js2-NAME)
               (equal js2-ts-string "undefined"))
          (js2-record-face 'font-lock-constant-face))))
      tt)))  ; return unflagged token

(defsubst js2-peek-flagged-token ()
  "Returns the current token along with any flags set for it."
  (js2-peek-token)
  js2-current-flagged-token)

(defsubst js2-consume-token ()
  (setq js2-current-flagged-token js2-EOF))

(defsubst js2-next-token ()
  (prog1
      (js2-peek-token)
    (js2-consume-token)))

(defsubst js2-next-flagged-token ()
  (js2-peek-token)
  (prog1 js2-current-flagged-token
    (js2-consume-token)))

(defsubst js2-match-token (match)
  "Consume and return t if next token matches MATCH, a bytecode.
Returns nil and consumes nothing if MATCH is not the next token."
  (if (/= (js2-peek-token) match)
      nil
    (js2-consume-token)
    t))

(defsubst js2-valid-prop-name-token (tt)
  (or (= tt js2-NAME)
      (and js2-allow-keywords-as-property-names
           (plusp tt)
           (aref js2-kwd-tokens tt))))

(defsubst js2-match-prop-name ()
  "Consume token and return t if next token is a valid property name.
It's valid if it's a js2-NAME, or `js2-allow-keywords-as-property-names'
is non-nil and it's a keyword token."
  (if (js2-valid-prop-name-token (js2-peek-token))
      (progn
        (js2-consume-token)
        t)
    nil))

(defsubst js2-must-match-prop-name (msg-id &optional pos len)
  (if (js2-match-prop-name)
      t
    (js2-report-error msg-id nil pos len)
    nil))

(defsubst js2-peek-token-or-eol ()
  "Return js2-EOL if the current token immediately follows a newline.
Else returns the current token.  Used in situations where we don't
consider certain token types valid if they are preceded by a newline.
One example is the postfix ++ or -- operator, which has to be on the
same line as its operand."
  (let ((tt (js2-peek-token)))
    ;; Check for last peeked token flags
    (if (js2-flag-set-p js2-current-flagged-token js2-ti-after-eol)
        js2-EOL
      tt)))

(defsubst js2-set-check-for-label ()
  (assert (= (logand js2-current-flagged-token js2-clear-ti-mask) js2-NAME))
  (js2-set-flag js2-current-flagged-token js2-ti-check-label))

(defsubst js2-must-match (token msg-id &optional pos len)
  "Match next token to token code TOKEN, or record a syntax error.
MSG-ID is the error message to report if the match fails.
Returns t on match, nil if no match."
  (if (js2-match-token token)
      t
    (js2-report-error msg-id nil pos len)
    nil))

(defsubst js2-inside-function ()
  (plusp js2-nesting-of-function))

(defsubst js2-set-requires-activation ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-needs-activation js2-current-script-or-fn) t)))

(defsubst js2-check-activation-name (name token)
  (when (js2-inside-function)
    ;; skip language-version 1.2 check from Rhino
    (if (or (string= "arguments" name)
            (and js2-compiler-activation-names  ; only used in codegen
                 (gethash name js2-compiler-activation-names)))
        (js2-set-requires-activation))))

(defsubst js2-set-is-generator ()
  (if (js2-function-node-p js2-current-script-or-fn)
      (setf (js2-function-node-is-generator js2-current-script-or-fn) t)))

(defsubst js2-must-have-xml ()
  (unless js2-compiler-xml-available
    (js2-report-error "msg.XML.not.available")))

(defsubst js2-push-scope (scope)
  "Push SCOPE, a `js2-scope', onto the lexical scope chain."
  (assert (js2-scope-p scope))
  (assert (null (js2-scope-parent-scope scope)))
  (assert (neq js2-current-scope scope))
  (setf (js2-scope-parent-scope scope) js2-current-scope
        js2-current-scope scope))

(defsubst js2-pop-scope ()
  (setq js2-current-scope
        (js2-scope-parent-scope js2-current-scope)))

(defsubst js2-enter-loop (loop-node)
  (push loop-node js2-loop-set)
  (push loop-node js2-loop-and-switch-set)
  (js2-push-scope loop-node)
  ;; Tell the current labeled statement (if any) its statement,
  ;; and set the jump target of the first label to the loop.
  ;; These are used in `js2-parse-continue' to verify that the
  ;; continue target is an actual labeled loop.  (And for codegen.)
  (when js2-labeled-stmt
    (setf (js2-labeled-stmt-node-stmt js2-labeled-stmt) loop-node
          (js2-label-node-loop (car (js2-labeled-stmt-node-labels
                                     js2-labeled-stmt))) loop-node)))

(defsubst js2-exit-loop ()
  (pop js2-loop-set)
  (pop js2-loop-and-switch-set)
  (js2-pop-scope))

(defsubst js2-enter-switch (switch-node)
  (push switch-node js2-loop-and-switch-set))

(defsubst js2-exit-switch ()
  (pop js2-loop-and-switch-set))

(defun js2-parse (&optional buf cb)
  "Tells the js2 parser to parse a region of JavaScript.

BUF is a buffer or buffer name containing the code to parse.
Call `narrow-to-region' first to parse only part of the buffer.

The returned AST root node is given some additional properties:
  `node-count' - total number of nodes in the AST
  `buffer' - BUF.  The buffer it refers to may change or be killed,
             so the value is not necessarily reliable.

An optional callback CB can be specified to report parsing
progress.  If `(functionp CB)' returns t, it will be called with
the current line number once before parsing begins, then again
each time the lexer reaches a new line number.

CB can also be a list of the form `(symbol cb ...)' to specify
multiple callbacks with different criteria.  Each symbol is a
criterion keyword, and the following element is the callback to
call

  :line  - called whenever the line number changes
  :token - called for each new token consumed

The list of criteria could be extended to include entering or
leaving a statement, an expression, or a function definition."
  (if (and cb (not (functionp cb)))
      (error "criteria callbacks not yet implemented"))
  (let ((inhibit-point-motion-hooks t)
        (js2-compiler-xml-available (>= js2-language-version 160))
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000))
        (case-fold-search nil)
        ast)
    (or buf (setq buf (current-buffer)))
    (save-excursion
      (set-buffer buf)
      (setq js2-scanned-comments nil
            js2-parsed-errors nil
            js2-parsed-warnings nil
            js2-imenu-recorder nil
            js2-imenu-function-map nil
            js2-label-set nil)
      (js2-init-scanner)
      (setq ast (js2-with-unmodifying-text-property-changes
                  (js2-do-parse)))
      (unless js2-ts-hit-eof
        (js2-report-error "msg.got.syntax.errors" (length js2-parsed-errors)))
      (setf (js2-ast-root-errors ast) js2-parsed-errors
            (js2-ast-root-warnings ast) js2-parsed-warnings)
      ;; if we didn't find any declarations, put a dummy in this list so we
      ;; don't end up re-parsing the buffer in `js2-mode-create-imenu-index'
      (unless js2-imenu-recorder
        (setq js2-imenu-recorder 'empty))
      (run-hooks 'js2-parse-finished-hook)
      ast)))

;; Corresponds to Rhino's Parser.parse() method.
(defun js2-do-parse ()
  "Parse current buffer starting from current point.
Scanner should be initialized."
  (let ((pos js2-ts-cursor)
        (end js2-ts-cursor)  ; in case file is empty
        root n tt)
    ;; initialize buffer-local parsing vars
    (setf root (make-js2-ast-root :buffer (buffer-name) :pos pos)
          js2-current-script-or-fn root
          js2-current-scope root
          js2-current-flagged-token js2-EOF
          js2-nesting-of-function 0
          js2-labeled-stmt nil
          js2-recorded-assignments nil)  ; for js2-highlight

    (while (/= (setq tt (js2-peek-token)) js2-EOF)
      (if (= tt js2-FUNCTION)
          (progn
            (js2-consume-token)
            (setq n (js2-parse-function (if js2-called-by-compile-function
                                            'FUNCTION_EXPRESSION
                                          'FUNCTION_STATEMENT)))
            (js2-record-imenu-functions n))
        ;; not a function - parse a statement
        (setq n (js2-parse-statement)))
      ;; add function or statement to script
      (setq end (js2-node-end n))
      (js2-block-node-push root n))

    ;; add comments to root in lexical order
    (when js2-scanned-comments
      ;; if we find a comment beyond end of normal kids, use its end
      (setq end (max end (js2-node-end (first js2-scanned-comments))))
      (dolist (comment js2-scanned-comments)
        (push comment (js2-ast-root-comments root))
        (js2-node-add-children root comment)))

    (setf (js2-node-len root) (- end pos))
    (js2-highlight-undeclared-vars)
    root))

(defun js2-function-parser ()
  (js2-consume-token)
  (js2-parse-function 'FUNCTION_EXPRESSION_STATEMENT))

(defun js2-parse-function-body (fn-node)
  (js2-must-match js2-LC "msg.no.brace.body")
  (let ((pos js2-token-beg)         ; LC position
        (pn (make-js2-block-node))  ; starts at LC position
        tt
        end)
    (incf js2-nesting-of-function)
    (unwind-protect
        (while (not (or (= (setq tt (js2-peek-token)) js2-ERROR)
                        (= tt js2-EOF)
                        (= tt js2-RC)))
          (js2-block-node-push pn (if (/= tt js2-FUNCTION)
                                      (js2-parse-statement)
                                    (js2-consume-token)
                                    (js2-parse-function 'FUNCTION_STATEMENT))))
      (decf js2-nesting-of-function))
    (setq end js2-token-end)  ; assume no curly and leave at current token
    (if (js2-must-match js2-RC "msg.no.brace.after.body" pos)
        (setq end js2-token-end))
    (setf (js2-node-pos pn) pos
          (js2-node-len pn) (- end pos))
    (setf (js2-function-node-body fn-node) pn)
    (js2-node-add-children fn-node pn)
    pn))

(defun js2-parse-function-params (fn-node pos)
  (if (js2-match-token js2-RP)
      (setf (js2-function-node-rp fn-node) (- js2-token-beg pos))
    (let (params len param)
      (loop for tt = (js2-peek-token)
            do
            (cond
             ;; destructuring param
             ((or (= tt js2-LB) (= tt js2-LC))
              (push (js2-parse-primary-expr) params))
             ;; simple name
             (t
              (js2-must-match js2-NAME "msg.no.parm")
              (js2-record-face 'js2-function-param-face)
              (setq param (js2-create-name-node))
              (js2-define-symbol js2-LP js2-ts-string param)
              (push param params)))
            while
            (js2-match-token js2-COMMA))
      (if (js2-must-match js2-RP "msg.no.paren.after.parms")
          (setf (js2-function-node-rp fn-node) (- js2-token-beg pos)))
      (dolist (p params)
        (js2-node-add-children fn-node p)
        (push p (js2-function-node-params fn-node))))))

(defsubst js2-check-inconsistent-return-warning (fn-node name)
  "Possibly show inconsistent-return warning.
Last token scanned is the close-curly for the function body."
  (when (and js2-mode-show-strict-warnings
             js2-strict-inconsistent-return-warning
             (not (js2-has-consistent-return-usage
                   (js2-function-node-body fn-node))))
    ;; Have it extend from close-curly to bol or beginning of block.
    (let ((pos (save-excursion
                 (goto-char js2-token-end)
                 (max (js2-node-abs-pos (js2-function-node-body fn-node))
                      (point-at-bol))))
          (end js2-token-end))
      (if (plusp (js2-name-node-length name))
          (js2-add-strict-warning "msg.no.return.value"
                                  (js2-name-node-name name) pos end)
        (js2-add-strict-warning "msg.anon.no.return.value" nil pos end)))))

(defun js2-parse-function (function-type)
  "Function parser.  FUNCTION-TYPE is a symbol."
  (let ((pos js2-token-beg)  ; start of 'function' keyword
        name
        name-beg
        name-end
        fn-node
        lp
        (synthetic-type function-type)
        member-expr-node)

    ;; parse function name, expression, or non-name (anonymous)
    (cond
     ;; function foo(...)
     ((js2-match-token js2-NAME)
      (setq name (js2-create-name-node t)
            name-beg js2-token-beg
            name-end js2-token-end)
      (unless (js2-match-token js2-LP)
        (when js2-allow-member-expr-as-function-name
          ;; function foo.bar(...)
          (setq member-expr-node name
                name nil
                member-expr-node (js2-parse-member-expr-tail
                                  nil member-expr-node)))
        (js2-must-match js2-LP "msg.no.paren.parms")))

     ((js2-match-token js2-LP)
      nil)  ; anonymous function:  leave name as null

     (t
      ;; function random-member-expr(...)
      (when js2-allow-member-expr-as-function-name
        ;; Note that memberExpr can not start with '(' like
        ;; in function (1+2).toString(), because 'function (' already
        ;; processed as anonymous function
        (setq member-expr-node (js2-parse-member-expr)))
      (js2-must-match js2-LP "msg.no.paren.parms")))

    (if (= js2-current-token js2-LP)  ; eventually matched LP?
        (setq lp js2-token-beg))

    (if member-expr-node
        (progn
          (setq synthetic-type 'FUNCTION_EXPRESSION)
          (js2-parse-highlight-member-expr-fn-name member-expr-node))
      (if name
          (js2-set-face name-beg name-end
                        'font-lock-function-name-face 'record)))

    (if (and (neq synthetic-type 'FUNCTION_EXPRESSION)
             (plusp (js2-name-node-length name)))
        ;; Function statements define a symbol in the enclosing scope
        (js2-define-symbol js2-FUNCTION (js2-name-node-name name) fn-node))

    (setf fn-node (make-js2-function-node :pos pos
                                          :name name
                                          :form function-type
                                          :lp (if lp (- lp pos))))

    (if (or (js2-inside-function) (plusp js2-nesting-of-with))
        ;; 1. Nested functions are not affected by the dynamic scope flag
        ;;    as dynamic scope is already a parent of their scope.
        ;; 2. Functions defined under the with statement also immune to
        ;;    this setup, in which case dynamic scope is ignored in favor
        ;;    of the with object.
        (setf (js2-function-node-ignore-dynamic fn-node) t))

    ;; dynamically bind all the per-function variables
    (let ((js2-current-script-or-fn fn-node)
          (js2-current-scope fn-node)
          (js2-nesting-of-with 0)
          (js2-end-flags 0)
          js2-label-set
          js2-loop-set
          js2-loop-and-switch-set)

      ;; parse params and function body
      (js2-parse-function-params fn-node pos)
      (js2-parse-function-body fn-node)
      (if name
          (js2-node-add-children fn-node name))

      (js2-check-inconsistent-return-warning fn-node name)

      ;; Function expressions define a name only in the body of the
      ;; function, and only if not hidden by a parameter name
      (if (and name
               (eq synthetic-type 'FUNCTION_EXPRESSION)
               (null (js2-scope-get-symbol js2-current-scope
                                           (js2-name-node-name name))))
          (js2-define-symbol js2-FUNCTION
                             (js2-name-node-name name)
                             fn-node))
      (if (and name
               (eq function-type 'FUNCTION_EXPRESSION_STATEMENT))
          (js2-record-imenu-functions fn-node)))

    (setf (js2-node-len fn-node) (- js2-ts-cursor pos)
          (js2-function-node-member-expr fn-node) member-expr-node)  ; may be nil

    ;; Rhino doesn't do this, but we need it for finding undeclared vars.
    ;; We wait until after parsing the function to set its parent scope,
    ;; since `js2-define-symbol' needs the defining-scope check to stop
    ;; at the function boundary when checking for redeclarations.
    (setf (js2-scope-parent-scope fn-node) js2-current-scope)

    fn-node))

(defun js2-parse-statements (&optional parent)
  "Parse a statement list.  Last token consumed must be js2-LC.

PARENT can be a `js2-block-node', in which case the statements are
appended to PARENT.  Otherwise a new `js2-block-node' is created
and returned.

This function does not match the closing js2-RC: the caller
matches the RC so it can provide a suitable error message if not
matched.  This means it's up to the caller to set the length of
the node to include the closing RC.  The node start pos is set to
the absolute buffer start position, and the caller should fix it
up to be relative to the parent node.  All children of this block
node are given relative start positions and correct lengths."
  (let ((pn (or parent (make-js2-block-node)))
        tt)
    (setf (js2-node-pos pn) js2-token-beg)
    (while (and (> (setq tt (js2-peek-token)) js2-EOF)
                (/= tt js2-RC))
      (js2-block-node-push pn (js2-parse-statement)))
    pn))

(defun js2-parse-statement ()
  (let (tt pn beg end)

    ;; coarse-grained user-interrupt check - needs work
    (and js2-parse-interruptable-p
         (zerop (% (incf js2-parse-stmt-count)
                   js2-statements-per-pause))
         (input-pending-p)
         (throw 'interrupted t))

    (setq pn (js2-statement-helper))

    ;; no-side-effects warning check
    (unless (js2-node-has-side-effects pn)
      (setq end (js2-node-end pn))
      (save-excursion
        (goto-char end)
        (setq beg (max (js2-node-pos pn) (point-at-bol))))
      (js2-add-strict-warning "msg.no.side.effects" nil beg end))

    pn))

;; These correspond to the switch cases in Parser.statementHelper
(defconst js2-parsers
  (let ((parsers (make-vector js2-num-tokens
                                #'js2-parse-expr-stmt)))
    (aset parsers js2-BREAK     #'js2-parse-break)
    (aset parsers js2-CONST     #'js2-parse-const-var)
    (aset parsers js2-CONTINUE  #'js2-parse-continue)
    (aset parsers js2-DEBUGGER  #'js2-parse-debugger)
    (aset parsers js2-DEFAULT   #'js2-parse-default-xml-namespace)
    (aset parsers js2-DO        #'js2-parse-do)
    (aset parsers js2-FOR       #'js2-parse-for)
    (aset parsers js2-FUNCTION  #'js2-function-parser)
    (aset parsers js2-IF        #'js2-parse-if)
    (aset parsers js2-LC        #'js2-parse-block)
    (aset parsers js2-LET       #'js2-parse-let-stmt)
    (aset parsers js2-NAME      #'js2-parse-name-or-label)
    (aset parsers js2-RETURN    #'js2-parse-ret-yield)
    (aset parsers js2-SEMI      #'js2-parse-semi)
    (aset parsers js2-SWITCH    #'js2-parse-switch)
    (aset parsers js2-THROW     #'js2-parse-throw)
    (aset parsers js2-TRY       #'js2-parse-try)
    (aset parsers js2-VAR       #'js2-parse-const-var)
    (aset parsers js2-WHILE     #'js2-parse-while)
    (aset parsers js2-WITH      #'js2-parse-with)
    (aset parsers js2-YIELD     #'js2-parse-ret-yield)
    parsers)
  "A vector mapping token types to parser functions.")

(defsubst js2-parse-warn-missing-semi (beg end)
  (and js2-mode-show-strict-warnings
       js2-strict-missing-semi-warning
       (js2-add-strict-warning
        "msg.missing.semi" nil
        ;; back up to beginning of statement or line
        (max beg (save-excursion
                   (goto-char end)
                   (point-at-bol)))
        end)))

(defconst js2-no-semi-insertion
  (list js2-IF
        js2-SWITCH
        js2-WHILE
        js2-DO
        js2-FOR
        js2-TRY
        js2-WITH
        js2-LC
        js2-ERROR
        js2-SEMI
        js2-FUNCTION)
  "List of tokens that don't do automatic semicolon insertion.")

(defconst js2-autoinsert-semi-and-warn
  (list js2-ERROR js2-EOF js2-RC))

(defun js2-statement-helper ()
  (let* ((tt (js2-peek-token))
         (first-tt tt)
         (beg js2-token-beg)
         (parser (if (= tt js2-ERROR)
                     #'js2-parse-semi
                   (aref js2-parsers tt)))
         pn
         tt-flagged)
    ;; If the statement is set, then it's been told its label by now.
    (and js2-labeled-stmt
         (js2-labeled-stmt-node-stmt js2-labeled-stmt)
         (setq js2-labeled-stmt nil))

    (setq pn (funcall parser))

    ;; Don't do auto semi insertion for certain statement types.
    (unless (or (memq first-tt js2-no-semi-insertion)
                (js2-labeled-stmt-node-p pn))
      (js2-auto-insert-semicolon pn))
    pn))

(defun js2-auto-insert-semicolon (pn)
  (let* ((tt-flagged (js2-peek-flagged-token))
         (tt (logand tt-flagged js2-clear-ti-mask))
         (pos (js2-node-pos pn)))
      (cond
       ((= tt js2-SEMI)
        ;; Consume ';' as a part of expression
        (js2-consume-token)
        ;; extend the node bounds to include the semicolon.
        (setf (js2-node-len pn) (- js2-token-end pos)))
       ((memq tt js2-autoinsert-semi-and-warn)
        ;; Autoinsert ;
        (js2-parse-warn-missing-semi pos (js2-node-end pn)))
       (t
        (if (js2-flag-not-set-p tt-flagged js2-ti-after-eol)
            ;; Report error if no EOL or autoinsert ';' otherwise
            (js2-report-error "msg.no.semi.stmt")
          (js2-parse-warn-missing-semi pos (js2-node-end pn)))))))

(defun js2-parse-condition ()
  "Parse a parenthesized boolean expression, e.g. in an if- or while-stmt.
The parens are discarded and the expression node is returned.
The `pos' field of the return value is set to an absolute position
that must be fixed up by the caller.
Return value is a list (EXPR LP RP), with absolute paren positions."
  (let (pn lp rp)
    (if (js2-must-match js2-LP "msg.no.paren.cond")
        (setq lp js2-token-beg))
    (setq pn (js2-parse-expr))
    (if (js2-must-match js2-RP "msg.no.paren.after.cond")
        (setq rp js2-token-beg))
    ;; Report strict warning on code like "if (a = 7) ..."
    (if (and js2-strict-cond-assign-warning
             (js2-assign-node-p pn))
        (js2-add-strict-warning "msg.equal.as.assign" nil
                                (js2-node-pos pn)
                                (+ (js2-node-pos pn)
                                   (js2-node-len pn))))
    (list pn lp rp)))

(defun js2-parse-if ()
  "Parser for if-statement.  Last matched token must be js2-IF."
  (let ((pos js2-token-beg)
        cond
        if-true
        if-false
        else-pos
        end
        pn)
    (js2-consume-token)
    (setq cond (js2-parse-condition)
          if-true (js2-parse-statement)
          if-false (if (js2-match-token js2-ELSE)
                       (progn
                         (setq else-pos (- js2-token-beg pos))
                         (js2-parse-statement)))
          end (js2-node-end (or if-false if-true))
          pn (make-js2-if-node :pos pos
                               :len (- end pos)
                               :condition (car cond)
                               :then-part if-true
                               :else-part if-false
                               :else-pos else-pos
                               :lp (js2-relpos (second cond) pos)
                               :rp (js2-relpos (third cond) pos)))
    (js2-node-add-children pn (car cond) if-true if-false)
    pn))

(defun js2-parse-switch ()
  "Parser for if-statement.  Last matched token must be js2-SWITCH."
  (let ((pos js2-token-beg)
        tt
        pn
        discriminant
        has-default
        case-expr
        case-node
        case-pos
        cases
        stmt
        lp
        rp)
    (js2-consume-token)
    (if (js2-must-match js2-LP "msg.no.paren.switch")
        (setq lp js2-token-beg))
    (setq discriminant (js2-parse-expr)
          pn (make-js2-switch-node :discriminant discriminant
                                   :pos pos
                                   :lp (js2-relpos lp pos)))
    (js2-node-add-children pn discriminant)
    (js2-enter-switch pn)
    (unwind-protect
        (progn
          (if (js2-must-match js2-RP "msg.no.paren.after.switch")
              (setf (js2-switch-node-rp pn) (- js2-token-beg pos)))
          (js2-must-match js2-LC "msg.no.brace.switch")
          (catch 'break
            (while t
              (setq tt (js2-next-token)
                    case-pos js2-token-beg)
              (cond
               ((= tt js2-RC)
                (setf (js2-node-len pn) (- js2-token-end pos))
                (throw 'break nil))  ; done

               ((= tt js2-CASE)
                (setq case-expr (js2-parse-expr))
                (js2-must-match js2-COLON "msg.no.colon.case"))

               ((= tt js2-DEFAULT)
                (if has-default
                    (js2-report-error "msg.double.switch.default"))
                (setq has-default t
                      case-expr nil)
                (js2-must-match js2-COLON "msg.no.colon.case"))

               (t
                (js2-report-error "msg.bad.switch")
                (throw 'break nil)))

              (setq case-node (make-js2-case-node :pos case-pos
                                                  :len (- js2-token-end case-pos)
                                                  :expr case-expr))
              (js2-node-add-children case-node case-expr)
              (while (and (/= (setq tt (js2-peek-token)) js2-RC)
                          (/= tt js2-CASE)
                          (/= tt js2-DEFAULT)
                          (/= tt js2-EOF))
                (setf stmt (js2-parse-statement)
                      (js2-node-len case-node) (- (js2-node-end stmt) case-pos))
                (js2-block-node-push case-node stmt))
              (push case-node cases)))
          ;; add cases last, as pushing reverses the order to be correct
          (dolist (kid cases)
            (js2-node-add-children pn kid)
            (push kid (js2-switch-node-cases pn)))
          pn)  ; return value
      (js2-exit-switch))))

(defun js2-parse-while ()
  "Parser for while-statement.  Last matched token must be js2-WHILE."
  (let ((pos js2-token-beg)
        (pn (make-js2-while-node))
        cond
        body)
    (js2-consume-token)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setf cond (js2-parse-condition)
                (js2-while-node-condition pn) (car cond)
                body (js2-parse-statement)
                (js2-while-node-body pn) body
                (js2-node-len pn) (- (js2-node-end body) pos)
                (js2-while-node-lp pn) (js2-relpos (second cond) pos)
                (js2-while-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn body (car cond)))
      (js2-exit-loop))
    pn))

(defun js2-parse-do ()
  "Parser for do-statement.  Last matched token must be js2-DO."
  (let ((pos js2-token-beg)
        (pn (make-js2-do-node))
        cond
        body
        end)
    (js2-consume-token)
    (js2-enter-loop pn)
    (unwind-protect
        (progn
          (setq body (js2-parse-statement))
          (js2-must-match js2-WHILE "msg.no.while.do")
          (setf (js2-do-node-while-pos pn) (- js2-token-beg pos)
                cond (js2-parse-condition)
                (js2-do-node-condition pn) (car cond)
                (js2-do-node-body pn) body
                end js2-ts-cursor
                (js2-do-node-lp pn) (js2-relpos (second cond) pos)
                (js2-do-node-rp pn) (js2-relpos (third cond) pos))
          (js2-node-add-children pn (car cond) body))
      (js2-exit-loop))
    ;; Always auto-insert semicolon to follow SpiderMonkey:
    ;; It is required by ECMAScript but is ignored by the rest of
    ;; world; see bug 238945
    (if (js2-match-token js2-SEMI)
        (setq end js2-ts-cursor))
    (setf (js2-node-len pn) (- end pos))
    pn))

(defun js2-parse-for ()
  "Parser for for-statement.  Last matched token must be js2-FOR.
Parses for, for-in, and for each-in statements."
  (let ((for-pos js2-token-beg)
        pn
        is-for-each
        is-for-in
        in-pos
        each-pos
        tmp-pos
        init  ; Node init is also foo in 'foo in object'
        cond  ; Node cond is also object in 'foo in object'
        incr  ; 3rd section of for-loop initializer
        body
        tt
        lp
        rp)
    (js2-consume-token)
    ;; See if this is a for each () instead of just a for ()
    (when (js2-match-token js2-NAME)
      (if (string= "each" js2-ts-string)
          (progn
            (setq is-for-each t
                  each-pos (- js2-token-beg for-pos)) ; relative
            (js2-record-face 'font-lock-keyword-face))
        (js2-report-error "msg.no.paren.for")))

    (if (js2-must-match js2-LP "msg.no.paren.for")
        (setq lp (- js2-token-beg for-pos)))
    (setq tt (js2-peek-token))

    ;; parse init clause
    (let ((js2-in-for-init t))  ; set as dynamic variable
      (cond
       ((= tt js2-SEMI)
        (setq init (make-js2-empty-expr-node)))
       ((or (= tt js2-VAR) (= tt js2-LET))
        (js2-consume-token)
        (setq init (js2-parse-variables tt js2-token-beg)))
       (t
        (setq init (js2-parse-expr)))))

    (if (js2-match-token js2-IN)
        (setq is-for-in t
              in-pos (- js2-token-beg for-pos)
              cond (js2-parse-expr))  ; object over which we're iterating
      ;; else ordinary for loop - parse cond and incr
      (js2-must-match js2-SEMI "msg.no.semi.for")
      (setq cond (if (= (js2-peek-token) js2-SEMI)
                     (make-js2-empty-expr-node) ; no loop condition
                   (js2-parse-expr)))
      (js2-must-match js2-SEMI "msg.no.semi.for.cond")
      (setq tmp-pos js2-token-end
            incr (if (= (js2-peek-token) js2-RP)
                     (make-js2-empty-expr-node :pos tmp-pos)
                   (js2-parse-expr))))

    (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
        (setq rp (- js2-token-beg for-pos)))
    (if (not is-for-in)
        (setq pn (make-js2-for-node :init init
                                    :condition cond
                                    :update incr
                                    :lp lp
                                    :rp rp))
      ;; cond could be null if 'in obj' got eaten by the init node.
      (if (js2-infix-node-p init)
          ;; it was (foo in bar) instead of (var foo in bar)
          (setq cond (js2-infix-node-right init)
                init (js2-infix-node-left init))
        (if (and (js2-var-decl-node-p init)
                 (> (length (js2-var-decl-node-kids init)) 1))
            (js2-report-error "msg.mult.index")))

      (setq pn (make-js2-for-in-node :iterator init
                                     :object cond
                                     :in-pos in-pos
                                     :foreach-p is-for-each
                                     :each-pos each-pos
                                     :lp lp
                                     :rp rp)))
    (unwind-protect
        (progn
          (js2-enter-loop pn)
          ;; We have to parse the body -after- creating the loop node,
          ;; so that the loop node appears in the js2-loop-set, allowing
          ;; break/continue statements to find the enclosing loop.
          (setf body (js2-parse-statement)
                (js2-loop-node-body pn) body
                (js2-node-pos pn) for-pos
                (js2-node-len pn) (- (js2-node-end body) for-pos))
          (js2-node-add-children pn init cond incr body))
      ;; finally
      (js2-exit-loop))
    pn))

(defun js2-parse-try ()
  "Parser for try-statement.  Last matched token must be js2-TRY."
  (let ((try-pos js2-token-beg)
        try-end
        try-block
        catch-blocks
        finally-block
        saw-default-catch
        peek
        var-name
        catch-cond
        catch-node
        guard-kwd
        catch-pos
        finally-pos
        pn
        block
        lp
        rp)
    (js2-consume-token)
    (if (/= (js2-peek-token) js2-LC)
        (js2-report-error "msg.no.brace.try"))
    (setq try-block (js2-parse-statement)
          try-end (js2-node-end try-block)
          peek (js2-peek-token))
    (cond
     ((= peek js2-CATCH)
      (while (js2-match-token js2-CATCH)
        (setq catch-pos js2-token-beg
              guard-kwd nil
              catch-cond nil
              lp nil
              rp nil)
        (if saw-default-catch
            (js2-report-error "msg.catch.unreachable"))
        (if (js2-must-match js2-LP "msg.no.paren.catch")
            (setq lp (- js2-token-beg catch-pos)))

        (js2-must-match js2-NAME "msg.bad.catchcond")
        (setq var-name (js2-create-name-node))

        (if (js2-match-token js2-IF)
            (setq guard-kwd (- js2-token-beg catch-pos)
                  catch-cond (js2-parse-expr))
          (setq saw-default-catch t))

        (if (js2-must-match js2-RP "msg.bad.catchcond")
            (setq rp (- js2-token-beg catch-pos)))
        (js2-must-match js2-LC "msg.no.brace.catchblock")

        (setq block (js2-parse-statements)
              try-end (js2-node-end block)
              catch-node (make-js2-catch-node :pos catch-pos
                                              :var-name var-name
                                              :guard-expr catch-cond
                                              :guard-kwd guard-kwd
                                              :block block
                                              :lp lp
                                              :rp rp))
        (if (js2-must-match js2-RC "msg.no.brace.after.body")
            (setq try-end js2-token-beg))
        (setf (js2-node-len block) (- try-end (js2-node-pos block))
              (js2-node-len catch-node) (- try-end catch-pos))
        (js2-node-add-children catch-node var-name catch-cond block)
        (push catch-node catch-blocks)))

     ((/= peek js2-FINALLY)
      (js2-must-match js2-FINALLY "msg.try.no.catchfinally"
                      (js2-node-pos try-block)
                      (- (setq try-end (js2-node-end try-block))
                         (js2-node-pos try-block)))))

    (when (js2-match-token js2-FINALLY)
      (setq finally-pos js2-token-beg
            block (js2-parse-statement)
            try-end (js2-node-end block)
            finally-block (make-js2-finally-node :pos finally-pos
                                                 :len (- try-end finally-pos)
                                                 :body block))
      (js2-node-add-children finally-block block))

    (setq pn (make-js2-try-node :pos try-pos
                                :len (- try-end try-pos)
                                :try-block try-block
                                :finally-block finally-block))
    (js2-node-add-children pn try-block finally-block)

    ;; push them onto the try-node, which reverses and corrects their order
    (dolist (cb catch-blocks)
      (js2-node-add-children pn cb)
      (push cb (js2-try-node-catch-clauses pn)))
    pn))

(defun js2-parse-throw ()
  "Parser for throw-statement.  Last matched token must be js2-THROW."
  (let ((pos js2-token-beg)
        expr
        pn)
    (js2-consume-token)
    (if (= (js2-peek-token-or-eol) js2-EOL)
        ;; ECMAScript does not allow new lines before throw expression,
        ;; see bug 256617
        (js2-report-error "msg.bad.throw.eol"))
    (setq expr (js2-parse-expr)
          pn (make-js2-throw-node :pos pos
                                  :len (- (js2-node-end expr) pos)
                                  :expr expr))
    (js2-node-add-children pn expr)
    pn))

(defsubst js2-match-jump-label-name (label-name)
  "If break/continue specified a label, return that label's labeled stmt.
Returns the corresponding `js2-labeled-stmt-node', or if LABEL-NAME
does not match an existing label, reports an error and returns nil."
  (let ((bundle (cdr (assoc label-name js2-label-set))))
    (if (null bundle)
        (js2-report-error "msg.undef.label"))
    bundle))

(defun js2-parse-break ()
  "Parser for break-statement.  Last matched token must be js2-BREAK."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        break-target ; statement to break from
        break-label  ; in "break foo", name-node representing the foo
        labels       ; matching labeled statement to break to
        pn)
    (js2-consume-token)  ; `break'
    (when (eq (js2-peek-token-or-eol) js2-NAME)
      (js2-consume-token)
      (setq break-label (js2-create-name-node)
            end (js2-node-end break-label)
            ;; matchJumpLabelName only matches if there is one
            labels (js2-match-jump-label-name js2-ts-string)
            break-target (if labels (car (js2-labeled-stmt-node-labels labels)))))

    (unless (or break-target break-label)
      ;; no break target specified - try for innermost enclosing loop/switch
      (if (null js2-loop-and-switch-set)
          (unless break-label
            (js2-report-error "msg.bad.break" nil pos (length "break")))
        (setq break-target (car js2-loop-and-switch-set))))

    (setq pn (make-js2-break-node :pos pos
                                  :len (- end pos)
                                  :label break-label
                                  :target break-target))
    (js2-node-add-children pn break-label)  ; but not break-target
    pn))

(defun js2-parse-continue ()
  "Parser for continue-statement.  Last matched token must be js2-CONTINUE."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        label   ; optional user-specified label, a `js2-name-node'
        labels  ; current matching labeled stmt, if any
        target  ; the `js2-loop-node' target of this continue stmt
        pn)
    (js2-consume-token)  ; `continue'
    (when (= (js2-peek-token-or-eol) js2-NAME)
      (js2-consume-token)
      (setq label (js2-create-name-node)
            end (js2-node-end label)
            ;; matchJumpLabelName only matches if there is one
            labels (js2-match-jump-label-name js2-ts-string)))
    (cond
     ((null labels)  ; no current label to go to
      (if (null js2-loop-set)  ; no loop to continue to
          (js2-report-error "msg.continue.outside" nil pos
                            (length "continue"))
        (setq target (car js2-loop-set))))  ; innermost enclosing loop
     (t
      (if (js2-loop-node-p (js2-labeled-stmt-node-stmt labels))
          (setq target (js2-labeled-stmt-node-stmt labels))
        (js2-report-error "msg.continue.nonloop" nil pos (- end pos)))))

    (setq pn (make-js2-continue-node :pos pos
                                     :len (- end pos)
                                     :label label
                                     :target target))
    (js2-node-add-children pn label)  ; but not target - it's not our child
    pn))

(defun js2-parse-with ()
  "Parser for with-statement.  Last matched token must be js2-WITH."
  (js2-consume-token)
  (let ((pos js2-token-beg)
        obj body pn lp rp)

    (if (js2-must-match js2-LP "msg.no.paren.with")
        (setq lp js2-token-beg))

    (setq obj (js2-parse-expr))

    (if (js2-must-match js2-RP "msg.no.paren.after.with")
        (setq rp js2-token-beg))

    (let ((js2-nesting-of-with (1+ js2-nesting-of-with)))
        (setq body (js2-parse-statement)))

    (setq pn (make-js2-with-node :pos pos
                                 :len (- (js2-node-end body) pos)
                                 :object obj
                                 :body body
                                 :lp (js2-relpos lp pos)
                                 :rp (js2-relpos rp pos)))
    (js2-node-add-children pn obj body)
    pn))

(defun js2-parse-const-var ()
  "Parser for var- or const-statement.
Last matched token must be js2-CONST or js2-VAR."
  (let ((tt (js2-peek-token))
        (pos js2-token-beg)
        expr
        pn)
    (js2-consume-token)
    (setq expr (js2-parse-variables tt js2-token-beg)
          pn (make-js2-expr-stmt-node :pos pos
                                      :len (- (js2-node-end expr) pos)
                                      :expr expr))
    (js2-node-add-children pn expr)
    pn))

(defsubst js2-wrap-with-expr-stmt (pos expr &optional add-child)
  (let ((pn (make-js2-expr-stmt-node :pos pos
                                     :len (js2-node-len expr)
                                     :type (if (js2-inside-function)
                                               js2-EXPR_VOID
                                             js2-EXPR_RESULT)
                                     :expr expr)))
    (if add-child
        (js2-node-add-children pn expr))
    pn))

(defun js2-parse-let-stmt ()
  "Parser for let-statement.  Last matched token must be js2-LET."
  (js2-consume-token)
  (let ((pos js2-token-beg)
        expr
        pn)
    (if (= (js2-peek-token) js2-LP)
        ;; let expression in statement context
        (setq expr (js2-parse-let pos 'statement)
              pn (js2-wrap-with-expr-stmt pos expr t))
      ;; else we're looking at a statement like let x=6, y=7;
      (setf expr (js2-parse-variables js2-LET pos)
            pn (js2-wrap-with-expr-stmt pos expr t)
            (js2-node-type pn) js2-EXPR_RESULT))
    pn))

(defun js2-parse-ret-yield ()
  (js2-parse-return-or-yield (js2-peek-token) nil))

(defconst js2-parse-return-stmt-enders
  (list js2-SEMI js2-RC js2-EOF js2-EOL js2-ERROR js2-RB js2-RP js2-YIELD))

(defsubst js2-now-all-set (before after mask)
  "Return whether or not the bits in the mask have changed to all set.
BEFORE is bits before change, AFTER is bits after change, and MASK is
the mask for bits.  Returns t if all the bits in the mask are set in AFTER
but not BEFORE."
  (and (/= (logand before mask) mask)
       (= (logand after mask) mask)))

(defun js2-parse-return-or-yield (tt expr-context)
  (let ((pos js2-token-beg)
        (end js2-token-end)
        (before js2-end-flags)
        (inside-function (js2-inside-function))
        e
        ret
        name)
    (unless inside-function
      (js2-report-error (if (eq tt js2-RETURN)
                            "msg.bad.return"
                          "msg.bad.yield")))
    (js2-consume-token)
    ;; This is ugly, but we don't want to require a semicolon.
    (unless (memq (js2-peek-token-or-eol) js2-parse-return-stmt-enders)
      (setq e (js2-parse-expr)
            end (js2-node-end e)))
    (cond
     ((eq tt js2-RETURN)
      (js2-set-flag js2-end-flags (if (null e)
                                      js2-end-returns
                                    js2-end-returns-value))
      (setq ret (make-js2-return-node :pos pos
                                      :len (- end pos)
                                      :retval e))
      (js2-node-add-children ret e)
      ;; See if we need a strict mode warning.
      ;; TODO:  The analysis done by `js2-has-consistent-return-usage' is
      ;; more thorough and accurate than this before/after flag check.
      ;; E.g. if there's a finally-block that always returns, we shouldn't
      ;; show a warning generated by inconsistent returns in the catch blocks.
      ;; Basically `js2-has-consistent-return-usage' needs to keep more state,
      ;; so we know which returns/yields to highlight, and we should get rid of
      ;; all the checking in `js2-parse-return-or-yield'.
      (if (and js2-strict-inconsistent-return-warning
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-returns js2-end-returns-value)))
          (js2-add-strict-warning "msg.return.inconsistent" nil pos end)))
     (t
      (unless (js2-inside-function)
        (js2-report-error "msg.bad.yield"))
      (js2-set-flag js2-end-flags js2-end-yields)
      (setq ret (make-js2-yield-node :pos pos
                                     :len (- end pos)
                                     :value e))
      (js2-node-add-children ret e)
      (unless expr-context
        (setq e ret
              ret (js2-wrap-with-expr-stmt pos e t))
      (js2-set-requires-activation)
      (js2-set-is-generator))))

    ;; see if we are mixing yields and value returns.
    (when (and inside-function
               (js2-now-all-set before js2-end-flags
                                (logior js2-end-yields js2-end-returns-value)))
      (setq name (js2-function-name js2-current-script-or-fn))
      (if (zerop (length name))
          (js2-report-error "msg.anon.generator.returns" nil pos (- end pos))
        (js2-report-error "msg.generator.returns" name pos (- end pos))))

    ret))

(defun js2-parse-debugger ()
  (js2-consume-token)
  (make-js2-keyword-node :type js2-DEBUGGER))

(defun js2-parse-block ()
  "Parser for a curly-delimited statement block.
Last token matched must be js2-LC."
  (let ((pos js2-token-beg)
        (pn (make-js2-scope)))
    (js2-consume-token)
    (js2-push-scope pn)
    (unwind-protect
        (progn
          (js2-parse-statements pn)
          (js2-must-match js2-RC "msg.no.brace.block")
          (setf (js2-node-len pn) (- js2-token-end pos)))
      (js2-pop-scope))
    pn))

;; for js2-ERROR too, to have a node for error recovery to work on
(defun js2-parse-semi ()
  "Parse a statement or handle an error.
Last matched token is js-SEMI or js-ERROR."
  (let ((tt (js2-peek-token)) pos len)
    (js2-consume-token)
    (if (eq tt js2-SEMI)
        (make-js2-empty-expr-node :len 1)
      (setq pos js2-token-beg
            len (- js2-token-beg pos))
      (js2-report-error "msg.syntax" nil pos len)
      (make-js2-error-node :pos pos :len len))))

(defun js2-parse-default-xml-namespace ()
  "Parse a `default xml namespace = <expr>' e4x statement."
  (let ((pos js2-token-beg)
        end len expr unary es)
    (js2-consume-token)
    (js2-must-have-xml)
    (js2-set-requires-activation)
    (setq len (- js2-ts-cursor pos))
    (unless (and (js2-match-token js2-NAME)
                 (string= js2-ts-string "xml"))
      (js2-report-error "msg.bad.namespace" nil pos len))
    (unless (and (js2-match-token js2-NAME)
                 (string= js2-ts-string "namespace"))
      (js2-report-error "msg.bad.namespace" nil pos len))
    (unless (js2-match-token js2-ASSIGN)
      (js2-report-error "msg.bad.namespace" nil pos len))
    (setq expr (js2-parse-expr)
          end (js2-node-end expr)
          unary (make-js2-unary-node :type js2-DEFAULTNAMESPACE
                                     :pos pos
                                     :len (- end pos)
                                     :operand expr))
    (js2-node-add-children unary expr)
    (make-js2-expr-stmt-node :pos pos
                             :len (- end pos)
                             :expr unary)))

(defun js2-record-label (label bundle)
  ;; current token should be colon that `js2-parse-primary-expr' left untouched
  (js2-consume-token)
  (let ((name (js2-label-node-name label))
        labeled-stmt
        dup)
    (when (setq labeled-stmt (cdr (assoc name js2-label-set)))
      ;; flag both labels if possible when used in editing mode
      (if (and js2-parse-ide-mode
               (setq dup (js2-get-label-by-name labeled-stmt name)))
          (js2-report-error "msg.dup.label" nil
                            (js2-node-abs-pos dup) (js2-node-len dup)))
      (js2-report-error "msg.dup.label" nil
                        (js2-node-pos label) (js2-node-len label)))
    (js2-labeled-stmt-node-add-label bundle label)
    (js2-node-add-children bundle label)
    ;; Add one reference to the bundle per label in `js2-label-set'
    (push (cons name bundle) js2-label-set)))

(defun js2-parse-name-or-label ()
  "Parser for identifier or label.  Last token matched must be js2-NAME.
Called when we found a name in a statement context.  If it's a label, we gather
up any following labels and the next non-label statement into a
`js2-labeled-stmt-node' bundle and return that.  Otherwise we parse an
expression and return it wrapped in a `js2-expr-stmt-node'."
  (let ((pos js2-token-beg)
        (end js2-token-end)
        expr
        stmt
        pn
        bundle
        (continue t))
    ;; set check for label and call down to `js2-parse-primary-expr'
    (js2-set-check-for-label)
    (setq expr (js2-parse-expr))

    (if (/= (js2-node-type expr) js2-LABEL)
        ;; Parsed non-label expression - wrap with expression stmt.
        (setq pn (js2-wrap-with-expr-stmt pos expr t))

      ;; else parsed a label
      (setq bundle (make-js2-labeled-stmt-node :pos pos))
      (js2-record-label expr bundle)

      ;; look for more labels
      (while (and continue (= (js2-peek-token) js2-NAME))
        (js2-set-check-for-label)
        (setq expr (js2-parse-expr))
        (if (/= (js2-node-type expr) js2-LABEL)
            (progn
              (setq stmt (js2-wrap-with-expr-stmt (js2-node-pos expr) expr t)
                    continue nil)
              (js2-auto-insert-semicolon stmt))
          (js2-record-label expr bundle)))

      ;; no more labels; now parse the labeled statement
      (unwind-protect
            (unless stmt
              (let ((js2-labeled-stmt bundle))  ; bind dynamically
                (setq stmt (js2-statement-helper))))
        ;; remove the labels for this statement from the global set
        (dolist (label (js2-labeled-stmt-node-labels bundle))
          (setq js2-label-set (remove label js2-label-set))))

      (setf (js2-labeled-stmt-node-stmt bundle) stmt
            (js2-node-len bundle) (- (js2-node-end stmt) pos))
      (js2-node-add-children bundle stmt)
      bundle)))

(defun js2-parse-expr-stmt ()
  "Default parser in statement context, if no recognized statement found."
  (js2-wrap-with-expr-stmt js2-token-beg (js2-parse-expr) t))

(defun js2-parse-variables (decl-type pos)
  "Parse a comma-separated list of variable declarations.
Could be a 'var', 'const' or 'let' expression, possibly in a for-loop initializer.

DECL-TYPE is a token value: either VAR, CONST, or LET depending on context.
For 'var' or 'const', the keyword should be the token last scanned.

POS is the position where the node should start. It's sometimes the
var/const/let keyword, and other times the beginning of the first token
in the first variable declaration.

Returns the parsed `js2-var-decl-node' expression node."
  (let* ((result (make-js2-var-decl-node :decl-type decl-type
                                         :pos pos))
         destructuring
         kid-pos
         tt
         init
         name
         end
         nbeg nend
         vi
         (continue t))
    ;; Example:
    ;; var foo = {a: 1, b: 2}, bar = [3, 4];
    ;; var {b: s2, a: s1} = foo, x = 6, y, [s3, s4] = bar;
    (while continue
      (setq destructuring nil
            name nil
            tt (js2-peek-token)
            kid-pos js2-token-beg
            end js2-token-end
            init nil)
      (if (or (= tt js2-LB) (= tt js2-LC))
          ;; Destructuring assignment, e.g., var [a, b] = ...
          (setq destructuring (js2-parse-primary-expr)
                end (js2-node-end destructuring))
        ;; Simple variable name
        (when (js2-must-match js2-NAME "msg.bad.var")
          (setq name (js2-create-name-node)
                nbeg js2-token-beg
                nend js2-token-end
                end nend)
          (js2-define-symbol decl-type js2-ts-string name js2-in-for-init)))

      (when (js2-match-token js2-ASSIGN)
        (setq init (js2-parse-assign-expr)
              end (js2-node-end init))
        (if (and js2-parse-ide-mode
                 (or (js2-object-node-p init)
                     (js2-function-node-p init)))
            (js2-record-imenu-functions init name)))

      (when name
        (js2-set-face nbeg nend (if (js2-function-node-p init)
                                    'font-lock-function-name-face
                                  'font-lock-variable-name-face)
                      'record))

      (setq vi (make-js2-var-init-node :pos kid-pos
                                       :len (- end kid-pos)
                                       :type decl-type))
      (if destructuring
          (progn
            (if (and (null init) (not js2-in-for-init))
                (js2-report-error "msg.destruct.assign.no.init"))
            (setf (js2-var-init-node-target vi) destructuring))
        (setf (js2-var-init-node-target vi) name))
      (setf (js2-var-init-node-initializer vi) init)
      (js2-node-add-children vi name destructuring init)

      (js2-block-node-push result vi)
      (unless (js2-match-token js2-COMMA)
        (setq continue nil)))

    (setf (js2-node-len result) (- end pos))
    result))

(defun js2-parse-let (pos &optional stmt-p)
  "Parse a let expression or statement.
A let-expression is of the form `let (vars) expr'.
A let-statment is of the form `let (vars) {statements}'.
The third form of let is a variable declaration list, handled
by `js2-parse-variables'."
  (let ((pn (make-js2-let-node :pos pos))
        beg vars body)
    (if (js2-must-match js2-LP "msg.no.paren.after.let")
        (setf (js2-let-node-lp pn) (- js2-token-beg pos)))
    (js2-push-scope pn)
    (unwind-protect
        (progn
          (setq vars (js2-parse-variables js2-LET js2-token-beg))
          (if (js2-must-match js2-RP "msg.no.paren.let")
              (setf (js2-let-node-rp pn) (- js2-token-beg pos)))
          (if (and stmt-p (eq (js2-peek-token) js2-LC))
              ;; let statement
              (progn
                (js2-consume-token)
                (setf beg js2-token-beg  ; position stmt at LC
                      body (js2-parse-statements))
                (js2-must-match js2-RC "msg.no.curly.let")
                (setf (js2-node-len body) (- js2-token-end beg)
                      (js2-node-len pn) (- js2-token-end pos)
                      (js2-let-node-body pn) body
                      (js2-node-type pn) js2-LET))
            ;; let expression
            (setf body (js2-parse-expr)
                  (js2-node-len pn) (- (js2-node-end body) pos)
                  (js2-let-node-body pn) body))
          (js2-node-add-children pn vars body))
      (js2-pop-scope))
    pn))

(defsubst js2-define-new-symbol (decl-type name node)
  (js2-scope-put-symbol js2-current-scope
                        name
                        (make-js2-symbol decl-type name node)))

(defun js2-define-symbol (decl-type name &optional node ignore-not-in-block)
  "Define a symbol in the current scope.
If NODE is non-nil, it is the AST node associated with the symbol."
  (let* ((defining-scope (js2-get-defining-scope js2-current-scope name))
         (symbol (if defining-scope
                     (js2-scope-get-symbol defining-scope name)))
         (sdt (if symbol (js2-symbol-decl-type symbol) -1)))
    (cond
     ((and symbol ; already defined
           (or (= sdt js2-CONST) ; old version is const
               (= decl-type js2-CONST) ; new version is const
               ;; two let-bound vars in this block have same name
               (and (= sdt js2-LET)
                    (eq defining-scope js2-current-scope))))
      (js2-report-error
       (cond
        ((= sdt js2-CONST) "msg.const.redecl")
        ((= sdt js2-LET) "msg.let.redecl")
        ((= sdt js2-VAR) "msg.var.redecl")
        ((= sdt js2-FUNCTION) "msg.function.redecl")
        (t "msg.parm.redecl"))
       name))

     ((= decl-type js2-LET)
      (if (and (not ignore-not-in-block)
               (or (= (js2-node-type js2-current-scope) js2-IF)
                   (js2-loop-node-p js2-current-scope)))
          (js2-report-error "msg.let.decl.not.in.block")
        (js2-define-new-symbol decl-type name node)))

     ((or (= decl-type js2-VAR)
          (= decl-type js2-CONST)
          (= decl-type js2-FUNCTION))
      (if symbol
          (if (and js2-strict-var-redeclaration-warning (= sdt js2-VAR))
              (js2-add-strict-warning "msg.var.redecl" name)
            (if (and js2-strict-var-hides-function-arg-warning (= sdt js2-LP))
                (js2-add-strict-warning "msg.var.hides.arg" name)))
        (js2-define-new-symbol decl-type name node)))

     ((= decl-type js2-LP)
      (if symbol
          ;; must be duplicate parameter. Second parameter hides the
          ;; first, so go ahead and add the second pararameter
          (js2-report-warning "msg.dup.parms" name))
      (js2-define-new-symbol decl-type name node))

     (t (js2-code-bug)))))

(defun js2-parse-expr ()
  (let* ((pn (js2-parse-assign-expr))
         (pos (js2-node-pos pn))
         left
         right
         op-pos)
    (while (js2-match-token js2-COMMA)
      (setq op-pos (- js2-token-beg pos))  ; relative
      (if (= (js2-peek-token) js2-YIELD)
          (js2-report-error "msg.yield.parenthesized"))
      (setq right (js2-parse-assign-expr)
            left pn
            pn (make-js2-infix-node :type js2-COMMA
                                    :pos pos
                                    :len (- js2-ts-cursor pos)
                                    :op-pos op-pos
                                    :left left
                                    :right right))
      (js2-node-add-children pn left right))
    pn))

(defun js2-parse-assign-expr ()
  (let ((tt (js2-peek-token))
        (pos js2-token-beg)
        pn
        left
        right
        op-pos)
    (if (= tt js2-YIELD)
        (js2-parse-return-or-yield tt t)
      ;; not yield - parse assignment expression
      (setq pn (js2-parse-cond-expr)
            tt (js2-peek-token))
      (when (and (<= js2-first-assign tt)
                 (<= tt js2-last-assign))
        (js2-consume-token)
        (setq op-pos (- js2-token-beg pos)  ; relative
              left pn
              right (js2-parse-assign-expr)
              pn (make-js2-assign-node :type tt
                                       :pos pos
                                       :len (- (js2-node-end right) pos)
                                       :op-pos op-pos
                                       :left left
                                       :right right))
        (when js2-parse-ide-mode
          (js2-highlight-assign-targets pn left right)
          (if (or (js2-function-node-p right)
                  (js2-object-node-p right))
              (js2-record-imenu-functions right left)))
        ;; do this last so ide checks above can use absolute positions
        (js2-node-add-children pn left right))
      pn)))

(defun js2-parse-cond-expr ()
  (let ((pos js2-token-beg)
        (pn (js2-parse-or-expr))
        test-expr
        if-true
        if-false
        q-pos
        c-pos)
    (when (js2-match-token js2-HOOK)
      (setq q-pos (- js2-token-beg pos)
            if-true (js2-parse-assign-expr))
      (js2-must-match js2-COLON "msg.no.colon.cond")
      (setq c-pos (- js2-token-beg pos)
            if-false (js2-parse-assign-expr)
            test-expr pn
            pn (make-js2-cond-node :pos pos
                                   :len (- (js2-node-end if-false) pos)
                                   :test-expr test-expr
                                   :true-expr if-true
                                   :false-expr if-false
                                   :q-pos q-pos
                                   :c-pos c-pos))
      (js2-node-add-children pn test-expr if-true if-false))
    pn))

(defun js2-make-binary (type left parser)
  "Helper for constructing a binary-operator AST node.
LEFT is the left-side-expression, already parsed, and the
binary operator should have just been matched.
PARSER is a function to call to parse the right operand,
or a `js2-node' struct if it has already been parsed."
  (let* ((pos (js2-node-pos left))
         (op-pos (- js2-token-beg pos))
         (right (if (js2-node-p parser)
                    parser
                  (funcall parser)))
         (pn (make-js2-infix-node :type type
                                  :pos pos
                                  :len (- (js2-node-end right) pos)
                                  :op-pos op-pos
                                  :left left
                                  :right right)))
    (js2-node-add-children pn left right)
    pn))

(defun js2-parse-or-expr ()
  (let ((pn (js2-parse-and-expr)))
    (when (js2-match-token js2-OR)
      (setq pn (js2-make-binary js2-OR
                                pn
                                'js2-parse-or-expr)))
    pn))

(defun js2-parse-and-expr ()
  (let ((pn (js2-parse-bit-or-expr)))
    (when (js2-match-token js2-AND)
      (setq pn (js2-make-binary js2-AND
                                pn
                                'js2-parse-and-expr)))
    pn))

(defun js2-parse-bit-or-expr ()
  (let ((pn (js2-parse-bit-xor-expr)))
    (while (js2-match-token js2-BITOR)
      (setq pn (js2-make-binary js2-BITOR
                                pn
                                'js2-parse-bit-xor-expr)))
    pn))

(defun js2-parse-bit-xor-expr ()
  (let ((pn (js2-parse-bit-and-expr)))
    (while (js2-match-token js2-BITXOR)
      (setq pn (js2-make-binary js2-BITXOR
                                pn
                                'js2-parse-bit-and-expr)))
    pn))

(defun js2-parse-bit-and-expr ()
  (let ((pn (js2-parse-eq-expr)))
    (while (js2-match-token js2-BITAND)
      (setq pn (js2-make-binary js2-BITAND
                                pn
                                'js2-parse-eq-expr)))
    pn))

(defconst js2-parse-eq-ops
  (list js2-EQ js2-NE js2-SHEQ js2-SHNE))

(defun js2-parse-eq-expr ()
  (let ((pn (js2-parse-rel-expr))
        tt)
    (while (memq (setq tt (js2-peek-token)) js2-parse-eq-ops)
      (js2-consume-token)
      (setq pn (js2-make-binary tt
                                pn
                                'js2-parse-rel-expr)))
    pn))

(defconst js2-parse-rel-ops
  (list js2-IN js2-INSTANCEOF js2-LE js2-LT js2-GE js2-GT))

(defun js2-parse-rel-expr ()
  (let ((pn (js2-parse-shift-expr))
        (continue t)
        tt)
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((and js2-in-for-init (= tt js2-IN))
        (setq continue nil))
       ((memq tt js2-parse-rel-ops)
        (js2-consume-token)
        (setq pn (js2-make-binary tt pn 'js2-parse-shift-expr)))
       (t
        (setq continue nil))))
    pn))

(defconst js2-parse-shift-ops
  (list js2-LSH js2-URSH js2-RSH))

(defun js2-parse-shift-expr ()
  (let ((pn (js2-parse-add-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (memq tt js2-parse-shift-ops)
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-add-expr)))
        (setq continue nil)))
    pn))

(defun js2-parse-add-expr ()
  (let ((pn (js2-parse-mul-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (or (= tt js2-ADD) (= tt js2-SUB))
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-mul-expr)))
        (setq continue nil)))
    pn))

(defconst js2-parse-mul-ops
  (list js2-MUL js2-DIV js2-MOD))

(defun js2-parse-mul-expr ()
  (let ((pn (js2-parse-unary-expr))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (if (memq tt js2-parse-mul-ops)
          (progn
            (js2-consume-token)
            (setq pn (js2-make-binary tt pn 'js2-parse-unary-expr)))
        (setq continue nil)))
    pn))

(defsubst js2-make-unary (type parser &rest args)
  "Make a unary node of type TYPE.
PARSER is either a node (for postfix operators) or a function to call
to parse the operand (for prefix operators)."
  (let* ((pos js2-token-beg)
         (postfix (js2-node-p parser))
         (expr (if postfix
                   parser
                 (apply parser args)))
         end
         pn)
    (if postfix  ; e.g. i++
        (setq pos (js2-node-pos expr)
              end js2-token-end)
      (setq end (js2-node-end expr)))
    (setq pn (make-js2-unary-node :type type
                                  :pos pos
                                  :len (- end pos)
                                  :operand expr))
    (js2-node-add-children pn expr)
    pn))

(defconst js2-incrementable-node-types
  (list js2-NAME js2-GETPROP js2-GETELEM js2-GET_REF js2-CALL)
  "Node types that can be the operand of a ++ or -- operator.")

(defsubst js2-check-bad-inc-dec (tt beg end unary)
  (unless (memq (js2-node-type (js2-unary-node-operand unary))
                js2-incrementable-node-types)
    (js2-report-error (if (= tt js2-INC)
                          "msg.bad.incr"
                        "msg.bad.decr")
                      nil beg (- end beg))))

(defun js2-parse-unary-expr ()
  (let ((tt (js2-peek-token))
        pn expr beg end)
    (cond
     ((or (= tt js2-VOID)
          (= tt js2-NOT)
          (= tt js2-BITNOT)
          (= tt js2-TYPEOF))
      (js2-consume-token)
      (js2-make-unary tt 'js2-parse-unary-expr))

     ((= tt js2-ADD)
      (js2-consume-token)
      ;; Convert to special POS token in decompiler and parse tree
      (js2-make-unary js2-POS 'js2-parse-unary-expr))

     ((= tt js2-SUB)
      (js2-consume-token)
      ;; Convert to special NEG token in decompiler and parse tree
      (js2-make-unary js2-NEG 'js2-parse-unary-expr))

     ((or (= tt js2-INC)
          (= tt js2-DEC))
      (js2-consume-token)
      (prog1
          (setq beg js2-token-beg
                end js2-token-end
                expr (js2-make-unary tt 'js2-parse-member-expr t))
        (js2-check-bad-inc-dec tt beg end expr)))

     ((= tt js2-DELPROP)
      (js2-consume-token)
      (js2-make-unary js2-DELPROP 'js2-parse-unary-expr))

     ((= tt js2-ERROR)
      (js2-consume-token)
      (make-js2-error-node))  ; try to continue

     ((and (= tt js2-LT)
           js2-compiler-xml-available)
      ;; XML stream encountered in expression.
      (js2-consume-token)
      (js2-parse-member-expr-tail t (js2-parse-xml-initializer)))
     (t
      (setq pn (js2-parse-member-expr t)
            ;; Don't look across a newline boundary for a postfix incop.
            tt (js2-peek-token-or-eol))
      (when (or (= tt js2-INC) (= tt js2-DEC))
        (js2-consume-token)
        (setf expr pn
              pn (js2-make-unary tt expr))
        (js2-node-set-prop pn 'postfix t)
        (js2-check-bad-inc-dec tt js2-token-beg js2-token-end pn))
      pn))))

(defun js2-parse-xml-initializer ()
  "Parse an E4X XML initializer.
I'm parsing it the way Rhino parses it, but without the tree-rewriting.
Then I'll postprocess the result, depending on whether we're in IDE
mode or codegen mode, and generate the appropriate rewritten AST.
IDE mode uses a rich AST that models the XML structure.  Codegen mode
just concatenates everything and makes a new XML or XMLList out of it."
  (let ((tt (js2-get-first-xml-token))
        pn-xml
        pn
        expr
        kids
        expr-pos
        (continue t)
        (first-token t))
    (when (not (or (= tt js2-XML) (= tt js2-XMLEND)))
      (js2-report-error "msg.syntax"))
    (setq pn-xml (make-js2-xml-node))
    (while continue
      (if first-token
          (setq first-token nil)
        (setq tt (js2-get-next-xml-token)))
      (cond
       ;; js2-XML means we found a {expr} in the XML stream.
       ;; The js2-ts-string is the XML up to the left-curly.
       ((= tt js2-XML)
        (push (make-js2-string-node :pos js2-token-beg
                                    :len (- js2-ts-cursor js2-token-beg))
              kids)
        (js2-must-match js2-LC "msg.syntax")
        (setq expr-pos js2-ts-cursor
              expr (if (eq (js2-peek-token) js2-RC)
                       (make-js2-empty-expr-node :pos expr-pos)
                     (js2-parse-expr)))
        (js2-must-match js2-RC "msg.syntax")
        (setq pn (make-js2-xml-js-expr-node :pos (js2-node-pos expr)
                                            :len (js2-node-len expr)
                                            :expr expr))
        (js2-node-add-children pn expr)
        (push pn kids))

       ;; a js2-XMLEND token means we hit the final close-tag.
       ((= tt js2-XMLEND)
        (push (make-js2-string-node :pos js2-token-beg
                                    :len (- js2-ts-cursor js2-token-beg))
              kids)
        (dolist (kid (nreverse kids))
          (js2-block-node-push pn-xml kid))
        (setf (js2-node-len pn-xml) (- js2-ts-cursor
                                       (js2-node-pos pn-xml))
              continue nil))
       (t
        (js2-report-error "msg.syntax")
        (setq continue nil))))
    pn-xml))


(defun js2-parse-argument-list ()
  "Parse an argument list and return it as a lisp list of nodes.
Returns the list in reverse order.  Consumes the right-paren token."
  (let (result)
    (unless (js2-match-token js2-RP)
      (loop do
            (if (= (js2-peek-token) js2-YIELD)
                (js2-report-error "msg.yield.parenthesized"))
            (push (js2-parse-assign-expr) result)
            while
            (js2-match-token js2-COMMA))
      (js2-must-match js2-RP "msg.no.paren.arg")
      result)))

(defun js2-parse-member-expr (&optional allow-call-syntax)
  (let ((tt (js2-peek-token))
        pn
        pos
        target
        args
        beg
        end
        init
        tail)
    (if (/= tt js2-NEW)
        (setq pn (js2-parse-primary-expr))
      ;; parse a 'new' expression
      (js2-consume-token)
      (setq pos js2-token-beg
            beg pos
            target (js2-parse-member-expr)
            end (js2-node-end target)
            pn (make-js2-new-node :pos pos
                                  :target target
                                  :len (- end pos)))
      (js2-node-add-children pn target)
      (when (js2-match-token js2-LP)
        ;; Add the arguments to pn, if any are supplied.
        (setf beg pos  ; start of "new" keyword
              pos js2-token-beg
              args (nreverse (js2-parse-argument-list))
              (js2-new-node-args pn) args
              end js2-token-end
              (js2-new-node-lp pn) (- pos beg)
              (js2-new-node-rp pn) (- end 1 beg))
        (apply #'js2-node-add-children pn args))

      (when (and js2-allow-rhino-new-expr-initializer
                 (js2-match-token js2-LC))
        (setf init (js2-parse-object-literal)
              end (js2-node-end init)
              (js2-new-node-initializer pn) init)
        (js2-node-add-children pn init))

        (setf (js2-node-len pn) (- beg pos)))  ; end outer if

    (js2-parse-member-expr-tail allow-call-syntax pn)))

(defun js2-parse-member-expr-tail (allow-call-syntax pn)
  "Parse a chain of property/array accesses or function calls.
Includes parsing for E4X operators like `..' and `.@'.
If ALLOW-CALL-SYNTAX is nil, stops when we encounter a left-paren.
Returns an expression tree that includes PN, the parent node."
  (let ((beg (js2-node-pos pn))
        tt
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ((or (= tt js2-DOT) (= tt js2-DOTDOT))
        (setq pn (js2-parse-property-access tt pn)))

       ((= tt js2-DOTQUERY)
        (setq pn (js2-parse-dot-query pn)))

       ((= tt js2-LB)
        (setq pn (js2-parse-element-get pn)))

       ((= tt js2-LP)
        (if allow-call-syntax
            (setq pn (js2-parse-function-call pn))
          (setq continue nil)))
       (t
        (setq continue nil))))
    (if (>= js2-highlight-level 2)
        (js2-parse-highlight-member-expr-node pn))
    pn))

(defun js2-parse-dot-query (pn)
  "Parse a dot-query expression, e.g. foo.bar.(@name == 2)
Last token parsed must be `js2-DOTQUERY'."
  (let ((pos (js2-node-pos pn))
        op-pos
        expr
        end)
    (js2-consume-token)
    (js2-must-have-xml)
    (js2-set-requires-activation)
    (setq op-pos js2-token-beg
          expr (js2-parse-expr)
          end (js2-node-end expr)
          pn (make-js2-xml-dot-query-node :left pn
                                          :pos pos
                                          :op-pos op-pos
                                          :right expr))
    (js2-node-add-children pn
                           (js2-xml-dot-query-node-left pn)
                           (js2-xml-dot-query-node-right pn))
    (if (js2-must-match js2-RP "msg.no.paren")
        (setf (js2-xml-dot-query-node-rp pn) js2-token-beg
              end js2-token-end))
    (setf (js2-node-len pn) (- end pos))
    pn))

(defun js2-parse-element-get (pn)
  "Parse an element-get expression, e.g. foo[bar].
Last token parsed must be `js2-RB'."
  (let ((lb js2-token-beg)
        (pos (js2-node-pos pn))
        rb
        expr)
    (js2-consume-token)
    (setq expr (js2-parse-expr))
    (if (js2-must-match js2-RB "msg.no.bracket.index")
        (setq rb js2-token-beg))
    (setq pn (make-js2-elem-get-node :target pn
                                     :pos pos
                                     :element expr
                                     :lb (js2-relpos lb pos)
                                     :rb (js2-relpos rb pos)
                                     :len (- js2-token-end pos)))
    (js2-node-add-children pn
                           (js2-elem-get-node-target pn)
                           (js2-elem-get-node-element pn))
    pn))

(defun js2-parse-function-call (pn)
  (let (args
        (pos (js2-node-pos pn)))
    (js2-consume-token)
    (setq pn (make-js2-call-node :pos pos
                                 :target pn
                                 :lp (- js2-token-beg pos)))
    (js2-node-add-children pn (js2-call-node-target pn))

    ;; Add the arguments to pn, if any are supplied.
    (setf args (nreverse (js2-parse-argument-list))
          (js2-call-node-rp pn) (- js2-token-beg pos)
          (js2-call-node-args pn) args)
    (apply #'js2-node-add-children pn args)

    (setf (js2-node-len pn) (- js2-ts-cursor pos))
    pn))

(defun js2-parse-property-access (tt pn)
  "Parse a property access, XML descendants access, or XML attr access."
  (let ((member-type-flags 0)
        (dot-pos js2-token-beg)
        (dot-len (if (= tt js2-DOTDOT) 2 1))
        name
        ref  ; right side of . or .. operator
        result)
    (js2-consume-token)
    (when (= tt js2-DOTDOT)
      (js2-must-have-xml)
      (setq member-type-flags js2-descendants-flag))
    (if (not js2-compiler-xml-available)
        (progn
          (js2-must-match-prop-name "msg.no.name.after.dot")
          (setq name (js2-create-name-node t js2-GETPROP)
                result (make-js2-prop-get-node :left pn
                                               :pos js2-token-beg
                                               :right name
                                               :len (- js2-token-end
                                                       js2-token-beg)))
          (js2-node-add-children result pn name)
          result)
      ;; otherwise look for XML operators
      (setf result (if (= tt js2-DOT)
                       (make-js2-prop-get-node)
                     (make-js2-infix-node :type js2-DOTDOT))
            (js2-node-pos result) (js2-node-pos pn)
            (js2-infix-node-op-pos result) dot-pos
            (js2-infix-node-left result) pn  ; do this after setting position
            tt (js2-next-token))
      (cond
       ;; needed for generator.throw()
       ((= tt js2-THROW)
        (js2-save-name-token-data js2-token-beg "throw")
        (setq ref (js2-parse-property-name nil js2-ts-string member-type-flags)))

       ;; handles: name, ns::name, ns::*, ns::[expr]
       ((js2-valid-prop-name-token tt)
        (setq ref (js2-parse-property-name -1 js2-ts-string member-type-flags)))

       ;; handles: *, *::name, *::*, *::[expr]
       ((= tt js2-MUL)
        (js2-save-name-token-data js2-token-beg "*")
        (setq ref (js2-parse-property-name nil "*" member-type-flags)))

       ;; handles: '@attr', '@ns::attr', '@ns::*', '@ns::[expr]', etc.
       ((= tt js2-XMLATTR)
        (setq result (js2-parse-attribute-access)))

       (t
        (js2-report-error "msg.no.name.after.dot" nil dot-pos dot-len)))

      (if ref
          (setf (js2-node-len result) (- (js2-node-end ref)
                                         (js2-node-pos result))
                (js2-infix-node-right result) ref))
      (if (js2-infix-node-p result)
          (js2-node-add-children result
                                 (js2-infix-node-left result)
                                 (js2-infix-node-right result)))
      result)))

(defun js2-parse-attribute-access ()
  "Parse an E4X XML attribute expression.
This includes expressions of the forms:

  @attr      @ns::attr     @ns::*
  @*         @*::attr      @*::*
  @[expr]    @*::[expr]    @ns::[expr]

Called if we peeked an '@' token."
  (let ((tt (js2-next-token))
        (at-pos js2-token-beg))
    (cond
     ;; handles: @name, @ns::name, @ns::*, @ns::[expr]
     ((js2-valid-prop-name-token tt)
      (js2-parse-property-name at-pos js2-ts-string 0))

     ;; handles: @*, @*::name, @*::*, @*::[expr]
     ((= tt js2-MUL)
      (js2-save-name-token-data js2-token-beg "*")
      (js2-parse-property-name js2-token-beg "*" 0))

     ;; handles @[expr]
     ((= tt js2-LB)
      (js2-parse-xml-elem-ref at-pos))

     (t
      (js2-report-error "msg.no.name.after.xmlAttr")
      ;; Avoid cascaded errors that happen if we make an error node here.
      (js2-save-name-token-data js2-token-beg "")
      (js2-parse-property-name js2-token-beg "" 0)))))

(defun js2-parse-property-name (at-pos s member-type-flags)
  "Check if :: follows name in which case it becomes qualified name.

AT-POS is a natural number if we just read an '@' token, else nil.
S is the name or string that was matched:  an identifier, 'throw' or '*'.
MEMBER-TYPE-FLAGS is a bit set tracking whether we're a '.' or '..' child.

Returns a `js2-xml-ref-node' if it's an attribute access, a child of a '..'
operator, or the name is followed by ::.  For a plain name, returns a
`js2-name-node'.  Returns a `js2-error-node' for malformed XML expressions."
  (let ((pos (or at-pos js2-token-beg))
        colon-pos
        (name (js2-create-name-node t js2-current-token))
        ns
        tt
        ref
        pn)
    (catch 'return
      (when (js2-match-token js2-COLONCOLON)
        (setq ns name
              colon-pos js2-token-beg
              tt (js2-next-token))
        (cond
         ;; handles name::name
         ((js2-valid-prop-name-token tt)
          (setq name (js2-create-name-node)))

         ;; handles name::*
         ((= tt js2-MUL)
          (js2-save-name-token-data js2-token-beg "*")
          (setq name (js2-create-name-node)))

         ;; handles name::[expr]
         ((= tt js2-LB)
          (throw 'return (js2-parse-xml-elem-ref at-pos ns colon-pos)))

         (t
          (js2-report-error "msg.no.name.after.coloncolon"))))

      (if (and (null ns) (zerop member-type-flags))
          name
        (prog1
            (setq pn
                  (make-js2-xml-prop-ref-node :pos pos
                                              :len (- (js2-node-end name) pos)
                                              :at-pos at-pos
                                              :colon-pos colon-pos
                                              :propname name))
          (js2-node-add-children pn name))))))

(defun js2-parse-xml-elem-ref (at-pos &optional namespace colon-pos)
  "Parse the [expr] portion of an xml element reference.
For instance, @[expr], @*::[expr], or ns::[expr]."
  (let* ((lb js2-token-beg)
         (pos (or at-pos lb))
         rb
         (expr (js2-parse-expr))
         (end (js2-node-end expr))
         pn)
    (if (js2-must-match js2-RB "msg.no.bracket.index")
        (setq rb js2-token-beg
              end js2-token-end))
    (prog1
        (setq pn
              (make-js2-xml-elem-ref-node :pos pos
                                          :len (- end pos)
                                          :namespace namespace
                                          :colon-pos colon-pos
                                          :at-pos at-pos
                                          :expr expr
                                          :lb (js2-relpos lb pos)
                                          :rb (js2-relpos rb pos)))
      (js2-node-add-children pn namespace expr))))

(defun js2-parse-primary-expr ()
  "Parses a literal (leaf) expression of some sort.
Includes complex literals such as functions, object-literals,
array-literals, array comprehensions and regular expressions."
  (let ((tt-flagged (js2-next-flagged-token))
        pn      ; parent node  (usually return value)
        tt
        px-pos  ; paren-expr pos
        len
        flags   ; regexp flags
        expr)
    (setq tt js2-current-token)
    (cond
     ((= tt js2-FUNCTION)
      (js2-parse-function 'FUNCTION_EXPRESSION))

     ((= tt js2-LB)
      (js2-parse-array-literal))

     ((= tt js2-LC)
      (js2-parse-object-literal))

     ((= tt js2-LET)
      (js2-parse-let js2-token-beg))

     ((= tt js2-LP)
      (setq px-pos js2-token-beg
            expr (js2-parse-expr))
      (js2-must-match js2-RP "msg.no.paren")
      (setq pn (make-js2-paren-node :pos px-pos
                                    :expr expr
                                    :len (- js2-token-end px-pos)))
      (js2-node-add-children pn (js2-paren-node-expr pn))
      pn)

     ((= tt js2-XMLATTR)
      (js2-must-have-xml)
      (js2-parse-attribute-access))

     ((= tt js2-NAME)
      (js2-parse-name tt-flagged tt))

     ((= tt js2-NUMBER)
      (make-js2-number-node))

     ((= tt js2-STRING)
      (prog1
          (make-js2-string-node)
        (js2-record-face 'font-lock-string-face)))

     ((or (= tt js2-DIV) (= tt js2-ASSIGN_DIV))
      ;; Got / or /= which in this context means a regexp literal
      (setq px-pos js2-token-beg)
      (js2-read-regexp tt)
      (setq flags js2-ts-regexp-flags
            js2-ts-regexp-flags nil)
      (prog1
          (make-js2-regexp-node :pos px-pos
                                :len (- js2-ts-cursor px-pos)
                                :value js2-ts-string
                                :flags flags)
        (js2-set-face px-pos js2-ts-cursor 'font-lock-string-face 'record)))

     ((or (= tt js2-NULL)
          (= tt js2-THIS)
          (= tt js2-FALSE)
          (= tt js2-TRUE))
      (make-js2-keyword-node :type tt))

     ((= tt js2-RESERVED)
      (js2-report-error "msg.reserved.id")
      (make-js2-name-node))

     ((= tt js2-ERROR)
      ;; the scanner or one of its subroutines reported the error.
      (make-js2-error-node))

     ((= tt js2-EOF)
      (setq px-pos (point-at-bol)
            len (- js2-ts-cursor px-pos))
      (js2-report-error "msg.unexpected.eof" nil px-pos len)
      (make-js2-error-node :pos px-pos :len len))

     (t
      (js2-report-error "msg.syntax")
      (make-js2-error-node)))))

(defun js2-parse-name (tt-flagged tt)
  (let ((name js2-ts-string)
        (name-pos js2-token-beg))
      (if (and (js2-flag-set-p tt-flagged js2-ti-check-label)
               (= (js2-peek-token) js2-COLON))
          (prog1
            ;; Do not consume colon, it is used as unwind indicator
            ;; to return to statementHelper.
            (make-js2-label-node :pos name-pos
                                 :len (- js2-token-end name-pos)
                                 :name name)
            (js2-set-face name-pos
                          js2-token-end
                          'font-lock-variable-name-face 'record))
        ;; Otherwise not a label, just a name.  Unfortunately peeking
        ;; the next token to check for a colon has biffed js2-token-beg
        ;; and js2-token-end.  We store the name's bounds in buffer vars
        ;; and `js2-create-name-node' uses them.
        (js2-save-name-token-data name-pos name)
        (if js2-compiler-xml-available
            (js2-parse-property-name nil name 0)
          (js2-create-name-node 'check-activation)))))

(defsubst js2-parse-warn-trailing-comma (msg pos elems comma-pos)
  (js2-add-strict-warning
   msg nil
   ;; back up from comma to beginning of line or array/objlit
   (max (if elems
            (js2-node-pos (car elems))
          pos)
        (save-excursion
          (goto-char comma-pos)
          (back-to-indentation)
          (point)))
   comma-pos))

(defun js2-parse-array-literal ()
  (let ((pos js2-token-beg)
        (end js2-token-end)
        (after-lb-or-comma t)
        after-comma
        tt
        elems
        pn
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ;; comma
       ((= tt js2-COMMA)
        (js2-consume-token)
        (setq after-comma js2-token-end)
        (if (not after-lb-or-comma)
            (setq after-lb-or-comma t)
          (push nil elems)))

       ;; end of array
       ((or (= tt js2-RB)
            (= tt js2-EOF))  ; prevent infinite loop
        (if (= tt js2-EOF)
            (js2-report-error "msg.no.bracket.arg" nil pos)
          (js2-consume-token))
        (setq continue nil
              end js2-token-end
              pn (make-js2-array-node :pos pos
                                      :len (- js2-ts-cursor pos)
                                      :elems (nreverse elems)))
        (apply #'js2-node-add-children pn (js2-array-node-elems pn))
        (when after-comma
          (js2-parse-warn-trailing-comma "msg.array.trailing.comma"
                                         pos elems after-comma)))

       ;; array comp
       ((and (>= js2-language-version 170)
             (= tt js2-FOR)          ; check for array comprehension
             (not after-lb-or-comma) ; "for" can't follow a comma
             elems                   ; must have at least 1 element
             (not (cdr elems)))      ; but no 2nd element
        (setf continue nil
              pn (js2-parse-array-comprehension (car elems) pos)))

       ;; another element
       (t
        (unless after-lb-or-comma
          (js2-report-error "msg.no.bracket.arg"))
        (push (js2-parse-assign-expr) elems)
        (setq after-lb-or-comma nil
              after-comma nil))))
    pn))

(defun js2-parse-array-comprehension (expr pos)
  "Parse a JavaScript 1.7 Array Comprehension.
EXPR is the first expression after the opening left-bracket.
POS is the beginning of the LB token preceding EXPR.
We should have just parsed the 'for' keyword before calling this function."
  (let (loops
        filter
        if-pos
        result)
    (while (= (js2-peek-token) js2-FOR)
      (push (js2-parse-array-comp-loop) loops))
    (when (= (js2-peek-token) js2-IF)
      (js2-consume-token)
      (setq if-pos (- js2-token-beg pos)  ; relative
            filter (js2-parse-condition)))
    (js2-must-match js2-RB "msg.no.bracket.arg" pos)
    (setq result (make-js2-array-comp-node :pos pos
                                           :len (- js2-ts-cursor pos)
                                           :result expr
                                           :loops (nreverse loops)
                                           :filter (car filter)
                                           :lp (js2-relpos (second filter) pos)
                                           :rp (js2-relpos (third filter) pos)
                                           :if-pos if-pos))
    (apply #'js2-node-add-children result expr (car filter)
           (js2-array-comp-node-loops result))
    result))

(defun js2-parse-array-comp-loop ()
  "Parse a 'for [each] (foo in bar)' expression in an Array comprehension.
Last token peeked should be the initial FOR."
  (let ((pos js2-token-beg)
        (pn (make-js2-array-comp-loop-node))
        tt
        iter
        obj
        foreach-p
        in-pos
        each-pos
        lp
        rp)
    (assert (= (js2-next-token) js2-FOR))  ; consumes token
    (js2-push-scope pn)
    (unwind-protect
        (progn
          (when (js2-match-token js2-NAME)
            (if (string= js2-ts-string "each")
                (progn
                  (setq foreach-p t
                        each-pos (- js2-token-beg pos)) ; relative
                  (js2-record-face 'font-lock-keyword-face))
              (js2-report-error "msg.no.paren.for")))

          (if (js2-must-match js2-LP "msg.no.paren.for")
              (setq lp (- js2-token-beg pos)))

          (setq tt (js2-peek-token))
          (cond
           ((or (= tt js2-LB)
                (= tt js2-LC))
            ;; handle destructuring assignment
            (setq iter (js2-parse-primary-expr)))

           ((js2-valid-prop-name-token tt)
            (js2-consume-token)
            (setq iter (js2-create-name-node)))

           (t
            (js2-report-error "msg.bad.var")))

          ;; Define as a let since we want the scope of the variable to
          ;; be restricted to the array comprehension
          (if (js2-name-node-p iter)
              (js2-define-symbol js2-LET (js2-name-node-name iter) pn t))

          (if (js2-must-match js2-IN "msg.in.after.for.name")
              (setq in-pos (- js2-token-beg pos)))

          (setq obj (js2-parse-expr))
          (if (js2-must-match js2-RP "msg.no.paren.for.ctrl")
              (setq rp (- js2-token-beg pos)))

          (setf (js2-node-pos pn) pos
                (js2-node-len pn) (- js2-ts-cursor pos)
                (js2-array-comp-loop-node-iterator pn) iter
                (js2-array-comp-loop-node-object pn) obj
                (js2-array-comp-loop-node-in-pos pn) in-pos
                (js2-array-comp-loop-node-each-pos pn) each-pos
                (js2-array-comp-loop-node-foreach-p pn) foreach-p
                (js2-array-comp-loop-node-lp pn) lp
                (js2-array-comp-loop-node-rp pn) rp)
          (js2-node-add-children pn iter obj))
      (js2-pop-scope))
    pn))

(defun js2-parse-object-literal ()
  (let ((pos js2-token-beg)
        tt
        elems
        result
        after-comma
        (continue t))
    (while continue
      (setq tt (js2-peek-token))
      (cond
       ;; {foo: ...}, {'foo': ...}, {get foo() {...}}, or {set foo(x) {...}}
       ((or (js2-valid-prop-name-token tt)
            (= tt js2-STRING))
        (setq after-comma nil
              result (js2-parse-named-prop tt))
        (if (and (null result)
                 (not js2-recover-from-parse-errors))
            (setq continue nil)
          (push result elems)))

       ;; {12: x} or {10.7: x}
       ((= tt js2-NUMBER)
        (js2-consume-token)
        (setq after-comma nil)
        (push (js2-parse-plain-property (make-js2-number-node)) elems))

       ;; trailing comma
       ((= tt js2-RC)
        (setq continue nil)
        (if after-comma
            (js2-parse-warn-trailing-comma "msg.extra.trailing.comma"
                                           pos elems after-comma)))
       (t
        (js2-report-error "msg.bad.prop")
        (unless js2-recover-from-parse-errors
          (setq continue nil))))         ; end switch

      (if (js2-match-token js2-COMMA)
          (setq after-comma js2-token-end)
        (setq continue nil)))           ; end loop

    (js2-must-match js2-RC "msg.no.brace.prop")
    (setq result (make-js2-object-node :pos pos
                                       :len (- js2-ts-cursor pos)
                                       :elems (nreverse elems)))
    (apply #'js2-node-add-children result (js2-object-node-elems result))
    result))

(defun js2-parse-named-prop (tt)
  "Parse a name, string, or getter/setter object property."
  (js2-consume-token)
  (let ((string-prop (and (= tt js2-STRING)
                          (make-js2-string-node)))
        expr
        (ppos js2-token-beg)
        (pend js2-token-end)
        (name (js2-create-name-node))
        (prop js2-ts-string))

    (if (and (= tt js2-NAME)
             (= (js2-peek-token) js2-NAME)
             (or (string= prop "get")
                 (string= prop "set")))
        (progn
          ;; getter/setter prop
          (js2-consume-token)
          (js2-set-face ppos pend 'font-lock-keyword-face 'record)  ; get/set
          (js2-record-face 'font-lock-function-name-face)      ; for peeked name
          (setq name (js2-create-name-node)) ; discard get/set & use peeked name
          (js2-parse-getter-setter-prop ppos name (string= prop "get")))

      ;; regular prop
      (prog1
          (setq expr (js2-parse-plain-property (or string-prop name)))
        (js2-set-face ppos pend
                      (if (js2-function-node-p
                           (js2-object-prop-node-right expr))
                          'font-lock-function-name-face
                        'font-lock-variable-name-face)
                      'record)))))

(defun js2-parse-plain-property (prop)
  "Parse a non-getter/setter property in an object literal.
PROP is the node representing the property:  a number, name or string."
  (js2-must-match js2-COLON "msg.no.colon.prop")
  (let* ((pos (js2-node-pos prop))
        (colon (- js2-token-beg pos))
        (expr (js2-parse-assign-expr))
        (result (make-js2-object-prop-node
                 :pos pos
                 ;; don't include last consumed token in length
                 :len (- (+ (js2-node-pos expr)
                            (js2-node-len expr))
                         pos)
                 :left prop
                 :right expr
                 :op-pos colon)))
    (js2-node-add-children result prop expr)
    result))

(defun js2-parse-getter-setter-prop (pos prop get-p)
  "Parse getter or setter property in an object literal.
JavaScript syntax is:

  { get foo() {...}, set foo(x) {...} }

POS is the start position of the `get' or `set' keyword.
PROP is the `js2-name-node' representing the property name.
GET-P is non-nil if the keyword was `get'."
  (let ((type (if get-p js2-GET js2-SET))
        result
        end
        (fn (js2-parse-function 'FUNCTION_EXPRESSION)))

    ;; it has to be an anonymous function, as we already parsed the name
    (if (/= (js2-node-type fn) js2-FUNCTION)
        (js2-report-error "msg.bad.prop")
      (if (plusp (length (js2-function-name fn)))
          (js2-report-error "msg.bad.prop")))

    (js2-node-set-prop fn 'GETTER_SETTER type)  ; for codegen
    (setq end (js2-node-end fn)
          result (make-js2-getter-setter-node :type type
                                              :pos pos
                                              :len (- end pos)
                                              :left prop
                                              :right fn))
    (js2-node-add-children result prop fn)
    result))

(defun js2-create-name-node (&optional check-activation-p token)
  "Create a name node using the token info from last scanned name.
In some cases we need to either synthesize a name node, or we lost
the name token information by peeking.  If the TOKEN parameter is
not `js2-NAME', then we use the token info saved in instance vars."
  (let ((beg js2-token-beg)
        (s js2-ts-string)
        name)
    (when (/= js2-current-token js2-NAME)
      (setq beg (or js2-prev-name-token-start js2-ts-cursor)
            s js2-prev-name-token-string
            js2-prev-name-token-start nil
            js2-prev-name-token-string nil))
    (setq name (make-js2-name-node :pos beg
                                   :name s
                                   :len (length s)))
    (if check-activation-p
        (js2-check-activation-name s (or token js2-NAME)))
    name))

(provide 'js2-parse)

;;; js2-parse.el ends here
