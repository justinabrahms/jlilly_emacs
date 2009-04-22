;;; js2-scan.el --- JavaScript scanner

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

;;; Commentary:

;; A port of Mozilla Rhino's scanner.
;; Corresponds to Rhino files Token.java and TokenStream.java.

;;; Code:

(require 'js2-util)

(eval-when-compile
  (require 'cl))

(defvar js2-tokens nil
  "List of all defined token names.")  ; intialized below

(defvar js2-token-names
  (let* ((names (make-vector js2-num-tokens -1))
         (case-fold-search nil)  ; only match js2-UPPER_CASE
         (syms (apropos-internal "^js2-\\(?:[A-Z_]+\\)")))
    (loop for sym in syms
          for i from 0
          do
          (unless (or (memq sym '(js2-EOF_CHAR js2-ERROR))
                      (not (boundp sym)))
            (aset names (symbol-value sym)         ; code, e.g. 152
                  (substring (symbol-name sym) 4)) ; name, e.g. "LET"
            (push sym js2-tokens)))
    names)
  "Vector mapping int values to token string names, sans `js2-' prefix.")

(defun js2-token-name (tok)
  "Return a string name for TOK, a token symbol or code.
Signals an error if it's not a recognized token."
  (let ((code tok))
    (if (symbolp tok)
        (setq code (symbol-value tok)))
    (if (eq code -1)
        "ERROR"
      (if (and (numberp code)
               (not (minusp code))
               (< code js2-num-tokens))
          (aref js2-token-names code)
        (error "Invalid token: %s" code)))))

(defsubst js2-token-sym (tok)
  "Return symbol for TOK given its code, e.g. 'js2-LP for code 86."
  (intern (js2-token-name tok)))

(defvar js2-token-codes
  (let ((table (make-hash-table :test 'eq :size 256)))
    (loop for name across js2-token-names
          for sym = (intern (concat "js2-" name))
          do
          (puthash sym (symbol-value sym) table))
    ;; clean up a few that are "wrong" in Rhino's token codes
    (puthash 'js2-DELETE js2-DELPROP table)
    table)
  "Hashtable mapping token symbols to their bytecodes.")

(defsubst js2-token-code (sym)
  "Return code for token symbol SYM, e.g. 86 for 'js2-LP."
  (or (gethash sym js2-token-codes)
      (error "Invalid token symbol: %s " sym)))  ; signal code bug

(defsubst js2-report-scan-error (msg &optional no-throw beg len)
  (setq js2-token-end js2-ts-cursor)
  (js2-report-error msg nil
                    (or beg js2-token-beg)
                    (or len (- js2-token-end js2-token-beg)))
  (unless no-throw
    (throw 'return js2-ERROR)))

(defsubst js2-get-string-from-buffer ()
  "Reverse the char accumulator and return it as a string."
  (setq js2-token-end js2-ts-cursor)
  (if js2-ts-string-buffer
      (apply #'string (nreverse js2-ts-string-buffer))
    ""))

;; TODO:  could potentially avoid a lot of consing by allocating a
;; char buffer the way Rhino does.
(defsubst js2-add-to-string (c)
  (push c js2-ts-string-buffer))

;; Note that when we "read" the end-of-file, we advance js2-ts-cursor
;; to (1+ (point-max)), which lets the scanner treat end-of-file like
;; any other character:  when it's not part of the current token, we
;; unget it, allowing it to be read again by the following call.
(defsubst js2-unget-char ()
  (decf js2-ts-cursor))

;; Rhino distinguishes \r and \n line endings.  We don't need to
;; because we only scan from Emacs buffers, which always use \n.
(defsubst js2-get-char ()
  "Read and return the next character from the input buffer.
Increments `js2-ts-lineno' if the return value is a newline char.
Updates `js2-ts-cursor' to the point after the returned char.
Returns `js2-EOF_CHAR' if we hit the end of the buffer.
Also updates `js2-ts-hit-eof' and `js2-ts-line-start' as needed."
  (let (c)
    ;; check for end of buffer
    (if (>= js2-ts-cursor (point-max))
        (setq js2-ts-hit-eof t
              js2-ts-cursor (1+ js2-ts-cursor)
              c js2-EOF_CHAR)  ; return value

      ;; otherwise read next char
      (setq c (char-before (incf js2-ts-cursor)))

      ;; if we read a newline, update counters
      (if (= c ?\n)
          (setq js2-ts-line-start js2-ts-cursor
                js2-ts-lineno (1+ js2-ts-lineno)))

      ;; TODO:  skip over format characters
      c)))

(defsubst js2-read-unicode-escape ()
  "Read a \\uNNNN sequence from the input.
Assumes the ?\ and ?u have already been read.
Returns the unicode character, or nil if it wasn't a valid character.
Doesn't change the values of any scanner variables."
  ;; I really wish I knew a better way to do this, but I can't
  ;; find the Emacs function that takes a 16-bit int and converts
  ;; it to a Unicode/utf-8 character.  So I basically eval it with (read).
  ;; Have to first check that it's 4 hex characters or it may stop
  ;; the read early.
  (ignore-errors
    (let ((s (buffer-substring-no-properties js2-ts-cursor
                                             (+ 4 js2-ts-cursor))))
      (if (string-match "[a-zA-Z0-9]\\{4\\}" s)
          (read (concat "?\\u" s))))))

(defsubst js2-match-char (test)
  "Consume and return next character if it matches TEST, a character.
Returns nil and consumes nothing if TEST is not the next character."
  (let ((c (js2-get-char)))
    (if (eq c test)
        t
      (js2-unget-char)
      nil)))

(defsubst js2-peek-char ()
  (prog1
      (js2-get-char)
    (js2-unget-char)))

(defsubst js2-java-identifier-start-p (c)
  (or
   (memq c '(?$ ?_))
   (char-is-uppercase c)
   (char-is-lowercase c)))

(defsubst js2-java-identifier-part-p (c)
  "Implementation of java.lang.Character.isJavaIdentifierPart()"
  ;; TODO:  make me Unicode-friendly.  See comments above.
  (or
   (memq c '(?$ ?_))
   (char-is-uppercase c)
   (char-is-lowercase c)
   (and (>= c ?0) (<= c ?9))))

(defsubst js2-alpha-p (c)
  ;; Use 'Z' < 'a'
  (if (<= c ?Z)
      (<= ?A c)
    (and (<= ?a c)
         (<= c ?z))))

(defsubst js2-digit-p (c)
  (and (<= ?0 c) (<= c ?9)))

(defsubst js2-js-space-p (c)
  (if (<= c 127)
      (memq c '(#x20 #x9 #xC #xB))
    (or
     (eq c #xA0)
     ;; TODO:  change this nil to check for Unicode space character
     nil)))

(defsubst js2-skip-line ()
  "Skip to end of line"
  (let (c)
    (while (and (/= js2-EOF_CHAR (setq c (js2-get-char)))
                (/= c ?\n)))
    (js2-unget-char)
    (setq js2-token-end js2-ts-cursor)))

(defun js2-init-scanner (&optional buf line)
  "Create token stream for BUF starting on LINE.
BUF defaults to current-buffer and line defaults to 1.

A buffer can only have one scanner active at a time, which yields
dramatically simpler code than using a defstruct.  If you need to
have simultaneous scanners in a buffer, copy the regions to scan
into temp buffers."
  (save-excursion
    (when buf
      (set-buffer buf))
    (setq js2-ts-dirty-line nil
          js2-ts-regexp-flags nil
          js2-ts-string ""
          js2-ts-number nil
          js2-ts-hit-eof nil
          js2-ts-line-start 0
          js2-ts-lineno (or line 1)
          js2-ts-line-end-char -1
          js2-ts-cursor (point-min)
          js2-ts-is-xml-attribute nil
          js2-ts-xml-is-tag-content nil
          js2-ts-xml-open-tags-count 0
          js2-ts-string-buffer nil)))

;; This function uses the cached op, string and number fields in
;; TokenStream; if getToken has been called since the passed token
;; was scanned, the op or string printed may be incorrect.
(defun js2-token-to-string (token)
  ;; Not sure where this function is used in Rhino.  Not tested.
  (if (not js2-debug-print-trees)
      ""
    (let ((name (js2-token-name token)))
      (cond
       ((memq token (list js2-STRING js2-REGEXP js2-NAME))
        (concat name " `" js2-ts-string "'"))
       ((eq token js2-NUMBER)
        (format "NUMBER %g" js2-ts-number))
       (t
        name)))))

(defconst js2-keywords
  '(break
    case catch const continue
    debugger default delete do
    else enum
    false finally for function
    if in instanceof import
    let
    new null
    return
    switch
    this throw true try typeof
    var void
    while with
    yield))

;; Token names aren't exactly the same as the keywords, unfortunately.
;; E.g. enum isn't in the tokens, and delete is js2-DELPROP.
(defconst js2-kwd-tokens
  (let ((table (make-vector js2-num-tokens nil))
        (tokens
         (list js2-BREAK
               js2-CASE js2-CATCH js2-CONST js2-CONTINUE
               js2-DEBUGGER js2-DEFAULT js2-DELPROP js2-DO
               js2-ELSE
               js2-FALSE js2-FINALLY js2-FOR js2-FUNCTION
               js2-IF js2-IN js2-INSTANCEOF js2-IMPORT
               js2-LET
               js2-NEW js2-NULL
               js2-RETURN
               js2-SWITCH
               js2-THIS js2-THROW js2-TRUE js2-TRY js2-TYPEOF
               js2-VAR
               js2-WHILE js2-WITH
               js2-YIELD)))
    (dolist (i tokens)
      (aset table i 'font-lock-keyword-face))
    (aset table js2-STRING 'font-lock-string-face)
    (aset table js2-REGEXP 'font-lock-string-face)
    (aset table js2-COMMENT 'font-lock-comment-face)
    (aset table js2-THIS 'font-lock-builtin-face)
    (aset table js2-VOID 'font-lock-constant-face)
    (aset table js2-NULL 'font-lock-constant-face)
    (aset table js2-TRUE 'font-lock-constant-face)
    (aset table js2-FALSE 'font-lock-constant-face)
    table)
  "Vector whose values are non-nil for tokens that are keywords.
The values are default faces to use for highlighting the keywords.")

(defconst js2-reserved-words
  '(abstract
    boolean byte
    char class
    double
    enum extends
    final float
    goto
    implements int interface
    long
    native
    package private protected public
    short static super synchronized
    throws transient
    volatile))

(defconst js2-keyword-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js2-keywords
          do (puthash
              (symbol-name k)                            ; instanceof
              (intern (concat "js2-"
                              (upcase (symbol-name k)))) ; js2-INSTANCEOF
              table))
    table)
  "JavaScript keywords by name, mapped to their symbols.")

(defconst js2-reserved-word-names
  (let ((table (make-hash-table :test 'equal)))
    (loop for k in js2-reserved-words
          do
          (puthash (symbol-name k) 'js2-RESERVED table))
    table)
  "JavaScript reserved words by name, mapped to 'js2-RESERVED.")

(defsubst js2-collect-string (buf)
  "Convert BUF, a list of chars, to a string.
Reverses BUF before converting."
  (cond
   ((stringp buf)
    buf)
   ((null buf)  ; for emacs21 compat
    "")
   (t
    (if buf
        (apply #'string (nreverse buf))
      ""))))

(defun js2-string-to-keyword (s)
  "Return token for S, a string, if S is a keyword or reserved word.
Returns a symbol such as 'js2-BREAK, or nil if not keyword/reserved."
  (or (gethash s js2-keyword-names)
      (gethash s js2-reserved-word-names)))

(defsubst js2-ts-set-char-token-bounds ()
  "Used when next token is one character."
  (setq js2-token-beg (1- js2-ts-cursor)
        js2-token-end js2-ts-cursor))

(defsubst js2-ts-return (token)
  "Return an N-character TOKEN from `js2-get-token'.
Updates `js2-token-end' accordingly."
  (setq js2-token-end js2-ts-cursor)
  (throw 'return token))

(defsubst js2-x-digit-to-int (c accumulator)
  "Build up a hex number.
If C is a hexadecimal digit, return ACCUMULATOR * 16 plus
corresponding number.  Otherwise return -1."
  (catch 'return
    (catch 'check
      ;; Use 0..9 < A..Z < a..z
      (cond
       ((<= c ?9)
        (decf c ?0)
        (if (<= 0 c)
            (throw 'check nil)))
       ((<= c ?F)
        (when (<= ?A c)
          (decf c (- ?A 10))
          (throw 'check nil)))
       ((<= c ?f)
        (when (<= ?a c)
          (decf c (- ?a 10))
          (throw 'check nil))))
      (throw 'return -1))
    (logior c (lsh accumulator 4))))

(defun js2-get-token ()
  "Return next JavaScript token, an int such as js2-RETURN."
  (let (c
        c1
        identifier-start
        is-unicode-escape-start
        contains-escape
        escape-val
        escape-start
        str
        result
        base
        is-integer
        quote-char
        val
        look-for-slash
        continue)
    (catch 'return
      (while t
        ;; Eat whitespace, possibly sensitive to newlines.
        (setq continue t)
        (while continue
          (setq c (js2-get-char))
          (cond
           ((eq c js2-EOF_CHAR)
            (js2-ts-set-char-token-bounds)
            (throw 'return js2-EOF))
           ((eq c ?\n)
            (js2-ts-set-char-token-bounds)
            (setq js2-ts-dirty-line nil)
            (throw 'return js2-EOL))
           ((not (js2-js-space-p c))
            (if (/= c ?-)               ; in case end of HTML comment
                (setq js2-ts-dirty-line t))
            (setq continue nil))))

        ;; Assume the token will be 1 char - fixed up below.
        (js2-ts-set-char-token-bounds)

        (when (eq c ?@)
          (throw 'return js2-XMLATTR))

        ;; identifier/keyword/instanceof?
        ;; watch out for starting with a <backslash>
        (cond
         ((eq c ?\\)
          (setq c (js2-get-char))
          (if (eq c ?u)
              (setq identifier-start t
                    is-unicode-escape-start t
                    js2-ts-string-buffer nil)
            (setq identifier-start nil)
            (js2-unget-char)
            (setq c ?\\)))
         (t
          (when (setq identifier-start (js2-java-identifier-start-p c))
            (setq js2-ts-string-buffer nil)
            (js2-add-to-string c))))

        (when identifier-start
          (setq contains-escape is-unicode-escape-start)
          (catch 'break
            (while t
              (if is-unicode-escape-start
                  ;; strictly speaking we should probably push-back
                  ;; all the bad characters if the <backslash>uXXXX
                  ;; sequence is malformed. But since there isn't a
                  ;; correct context(is there?) for a bad Unicode
                  ;; escape sequence in an identifier, we can report
                  ;; an error here.
                  (progn
                    (setq escape-val 0)
                    (dotimes (i 4)
                      (setq c (js2-get-char)
                            escape-val (js2-x-digit-to-int c escape-val))
                      ;; Next check takes care of c < 0 and bad escape
                      (if (minusp escape-val)
                          (throw 'break nil)))
                    (if (minusp escape-val)
                        (js2-report-scan-error "msg.invalid.escape" t))
                    (js2-add-to-string escape-val)
                    (setq is-unicode-escape-start nil))
                (setq c (js2-get-char))
                (cond
                 ((eq c ?\\)
                  (setq c (js2-get-char))
                  (if (eq c ?u)
                      (setq is-unicode-escape-start t
                            contains-escape t)
                    (js2-report-scan-error "msg.illegal.character" t)))
                 (t
                  (if (or (eq c js2-EOF_CHAR)
                          (not (js2-java-identifier-part-p c)))
                      (throw 'break nil))
                  (js2-add-to-string c))))))
          (js2-unget-char)

          (setq str (js2-get-string-from-buffer))
          (unless contains-escape
            ;; OPT we shouldn't have to make a string (object!) to
            ;; check if it's a keyword.

            ;; Return the corresponding token if it's a keyword
            (when (setq result (js2-string-to-keyword str))
              (if (and (< js2-language-version 170)
                       (memq result '(js2-LET js2-YIELD)))
                  ;; LET and YIELD are tokens only in 1.7 and later
                  (setq result 'js2-NAME))
              (if (neq result js2-RESERVED)
                  (throw 'return (js2-token-code result)))
              (js2-report-warning "msg.reserved.keyword" str)))

          ;; If we want to intern these as Rhino does, just use (intern str)
          (setq js2-ts-string str)
          (throw 'return js2-NAME))     ; end identifier/kwd check

        ;; is it a number?
        (when (or (js2-digit-p c)
                  (and (eq c ?.) (js2-digit-p (js2-peek-char))))
          (setq js2-ts-string-buffer nil
                base 10)
          (when (eq c ?0)
            (setq c (js2-get-char))
            (cond
             ((or (eq c ?x) (eq c ?X))
              (setq base 16)
              (setq c (js2-get-char)))
             ((js2-digit-p c)
              (setq base 8))
             (t
              (js2-add-to-string ?0))))

          (if (eq base 16)
              (while (<= 0 (js2-x-digit-to-int c 0))
                (js2-add-to-string c)
                (setq c (js2-get-char)))
            (while (and (<= ?0 c) (<= c ?9))
              ;; We permit 08 and 09 as decimal numbers, which
              ;; makes our behavior a superset of the ECMA
              ;; numeric grammar.  We might not always be so
              ;; permissive, so we warn about it.
              (when (and (eq base 8) (>= c ?8))
                (js2-report-warning "msg.bad.octal.literal"
                                    (if (eq c ?8) "8" "9"))
                (setq base 10))
              (js2-add-to-string c)
              (setq c (js2-get-char))))

          (setq is-integer t)

          (when (and (eq base 10) (memq c '(?. ?e ?E)))
            (setq is-integer nil)
            (when (eq c ?.)
              (loop do
                    (js2-add-to-string c)
                    (setq c (js2-get-char))
                    while (js2-digit-p c)))
            (when (memq c '(?e ?E))
              (js2-add-to-string c)
              (setq c (js2-get-char))
              (when (memq c '(?+ ?-))
                (js2-add-to-string c)
                (setq c (js2-get-char)))
              (unless (js2-digit-p c)
                (js2-report-scan-error "msg.missing.exponent" t))
              (loop do
                    (js2-add-to-string c)
                    (setq c (js2-get-char))
                    while (js2-digit-p c))))

          (js2-unget-char)
          (setq js2-ts-string (js2-get-string-from-buffer)
                js2-ts-number
                (if (and (eq base 10) (not is-integer))
                    (string-to-number js2-ts-string)
                  ;; TODO:  call runtime number-parser.  Some of it is in
                  ;; js2-util.el, but I need to port ScriptRuntime.stringToNumber.
                  (string-to-number js2-ts-string)))
          (throw 'return js2-NUMBER))

        ;; is it a string?
        (when (memq c '(?\" ?\'))
          ;; We attempt to accumulate a string the fast way, by
          ;; building it directly out of the reader.  But if there
          ;; are any escaped characters in the string, we revert to
          ;; building it out of a string buffer.
          (setq quote-char c
                js2-ts-string-buffer nil
                c (js2-get-char))
          (catch 'break
            (while (/= c quote-char)
              (catch 'continue
                (when (or (eq c ?\n) (eq c js2-EOF_CHAR))
                  (js2-unget-char)
                  (setq js2-token-end js2-ts-cursor)
                  (js2-report-error "msg.unterminated.string.lit")
                  (throw 'return js2-STRING))

                (when (eq c ?\\)
                  ;; We've hit an escaped character
                  (setq c (js2-get-char))
                  (case c
                    (?b (setq c ?\b))
                    (?f (setq c ?\f))
                    (?n (setq c ?\n))
                    (?r (setq c ?\r))
                    (?t (setq c ?\t))
                    (?v (setq c ?\v))
                    (?u
                     (setq c1 (js2-read-unicode-escape))
                     (if js2-parse-ide-mode
                         (if c1
                             (progn
                               ;; just copy the string in IDE-mode
                               (js2-add-to-string ?\\)
                               (js2-add-to-string ?u)
                               (dotimes (i 3)
                                 (js2-add-to-string (js2-get-char)))
                               (setq c (js2-get-char))) ; added at end of loop
                           ;; flag it as an invalid escape
                           (js2-report-warning "msg.invalid.escape"
                                               nil (- js2-ts-cursor 2) 6))
                       ;; Get 4 hex digits; if the u escape is not
                       ;; followed by 4 hex digits, use 'u' + the
                       ;; literal character sequence that follows.
                       (js2-add-to-string ?u)
                       (setq escape-val 0)
                       (dotimes (i 4)
                         (setq c (js2-get-char)
                               escape-val (js2-x-digit-to-int c escape-val))
                         (if (minusp escape-val)
                             (throw 'continue nil))
                         (js2-add-to-string c))
                       ;; prepare for replace of stored 'u' sequence by escape value
                       (setq js2-ts-string-buffer (nthcdr 5 js2-ts-string-buffer)
                             c escape-val)))
                    (?x
                     ;; Get 2 hex digits, defaulting to 'x'+literal
                     ;; sequence, as above.
                     (setq c (js2-get-char)
                           escape-val (js2-x-digit-to-int c 0))
                     (if (minusp escape-val)
                         (progn
                           (js2-add-to-string ?x)
                           (throw 'continue nil))
                       (setq c1 c
                             c (js2-get-char)
                             escape-val (js2-x-digit-to-int c escape-val))
                       (if (minusp escape-val)
                           (progn
                             (js2-add-to-string ?x)
                             (js2-add-to-string c1)
                             (throw 'continue nil))
                         ;; got 2 hex digits
                         (setq c escape-val))))
                    (?\n
                     ;; Remove line terminator after escape to follow
                     ;; SpiderMonkey and C/C++
                     (setq c (js2-get-char))
                     (throw 'continue nil))
                    (t
                     (when (and (<= ?0 c) (< c ?8))
                       (setq val (- c ?0)
                             c (js2-get-char))
                       (when (and (<= ?0 c) (< c ?8))
                         (setq val (- (+ (* 8 val) c) ?0)
                               c (js2-get-char))
                         (when (and (<= ?0 c)
                                    (< c ?8)
                                    (< val #o37))
                           ;; c is 3rd char of octal sequence only
                           ;; if the resulting val <= 0377
                           (setq val (- (+ (* 8 val) c) ?0)
                                 c (js2-get-char))))
                       (js2-unget-char)
                       (setq c val)))))
                (js2-add-to-string c)
                (setq c (js2-get-char)))))
          (setq js2-ts-string (js2-get-string-from-buffer))
          (throw 'return js2-STRING))

        (case c
          (?\;
           (throw 'return js2-SEMI))
          (?\[
           (throw 'return js2-LB))
          (?\]
           (throw 'return js2-RB))
          (?{
           (throw 'return js2-LC))
          (?}
           (throw 'return js2-RC))
          (?\(
           (throw 'return js2-LP))
          (?\)
           (throw 'return js2-RP))
          (?,
           (throw 'return js2-COMMA))
          (??
           (throw 'return js2-HOOK))
          (?:
           (if (js2-match-char ?:)
               (js2-ts-return js2-COLONCOLON)
             (throw 'return js2-COLON)))
          (?.
           (if (js2-match-char ?.)
               (js2-ts-return js2-DOTDOT)
             (if (js2-match-char ?\()
                 (js2-ts-return js2-DOTQUERY)
               (throw 'return js2-DOT))))
          (?|
           (if (js2-match-char ?|)
               (throw 'return js2-OR)
             (if (js2-match-char ?=)
                 (js2-ts-return js2-ASSIGN_BITOR)
               (throw 'return js2-BITOR))))
          (?^
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_BITOR)
             (throw 'return js2-BITXOR)))
          (?&
           (if (js2-match-char ?&)
               (throw 'return js2-AND)
             (if (js2-match-char ?=)
                 (js2-ts-return js2-ASSIGN_BITAND)
               (throw 'return js2-BITAND))))
          (?=
           (if (js2-match-char ?=)
               (if (js2-match-char ?=)
                   (js2-ts-return js2-SHEQ)
                 (throw 'return js2-EQ))
             (throw 'return js2-ASSIGN)))
          (?!
           (if (js2-match-char ?=)
               (if (js2-match-char ?=)
                   (js2-ts-return js2-SHNE)
                 (js2-ts-return js2-NE))
             (throw 'return js2-NOT)))
          (?<
           ;; NB:treat HTML begin-comment as comment-till-eol
           (when (js2-match-char ?!)
             (when (js2-match-char ?-)
               (when (js2-match-char ?-)
                 (js2-skip-line)
                 (setq js2-ts-comment-type 'html)
                 (throw 'return js2-COMMENT)))
             (js2-unget-char))

           (if (js2-match-char ?<)
               (if (js2-match-char ?=)
                   (js2-ts-return js2-ASSIGN_LSH)
                 (js2-ts-return js2-LSH))
             (if (js2-match-char ?=)
                 (js2-ts-return js2-LE)
               (throw 'return js2-LT))))
          (?>
           (if (js2-match-char ?>)
               (if (js2-match-char ?>)
                   (if (js2-match-char ?=)
                       (js2-ts-return js2-ASSIGN_URSH)
                     (js2-ts-return js2-URSH))
                 (if (js2-match-char ?=)
                     (js2-ts-return js2-ASSIGN_RSH)
                   (js2-ts-return js2-RSH)))
             (if (js2-match-char ?=)
                 (js2-ts-return js2-GE)
               (throw 'return js2-GT))))
          (?*
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_MUL)
             (throw 'return js2-MUL)))

          (?/
           ;; is it a // comment?
           (when (js2-match-char ?/)
             (setq js2-token-beg (- js2-ts-cursor 2))
             (js2-skip-line)
             (setq js2-ts-comment-type 'line)
             (throw 'return js2-COMMENT))

           ;; is it a /* comment?
           (when (js2-match-char ?*)
             (setq look-for-slash nil
                   js2-token-beg (- js2-ts-cursor 2)
                   js2-ts-comment-type
                   (if (js2-match-char ?*)
                       (progn
                         (setq look-for-slash t)
                         'jsdoc)
                     'block))
             (while t
               (setq c (js2-get-char))
               (cond
                ((eq c js2-EOF_CHAR)
                 (setq js2-token-end (1- js2-ts-cursor))
                 (js2-report-error "msg.unterminated.comment")
                 (throw 'return js2-COMMENT))
                ((eq c ?*)
                 (setq look-for-slash t))
                ((eq c ?/)
                 (if look-for-slash
                   (js2-ts-return js2-COMMENT)))
                (t
                 (setq look-for-slash nil
                       js2-token-end js2-ts-cursor)))))

           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_DIV)
             (throw 'return js2-DIV)))

           (?#
            (when js2-skip-preprocessor-directives
              (js2-skip-line)
              (setq js2-ts-comment-type 'preprocessor
                    js2-token-end js2-ts-cursor)
              (throw 'return js2-COMMENT))
            (throw 'return js2-ERROR))

          (?%
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_MOD)
             (throw 'return js2-MOD)))
          (?~
           (throw 'return js2-BITNOT))
          (?+
           (if (js2-match-char ?=)
               (js2-ts-return js2-ASSIGN_ADD)
             (if (js2-match-char ?+)
                 (js2-ts-return js2-INC)
               (throw 'return js2-ADD))))
          (?-
           (cond
            ((js2-match-char ?=)
             (setq c js2-ASSIGN_SUB))
            ((js2-match-char ?-)
             (unless js2-ts-dirty-line
               ;; treat HTML end-comment after possible whitespace
               ;; after line start as comment-until-eol
               (when (js2-match-char ?>)
                 (js2-skip-line)
                 (setq js2-ts-comment-type 'html)
                 (throw 'return js2-COMMENT)))
             (setq c js2-DEC))
            (t
             (setq c js2-SUB)))
           (setq js2-ts-dirty-line t)
           (js2-ts-return c))

          (otherwise
           (js2-report-scan-error "msg.illegal.character")))))))

(defun js2-read-regexp (start-token)
  "Called by parser when it gets / or /= in literal context."
  (let (c
        err
        in-class  ; inside a '[' .. ']' character-class
        flags
        (continue t))
    (setq js2-token-beg js2-ts-cursor
          js2-ts-string-buffer nil
          js2-ts-regexp-flags nil)

    (if (eq start-token js2-ASSIGN_DIV)
        ;; mis-scanned /=
        (js2-add-to-string ?=)
      (if (neq start-token js2-DIV)
          (error "failed assertion")))

    (while (and (not err)
                (or (/= (setq c (js2-get-char)) ?/)
                    in-class))
      (cond
       ((or (= c ?\n)
            (= c js2-EOF_CHAR))
        (setq js2-token-end (1- js2-ts-cursor)
              err t
              js2-ts-string (js2-collect-string js2-ts-string-buffer))
        (js2-report-error "msg.unterminated.re.lit"))
       (t (cond
           ((= c ?\\)
            (js2-add-to-string c)
            (setq c (js2-get-char)))

           ((= c ?\[)
            (setq in-class t))

           ((= c ?\])
            (setq in-class nil)))
          (js2-add-to-string c))))

    (unless err
      (while continue
        (cond
         ((js2-match-char ?g)
          (push ?g flags))
         ((js2-match-char ?i)
          (push ?i flags))
         ((js2-match-char ?m)
          (push ?m flags))
         (t
          (setq continue nil))))
      (if (js2-alpha-p (js2-peek-char))
          (js2-report-scan-error "msg.invalid.re.flag" t
                                 js2-ts-cursor 1))
      (setq js2-ts-string (js2-collect-string js2-ts-string-buffer)
            js2-ts-regexp-flags (js2-collect-string flags)
            js2-token-end js2-ts-cursor)
      ;; tell `parse-partial-sexp' to ignore this range of chars
      (put-text-property js2-token-beg js2-token-end 'syntax-class '(2)))))

(defun js2-get-first-xml-token ()
  (setq js2-ts-xml-open-tags-count 0
        js2-ts-is-xml-attribute nil
        js2-ts-xml-is-tag-content nil)
  (js2-unget-char)
  (js2-get-next-xml-token))

(defsubst js2-xml-discard-string ()
  "Throw away the string in progress and flag an XML parse error."
  (setq js2-ts-string-buffer nil
        js2-ts-string nil)
  (js2-report-scan-error "msg.XML.bad.form" t))

(defun js2-get-next-xml-token ()
  (setq js2-ts-string-buffer nil  ; for recording the XML
        js2-token-beg js2-ts-cursor)
  (let (c result)
    (setq result
          (catch 'return
            (while t
              (setq c (js2-get-char))
              (cond
               ((= c js2-EOF_CHAR)
                (throw 'return js2-ERROR))

               (js2-ts-xml-is-tag-content
                (case c
                  (?>
                   (js2-add-to-string c)
                   (setq js2-ts-xml-is-tag-content nil
                         js2-ts-is-xml-attribute nil))
                  (?/
                   (js2-add-to-string c)
                   (when (eq ?> (js2-peek-char))
                     (setq c (js2-get-char))
                     (js2-add-to-string c)
                     (setq js2-ts-xml-is-tag-content nil)
                     (decf js2-ts-xml-open-tags-count)))
                  (?{
                   (js2-unget-char)
                   (setq js2-ts-string (js2-get-string-from-buffer))
                   (throw 'return js2-XML))
                  ((?\' ?\")
                   (js2-add-to-string c)
                   (unless (js2-read-quoted-string c)
                     (throw 'return js2-ERROR)))
                  (?=
                   (js2-add-to-string c)
                   (setq js2-ts-is-xml-attribute t))
                  ((? ?\t ?\r ?\n)
                   (js2-add-to-string c))
                  (t
                   (js2-add-to-string c)
                   (setq js2-ts-is-xml-attribute nil)))
                (when (and (not js2-ts-xml-is-tag-content)
                           (zerop js2-ts-xml-open-tags-count))
                  (setq js2-ts-string (js2-get-string-from-buffer))
                  (throw 'return js2-XMLEND)))

               (t
                ;; else not tag content
                (case c
                  (?<
                   (js2-add-to-string c)
                   (setq c (js2-peek-char))
                   (case c
                     (?!
                      (setq c (js2-get-char)) ;; skip !
                      (js2-add-to-string c)
                      (setq c (js2-peek-char))
                      (case c
                        (?-
                         (setq c (js2-get-char)) ;; skip -
                         (js2-add-to-string c)
                         (if (eq c ?-)
                             (progn
                               (js2-add-to-string c)
                               (unless (js2-read-xml-comment)
                                 (throw 'return js2-ERROR)))
                           (js2-xml-discard-string)
                           (throw 'return js2-ERROR)))
                        (?\[
                         (setq c (js2-get-char)) ;; skip [
                         (js2-add-to-string c)
                         (if (and (= (js2-get-char) ?C)
                                  (= (js2-get-char) ?D)
                                  (= (js2-get-char) ?A)
                                  (= (js2-get-char) ?T)
                                  (= (js2-get-char) ?A)
                                  (= (js2-get-char) ?\[))
                             (progn
                               (js2-add-to-string ?C)
                               (js2-add-to-string ?D)
                               (js2-add-to-string ?A)
                               (js2-add-to-string ?T)
                               (js2-add-to-string ?A)
                               (js2-add-to-string ?\[)
                               (unless (js2-read-cdata)
                                 (throw 'return js2-ERROR)))
                           (js2-xml-discard-string)
                           (throw 'return js2-ERROR)))
                        (t
                         (unless (js2-read-entity)
                           (throw 'return js2-ERROR)))))
                     (??
                      (setq c (js2-get-char)) ;; skip ?
                      (js2-add-to-string c)
                      (unless (js2-read-PI)
                        (throw 'return js2-ERROR)))
                     (?/
                      ;; end tag
                      (setq c (js2-get-char)) ;; skip /
                      (js2-add-to-string c)
                      (when (zerop js2-ts-xml-open-tags-count)
                        (js2-xml-discard-string)
                        (throw 'return js2-ERROR))
                      (setq js2-ts-xml-is-tag-content t)
                      (decf js2-ts-xml-open-tags-count))
                     (t
                      ;; start tag
                      (setq js2-ts-xml-is-tag-content t)
                      (incf js2-ts-xml-open-tags-count))))
                  (?{
                   (js2-unget-char)
                   (setq js2-ts-string (js2-get-string-from-buffer))
                   (throw 'return js2-XML))
                  (t
                   (js2-add-to-string c))))))))
    (setq js2-token-end js2-ts-cursor)
    result))
        
(defun js2-read-quoted-string (quote)
  (let (c)
    (catch 'return
      (while (/= (setq c (js2-get-char)) js2-EOF_CHAR)
        (js2-add-to-string c)
        (if (eq c quote)
            (throw 'return t)))
      (js2-xml-discard-string)  ;; throw away string in progress
      nil)))

(defun js2-read-xml-comment ()
  (let ((c (js2-get-char)))
    (catch 'return
      (while (/= c js2-EOF_CHAR)
        (catch 'continue
          (js2-add-to-string c)
          (when (and (eq c ?-) (eq ?- (js2-peek-char)))
            (setq c (js2-get-char))
            (js2-add-to-string c)
            (if (eq (js2-peek-char) ?>)
                (progn
                  (setq c (js2-get-char)) ;; skip >
                  (js2-add-to-string c)
                  (throw 'return t))
              (throw 'continue nil)))
          (setq c (js2-get-char))))
      (js2-xml-discard-string)
      nil)))

(defun js2-read-cdata ()
  (let ((c (js2-get-char)))
    (catch 'return
      (while (/= c js2-EOF_CHAR)
        (catch 'continue
          (js2-add-to-string c)
          (when (and (eq c ?\]) (eq (js2-peek-char) ?\]))
            (setq c (js2-get-char))
            (js2-add-to-string c)
            (if (eq (js2-peek-char) ?>)
                (progn
                  (setq c (js2-get-char)) ;; Skip >
                  (js2-add-to-string c)
                  (throw 'return t))
              (throw 'continue nil)))
          (setq c (js2-get-char))))
      (js2-xml-discard-string)
      nil)))

(defun js2-read-entity ()
  (let ((decl-tags 1)
        c)
    (catch 'return
      (while (/= js2-EOF_CHAR (setq c (js2-get-char)))
        (js2-add-to-string c)
        (case c
          (?<
           (incf decl-tags))
          (?>
           (decf decl-tags)
           (if (zerop decl-tags)
               (throw 'return t)))))
      (js2-xml-discard-string)
      nil)))

(defun js2-read-PI ()
  "Scan an XML processing instruction."
  (let (c)
    (catch 'return
      (while (/= js2-EOF_CHAR (setq c (js2-get-char)))
        (js2-add-to-string c)
        (when (and (eq c ??) (eq (js2-peek-char) ?>))
          (setq c (js2-get-char))  ;; Skip >
          (js2-add-to-string c)
          (throw 'return t)))
      (js2-xml-discard-string)
      nil)))

(defun js2-scanner-get-line ()
  "Return the text of the current scan line."
  (buffer-substring (point-at-bol) (point-at-eol)))

(provide 'js2-scan)

;;; js2-scan.el ends here
