;;; js2-vars.el -- byte-compiler support for js2-mode

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

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-langs)    ; it's here in Emacs 21...
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar js2-emacs22 (>= emacs-major-version 22))

(defcustom js2-highlight-level 2
  "Amount of syntax highlighting to perform.
nil, zero or negative means none.
1 adds basic syntax highlighting.
2 adds highlighting of some Ecma built-in properties.
3 adds highlighting of many Ecma built-in functions."
  :type 'integer
  :group 'js2-mode)

(defvar js2-mode-dev-mode-p t
  "Non-nil if running in development mode.  Normally nil.")

(defgroup js2-mode nil
  "An improved JavaScript mode."
  :group 'languages)

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              2)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :type 'integer)
(make-variable-buffer-local 'js2-basic-offset)

(defcustom js2-cleanup-whitespace t
  "Non-nil to invoke `delete-trailing-whitespace' before saves."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste etc. can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-mirror-mode t
  "Non-nil to insert closing brackets, parens, etc. automatically."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-auto-indent-flag t
  "Automatic indentation with punctuation characters. If non-nil, the
current line is indented when certain punctuations are inserted."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-bounce-indent-flag t
  "Non-nil to have indent-line function choose among alternatives.
If nil, the indent-line function will indent to a predetermined column
based on heuristic guessing.  If non-nil, then if the current line is
already indented to that predetermined column, indenting will choose
another likely column and indent to that spot.  Repeated invocation of
the indent-line function will cycle among the computed alternatives.
See the function `js2-bounce-indent' for details."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-indent-on-enter-key nil
  "Non-nil to have Enter/Return key indent the line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-enter-indents-newline t
  "Non-nil to have Enter/Return key indent the newly-inserted line.
This is unusual for Emacs modes but common in IDEs like Eclipse."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-rebind-eol-bol-keys t
  "Non-nil to rebind beginning-of-line and end-of-line keys.
If non-nil, bounce between bol/eol and first/last non-whitespace char."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-electric-keys '("{" "}" "(" ")" "[" "]" ":" ";" "," "*")
  "Keys that auto-indent when `js2-auto-indent-flag' is non-nil.
Each value in the list is passed to `define-key'."
  :type 'list
  :group 'js2-mode)

(defcustom js2-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js2-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js2-mode)
(make-variable-buffer-local 'js2-idle-timer-delay)

(defcustom js2-dynamic-idle-timer-adjust 0
  "Positive to adjust `js2-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js2-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js2-idle-timer-delay is multiplied by 2.
If `js2-dynamic-idle-timer-adjust' is 0 or negative,
`js2-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js2-mode)

(defcustom js2-mode-escape-quotes t
  "Non-nil to disable automatic quote-escaping inside strings."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-squeeze-spaces t
  "Non-nil to normalize whitespace when filling in comments.
Multiple runs of spaces are converted to a single space."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-parse-errors t
  "True to highlight parse errors."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262 forbids them, but many browsers permit them.  IE is the
big exception, and can produce bugs if you have trailing commas."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-missing-semi-warning t
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-redeclaration-warning t
  "Non-nil to warn about redeclaring variables in a script or function."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-hides-function-arg-warning t
  "Non-nil to warn about a var decl hiding a function argument."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-skip-preprocessor-directives nil
  "Non-nil to treat lines beginning with # as comments.
Useful for viewing Mozilla JavaScript source code."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-basic-offset c-basic-offset
  "Functions like `c-basic-offset' in js2-mode buffers."
  :type 'integer
  :group 'js2-mode)
(make-variable-buffer-local 'js2-basic-offset)

(defcustom js2-language-version 170
  "Configures what JavaScript language version to recognize.
Currently only 150, 160 and 170 are supported, corresponding
to JavaScript 1.5, 1.6 and 1.7, respectively.  In a nutshell,
1.6 adds E4X support, and 1.7 adds let, yield, and Array
comprehensions."
  :type 'integer
  :group 'js2-mode)

(defcustom js2-allow-keywords-as-property-names t
  "If non-nil, you can use JavaScript keywords as object property names.
Examples:

  var foo = {int: 5, while: 6, continue: 7};
  foo.return = 8;

Ecma-262 forbids this syntax, but many browsers support it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-instanceof-has-side-effects nil
  "If non-nil, treats the instanceof operator as having side effects.
This is useful for xulrunner apps."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-rhino-new-expr-initializer t
  "Non-nil to support a Rhino's experimental syntactic construct.

Rhino supports the ability to follow a `new' expression with an object
literal, which is used to set additional properties on the new object
after calling its constructor.  Syntax:

  new <expr> [ ( arglist ) ] [initializer]

Hence, this expression:

  new Object {a: 1, b: 2}

results in an Object with properties a=1 and b=2.  This syntax is
apparently not configurable in Rhino - it's currently always enabled,
as of Rhino version 1.7R2."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-member-expr-as-function-name nil
  "Non-nil to support experimental Rhino syntax for function names.

Rhino supports an experimental syntax configured via the Rhino Context
setting `allowMemberExprAsFunctionName'.  The experimental syntax is:

  function <member-expr> ( [ arg-list ] ) { <body> }

Where member-expr is a non-parenthesized 'member expression', which
is anything at the grammar level of a new-expression or lower, meaning
any expression that does not involve infix or unary operators.

When <member-expr> is not a simple identifier, then it is syntactic
sugar for assigning the anonymous function to the <member-expr>.  Hence,
this code:

  function a.b().c[2] (x, y) { ... }

is rewritten as:

  a.b().c[2] = function(x, y) {...}

which doesn't seem particularly useful, but Rhino permits it."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-mode-version 20080322
  "Release number for `js2-mode'.")

;; scanner variables

;; We record the start and end position of each token.
(defvar js2-token-beg 1)
(make-variable-buffer-local 'js2-token-beg)
(defvar js2-token-end -1)
(make-variable-buffer-local 'js2-token-end)

(defvar js2-EOF_CHAR -1
  "Represents end of stream.  Distinct from js2-EOF token type.")

;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js2-' prefix in front of each name.
(defvar js2-ERROR -1)
(defvar js2-EOF 0)
(defvar js2-EOL 1)
(defvar js2-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js2-LEAVEWITH 3)
(defvar js2-RETURN 4)
(defvar js2-GOTO 5)
(defvar js2-IFEQ 6)
(defvar js2-IFNE 7)
(defvar js2-SETNAME 8)
(defvar js2-BITOR 9)
(defvar js2-BITXOR 10)
(defvar js2-BITAND 11)
(defvar js2-EQ 12)
(defvar js2-NE 13)
(defvar js2-LT 14)
(defvar js2-LE 15)
(defvar js2-GT 16)
(defvar js2-GE 17)
(defvar js2-LSH 18)
(defvar js2-RSH 19)
(defvar js2-URSH 20)
(defvar js2-ADD 21)            ; infix plus
(defvar js2-SUB 22)            ; infix minus
(defvar js2-MUL 23)
(defvar js2-DIV 24)
(defvar js2-MOD 25)
(defvar js2-NOT 26)
(defvar js2-BITNOT 27)
(defvar js2-POS 28)            ; unary plus
(defvar js2-NEG 29)            ; unary minus
(defvar js2-NEW 30)
(defvar js2-DELPROP 31)
(defvar js2-TYPEOF 32)
(defvar js2-GETPROP 33)
(defvar js2-GETPROPNOWARN 34)
(defvar js2-SETPROP 35)
(defvar js2-GETELEM 36)
(defvar js2-SETELEM 37)
(defvar js2-CALL 38)
(defvar js2-NAME 39)           ; an identifier
(defvar js2-NUMBER 40)
(defvar js2-STRING 41)
(defvar js2-NULL 42)
(defvar js2-THIS 43)
(defvar js2-FALSE 44)
(defvar js2-TRUE 45)
(defvar js2-SHEQ 46)           ; shallow equality (===)
(defvar js2-SHNE 47)           ; shallow inequality (!==)
(defvar js2-REGEXP 48)
(defvar js2-BINDNAME 49)
(defvar js2-THROW 50)
(defvar js2-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js2-IN 52)
(defvar js2-INSTANCEOF 53)
(defvar js2-LOCAL_LOAD 54)
(defvar js2-GETVAR 55)
(defvar js2-SETVAR 56)
(defvar js2-CATCH_SCOPE 57)
(defvar js2-ENUM_INIT_KEYS 58)
(defvar js2-ENUM_INIT_VALUES 59)
(defvar js2-ENUM_INIT_ARRAY 60)
(defvar js2-ENUM_NEXT 61)
(defvar js2-ENUM_ID 62)
(defvar js2-THISFN 63)
(defvar js2-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js2-ARRAYLIT 65)       ; array literal
(defvar js2-OBJECTLIT 66)      ; object literal
(defvar js2-GET_REF 67)        ; *reference
(defvar js2-SET_REF 68)        ; *reference = something
(defvar js2-DEL_REF 69)        ; delete reference
(defvar js2-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js2-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js2-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; XML support
(defvar js2-DEFAULTNAMESPACE 73)
(defvar js2-ESCXMLATTR 74)
(defvar js2-ESCXMLTEXT 75)
(defvar js2-REF_MEMBER 76)     ; Reference for x.@y, x..y etc.
(defvar js2-REF_NS_MEMBER 77)  ; Reference for x.ns::y, x..ns::y etc.
(defvar js2-REF_NAME 78)       ; Reference for @y, @[y] etc.
(defvar js2-REF_NS_NAME 79)    ; Reference for ns::y, @ns::y@[y] etc.

(defvar js2-first-bytecode js2-ENTERWITH)
(defvar js2-last-bytecode js2-REF_NS_NAME)

(defvar js2-TRY 80)
(defvar js2-SEMI 81)           ; semicolon
(defvar js2-LB 82)             ; left and right brackets
(defvar js2-RB 83)
(defvar js2-LC 84)             ; left and right curly-braces
(defvar js2-RC 85)
(defvar js2-LP 86)             ; left and right parens
(defvar js2-RP 87)
(defvar js2-COMMA 88)          ; comma operator

(defvar js2-ASSIGN 89)         ; simple assignment (=)
(defvar js2-ASSIGN_BITOR 90)   ; |=
(defvar js2-ASSIGN_BITXOR 91)  ; ^=
(defvar js2-ASSIGN_BITAND 92)  ; &=
(defvar js2-ASSIGN_LSH 93)     ; <<=
(defvar js2-ASSIGN_RSH 94)     ; >>=
(defvar js2-ASSIGN_URSH 95)    ; >>>=
(defvar js2-ASSIGN_ADD 96)     ; +=
(defvar js2-ASSIGN_SUB 97)     ; -=
(defvar js2-ASSIGN_MUL 98)     ; *=
(defvar js2-ASSIGN_DIV 99)     ; /=
(defvar js2-ASSIGN_MOD 100)    ; %=

(defvar js2-first-assign js2-ASSIGN)
(defvar js2-last-assign js2-ASSIGN_MOD)

(defvar js2-HOOK 101)          ; conditional (?:)
(defvar js2-COLON 102)
(defvar js2-OR 103)            ; logical or (||)
(defvar js2-AND 104)           ; logical and (&&)
(defvar js2-INC 105)           ; increment/decrement (++ --)
(defvar js2-DEC 106)
(defvar js2-DOT 107)           ; member operator (.)
(defvar js2-FUNCTION 108)      ; function keyword
(defvar js2-EXPORT 109)        ; export keyword
(defvar js2-IMPORT 110)        ; import keyword
(defvar js2-IF 111)            ; if keyword
(defvar js2-ELSE 112)          ; else keyword
(defvar js2-SWITCH 113)        ; switch keyword
(defvar js2-CASE 114)          ; case keyword
(defvar js2-DEFAULT 115)       ; default keyword
(defvar js2-WHILE 116)         ; while keyword
(defvar js2-DO 117)            ; do keyword
(defvar js2-FOR 118)           ; for keyword
(defvar js2-BREAK 119)         ; break keyword
(defvar js2-CONTINUE 120)      ; continue keyword
(defvar js2-VAR 121)           ; var keyword
(defvar js2-WITH 122)          ; with keyword
(defvar js2-CATCH 123)         ; catch keyword
(defvar js2-FINALLY 124)       ; finally keyword
(defvar js2-VOID 125)          ; void keyword
(defvar js2-RESERVED 126)      ; reserved keywords

(defvar js2-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js2-BLOCK 128)         ; statement block
(defvar js2-LABEL 129)         ; label
(defvar js2-TARGET 130)
(defvar js2-LOOP 131)
(defvar js2-EXPR_VOID 132)     ; expression statement in functions
(defvar js2-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js2-JSR 134)
(defvar js2-SCRIPT 135)        ; top-level node for entire script
(defvar js2-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js2-USE_STACK 137)
(defvar js2-SETPROP_OP 138)    ; x.y op= something
(defvar js2-SETELEM_OP 139)    ; x[y] op= something
(defvar js2-LOCAL_BLOCK 140)
(defvar js2-SET_REF_OP 141)    ; *reference op= something

;; For XML support:
(defvar js2-DOTDOT 142)        ; member operator (..)
(defvar js2-COLONCOLON 143)    ; namespace::name
(defvar js2-XML 144)           ; XML type
(defvar js2-DOTQUERY 145)      ; .() -- e.g., x.emps.emp.(name == "terry")
(defvar js2-XMLATTR 146)       ; @
(defvar js2-XMLEND 147)

;; Optimizer-only tokens
(defvar js2-TO_OBJECT 148)
(defvar js2-TO_DOUBLE 149)

(defvar js2-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js2-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js2-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js2-CONST 153)
(defvar js2-SETCONST 154)
(defvar js2-SETCONSTVAR 155)
(defvar js2-ARRAYCOMP 156)
(defvar js2-LETEXPR 157)
(defvar js2-WITHEXPR 158)
(defvar js2-DEBUGGER 159)

(defvar js2-COMMENT 160)  ; not yet in Rhino

(defvar js2-num-tokens (1+ js2-COMMENT))

(defconst js2-debug-print-trees nil)

;; Rhino accepts any string or stream as input.
;; Emacs character processing works best in buffers, so we'll
;; assume the input is a buffer.  JavaScript strings can be
;; copied into temp buffers before scanning them.

(defmacro deflocal (name value comment)
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(deflocal js2-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(deflocal js2-ts-regexp-flags nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-string ""
  "Token stream buffer-local variable.
Last string scanned.")

(deflocal js2-ts-number nil
  "Token stream buffer-local variable.
Last literal number scanned.")

(deflocal js2-ts-hit-eof nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-line-start 0
  "Token stream buffer-local variable.")

(deflocal js2-ts-lineno 1
  "Token stream buffer-local variable.")

(deflocal js2-ts-line-end-char -1
  "Token stream buffer-local variable.")

(deflocal js2-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

(deflocal js2-ts-is-xml-attribute nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-xml-is-tag-content nil
  "Token stream buffer-local variable.")

(deflocal js2-ts-xml-open-tags-count 0
  "Token stream buffer-local variable.")

(deflocal js2-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(deflocal js2-ts-comment-type nil
  "Token stream buffer-local variable.")

;;; Parser variables

(defvar js2-parsed-errors nil
  "List of errors produced during scanning/parsing.")
(make-variable-buffer-local 'js2-parsed-errors)

(defvar js2-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")
(make-variable-buffer-local 'js2-parsed-warnings)

(defvar js2-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")
(make-variable-buffer-local 'js2-recover-from-parse-errors)

(defvar js2-parse-hook nil
  "List of callbacks for receiving parsing progress.")
(make-variable-buffer-local 'js2-parse-hook)

(defvar js2-parse-finished-hook nil
  "List of callbacks to notify when parsing finishes.
Not called if parsing was interrupted.")

(defvar js2-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")
(make-variable-buffer-local 'js2-is-eval-code)

(defvar js2-parse-ide-mode t
  "Non-nil if the parser is being used for `js2-mode'.
If non-nil, the parser will set text properties for fontification
and the syntax-table.  The value should be nil when using the
parser as a frontend to an interpreter or byte compiler.")

;;; Parser instance variables (buffer-local vars for js2-parse)

(defconst js2-clear-ti-mask #xFFFF
  "Mask to clear token information bits.")

(defconst js2-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

(defconst js2-ti-check-label (lsh 1 17)
  "Flag:  indicates to check for label.")

;; Inline Rhino's CompilerEnvirons vars as buffer-locals.

(defvar js2-compiler-generate-debug-info t)
(make-variable-buffer-local 'js2-compiler-generate-debug-info)

(defvar js2-compiler-use-dynamic-scope nil)
(make-variable-buffer-local 'js2-compiler-use-dynamic-scope)

(defvar js2-compiler-reserved-keywords-as-identifier nil)
(make-variable-buffer-local 'js2-compiler-reserved-keywords-as-identifier)

(defvar js2-compiler-xml-available t)
(make-variable-buffer-local 'js2-compiler-xml-available)

(defvar js2-compiler-optimization-level 0)
(make-variable-buffer-local 'js2-compiler-optimization-level)

(defvar js2-compiler-generating-source t)
(make-variable-buffer-local 'js2-compiler-generating-source)

(defvar js2-compiler-strict-mode nil)
(make-variable-buffer-local 'js2-compiler-strict-mode)

(defvar js2-compiler-report-warning-as-error nil)
(make-variable-buffer-local 'js2-compiler-report-warning-as-error)

(defvar js2-compiler-generate-observer-count nil)
(make-variable-buffer-local 'js2-compiler-generate-observer-count)

(defvar js2-compiler-activation-names nil)
(make-variable-buffer-local 'js2-compiler-activation-names)

;; SKIP:  sourceURI

;; There's a compileFunction method in Context.java - may need it.
(defvar js2-called-by-compile-function nil
  "True if `js2-parse' was called by `js2-compile-function'.
Will only be used when we finish implementing the interpreter.")
(make-variable-buffer-local 'js2-called-by-compile-function)

;; SKIP:  ts  (we just call `js2-init-scanner' and use its vars)

(defvar js2-current-flagged-token js2-EOF)
(make-variable-buffer-local 'js2-current-flagged-token)

(defvar js2-current-token js2-EOF)
(make-variable-buffer-local 'js2-current-token)

;; SKIP:  node factory - we're going to just call functions directly,
;; and eventually go to a unified AST format.

(defvar js2-nesting-of-function 0)
(make-variable-buffer-local 'js2-nesting-of-function)

(defvar js2-recorded-assignments nil)
(make-variable-buffer-local 'js2-assignments-from-parse)

;; SKIP:  decompiler
;; SKIP:  encoded-source

;;; These variables are per-function and should be saved/restored
;;; during function parsing.

(defvar js2-current-script-or-fn nil)
(make-variable-buffer-local 'js2-current-script-or-fn)

(defvar js2-current-scope nil)
(make-variable-buffer-local 'js2-current-scope)

(defvar js2-nesting-of-with 0)
(make-variable-buffer-local 'js2-nesting-of-with)

(defvar js2-label-set nil
  "An alist mapping label names to nodes.")
(make-variable-buffer-local 'js2-label-set)

(defvar js2-loop-set nil)
(make-variable-buffer-local 'js2-loop-set)

(defvar js2-loop-and-switch-set nil)
(make-variable-buffer-local 'js2-loop-and-switch-set)

(defvar js2-has-return-value nil)
(make-variable-buffer-local 'js2-has-return-value)

(defvar js2-end-flags 0)
(make-variable-buffer-local 'js2-end-flags)

;;; end of per function variables

;; Without 2-token lookahead, labels are a problem.
;; These vars store the token info of the last matched name,
;; iff it wasn't the last matched token.  Only valid in some contexts.
(defvar js2-prev-name-token-start nil)
(defvar js2-prev-name-token-string nil)

(defsubst js2-save-name-token-data (pos name)
  (setq js2-prev-name-token-start pos
        js2-prev-name-token-string name))

;; These flags enumerate the possible ways a statement/function can
;; terminate. These flags are used by endCheck() and by the Parser to
;; detect inconsistent return usage.
;;
;; END_UNREACHED is reserved for code paths that are assumed to always be
;; able to execute (example: throw, continue)
;;
;; END_DROPS_OFF indicates if the statement can transfer control to the
;; next one. Statement such as return dont. A compound statement may have
;; some branch that drops off control to the next statement.
;;
;; END_RETURNS indicates that the statement can return (without arguments)
;; END_RETURNS_VALUE indicates that the statement can return a value.
;;
;; A compound statement such as
;; if (condition) {
;;   return value;
;; }
;; Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js2-end-unreached     #x0)
(defconst js2-end-drops-off     #x1)
(defconst js2-end-returns       #x2)
(defconst js2-end-returns-value #x4)
(defconst js2-end-yields        #x8)

;; Rhino awkwardly passes a statementLabel parameter to the
;; statementHelper() function, the main statement parser, which
;; is then used by quite a few of the sub-parsers.  We just make
;; it a buffer-local variable and make sure it's cleaned up properly.
(defvar js2-labeled-stmt nil)  ; type `js2-labeled-stmt-node'
(make-variable-buffer-local 'js2-labeled-stmt)

;; Similarly, Rhino passes an inForInit boolean through about half
;; the expression parsers.  We use a dynamically-scoped variable,
;; which makes it easier to funcall the parsers individually without
;; worrying about whether they take the parameter or not.
(defvar js2-in-for-init nil)
(make-variable-buffer-local 'js2-in-for-init)

(defvar js2-temp-name-counter 0)
(make-variable-buffer-local 'js2-temp-name-counter)

(defvar js2-parse-stmt-count 0)
(make-variable-buffer-local 'js2-parse-stmt-count)

(defsubst js2-get-next-temp-name ()
  (format "$%d" (incf js2-temp-name-counter)))

(defvar js2-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(defvar js2-statements-per-pause 50
  "Pause after this many statements to check for user input.
If user input is pending, stop the parse and discard the tree.
This makes for a smoother user experience for large files.
You may have to wait a second or two before the highlighting
and error-reporting appear, but you can always type ahead if
you wish.  This appears to be more or less how Eclipse, IntelliJ
and other editors work.")

(defvar js2-record-comments t
  "Instructs the scanner to record comments in `js2-scanned-comments'.")
(make-variable-buffer-local 'js2-record-comments)

(defvar js2-scanned-comments nil
  "List of all comments from the current parse.")
(make-variable-buffer-local 'js2-scanned-comments)

(defun js2-underline-color (color)
  "Return a legal value for the :underline face attribute based on COLOR."
  ;; In XEmacs the :underline attribute can only be a boolean.
  ;; In GNU it can be the name of a colour.
  (if (featurep 'xemacs)
      (if color t nil)
    color))

(defcustom js2-mode-indent-inhibit-undo nil
  "Non-nil to disable collection of Undo information when indenting lines.
Some users have requested this behavior.  It's nil by default because
other Emacs modes don't work this way."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-indent-ignore-first-tab nil
  "If non-nil, ignore first TAB keypress if we look indented properly.
It's fairly common for users to navigate to an already-indented line
and press TAB for reassurance that it's been indented.  For this class
of users, we want the first TAB press on a line to be ignored if the
line is already indented to one of the precomputed alternatives.

This behavior is only partly implemented.  If you TAB-indent a line,
navigate to another line, and then navigate back, it fails to clear
the last-indented variable, so it thinks you've already hit TAB once,
and performs the indent.  A full solution would involve getting on the
point-motion hooks for the entire buffer.  If we come across another
use cases that requires watching point motion, I'll consider doing it.

If you set this variable to nil, then the TAB key will always change
the indentation of the current line, if more than one alternative
indentation spot exists."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-indent-hook nil
  "A hook for user-defined indentation rules.

Functions on this hook should expect two arguments:    (LIST INDEX)
The LIST argument is the list of computed indentation points for
the current line.  INDEX is the list index of the indentation point
that `js2-bounce-indent' plans to use.  If INDEX is nil, then the
indent function is not going to change the current line indentation.

If a hook function on this list returns a non-nil value, then
`js2-bounce-indent' assumes the hook function has performed its own
indentation, and will do nothing.  If all hook functions on the list
return nil, then `js2-bounce-indent' will use its computed indentation
and reindent the line.

When hook functions on this hook list are called, the variable
`js2-mode-ast' may or may not be set, depending on whether the
parse tree is available.  If the variable is nil, you can pass a
callback to `js2-mode-wait-for-parse', and your callback will be
called after the new parse tree is built.  This can take some time
in large files.")

(defface js2-warning-face
  `((((class color) (background light))
     (:underline ,(js2-underline-color "orange")))
    (((class color) (background dark))
     (:underline ,(js2-underline-color "orange")))
    (t (:underline t)))
  "Face for JavaScript warnings."
  :group 'js2-mode)

(defface js2-error-face
  `((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for JavaScript errors."
  :group 'js2-mode)

(defface js2-jsdoc-tag-face
  '((t :foreground "SlateGray"))
  "Face used to highlight @whatever tags in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-type-face
  '((t :foreground "SteelBlue"))
  "Face used to highlight {FooBar} types in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-value-face
  '((t :foreground "PeachPuff3"))
  "Face used to highlight tag values in jsdoc comments."
  :group 'js2-mode)

(defface js2-function-param-face
  '((t :foreground "SeaGreen"))
  "Face used to highlight function parameters in javascript."
  :group 'js2-mode)

(defface js2-instance-member-face
  '((t :foreground "DarkOrchid"))
  "Face used to highlight instance variables in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-member-face
  '((t :foreground "PeachPuff3"))
  "Face used to highlight calls to private methods in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-function-call-face
  '((t :foreground "goldenrod"))
  "Face used to highlight calls to private functions in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-name-face
  (if js2-emacs22
      '((((class color) (min-colors 88) (background light))
         (:foreground "rosybrown"))
        (((class color) (min-colors 8) (background dark))
         (:foreground "yellow"))
        (((class color) (min-colors 8) (background light))
         (:foreground "magenta")))
    '((((type tty pc) (class color) (background light))
       (:foreground "magenta"))
      (((type tty pc) (class color) (background dark))
       (:foreground "yellow"))
      (t (:foreground "RosyBrown"))))
    "Face used to highlight jsdoc html tag names"
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-delimiter-face
  (if js2-emacs22
      '((((class color) (min-colors 88) (background light))
         (:foreground "dark khaki"))
        (((class color) (min-colors 8) (background dark))
         (:foreground "green"))
        (((class color) (min-colors 8) (background light))
         (:foreground "green")))
    '((((type tty pc) (class color) (background light))
       (:foreground "green"))
      (((type tty pc) (class color) (background dark))
       (:foreground "green"))
      (t (:foreground "dark khaki"))))
  "Face used to highlight brackets in jsdoc html tags."
  :group 'js2-mode)

(defface js2-external-variable-face
  '((t :foreground "orange"))
  "Face used to highlight assignments to undeclared variables.
An undeclared variable is any variable not declared with var or let
in the current scope or any lexically enclosing scope.  If you assign
to such a variable, then you are either expecting it to originate from
another file, or you've got a potential bug."
  :group 'js2-mode)

(defcustom js2-highlight-external-variables t
  "Non-nil to higlight assignments to undeclared variables."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-mode-map
  (let ((map (make-sparse-keymap))
        keys)
    (define-key map [mouse-1] #'js2-mode-show-node)
    (define-key map "\C-m" #'js2-enter-key)
    (when js2-rebind-eol-bol-keys
      (define-key map "\C-a" #'js2-beginning-of-line)
      (define-key map "\C-e" #'js2-end-of-line))
    (define-key map "\C-c\C-e" #'js2-mode-hide-element)
    (define-key map "\C-c\C-s" #'js2-mode-show-element)
    (define-key map "\C-c\C-a" #'js2-mode-show-all)
    (define-key map "\C-c\C-f" #'js2-mode-toggle-hide-functions)
    (define-key map "\C-c\C-t" #'js2-mode-toggle-hide-comments)
    (define-key map "\C-c\C-o" #'js2-mode-toggle-element)
    (define-key map "\C-c\C-w" #'js2-mode-toggle-warnings-and-errors)
    (define-key map (kbd "C-c C-'") #'js2-next-error)
    ;; also define user's preference for next-error, if available
    (if (setq keys (where-is-internal #'next-error))
        (define-key map (car keys) #'js2-next-error))
    (define-key map (or (car (where-is-internal #'mark-defun))
                        (kbd "M-C-h"))
      #'js2-mark-defun)
    (define-key map (or (car (where-is-internal #'narrow-to-defun))
                        (kbd "C-x nd"))
      #'js2-narrow-to-defun)
    (define-key map [down-mouse-3] #'js2-mouse-3)
    (when js2-auto-indent-flag
      (mapc (lambda (key)
              (define-key map key #'js2-insert-and-indent))
            js2-electric-keys))

    (define-key map [menu-bar javascript]
      (cons "JavaScript" (make-sparse-keymap "JavaScript")))

    (define-key map [menu-bar javascript customize-js2-mode]
      '(menu-item "Customize js2-mode" js2-mode-customize
                  :help "Customize the behavior of this mode"))

    (define-key map [menu-bar javascript js2-force-refresh]
      '(menu-item "Force buffer refresh" js2-mode-reset
                  :help "Re-parse the buffer from scratch"))

    (define-key map [menu-bar javascript separator-2]
      '("--"))

    (define-key map [menu-bar javascript next-error]
      '(menu-item "Next warning or error" js2-next-error
                  :enabled (and js2-mode-ast
                                (or (js2-ast-root-errors js2-mode-ast)
                                    (js2-ast-root-warnings js2-mode-ast)))
                  :help "Move to next warning or error"))

    (define-key map [menu-bar javascript display-errors]
      '(menu-item "Show errors and warnings" js2-mode-display-warnings-and-errors
                  :visible (not js2-mode-show-parse-errors)
                  :help "Turn on display of warnings and errors"))

    (define-key map [menu-bar javascript hide-errors]
      '(menu-item "Hide errors and warnings" js2-mode-hide-warnings-and-errors
                  :visible js2-mode-show-parse-errors
                  :help "Turn off display of warnings and errors"))

    (define-key map [menu-bar javascript separator-1]
      '("--"))

    (define-key map [menu-bar javascript js2-toggle-function]
      '(menu-item "Show/collapse element" js2-mode-toggle-element
                  :help "Hide or show function body or comment"))

    (define-key map [menu-bar javascript show-comments]
      '(menu-item "Show block comments" js2-mode-toggle-hide-comments
                  :visible js2-mode-comments-hidden
                  :help "Expand all hidden block comments"))

    (define-key map [menu-bar javascript hide-comments]
      '(menu-item "Hide block comments" js2-mode-toggle-hide-comments
                  :visible (not js2-mode-comments-hidden)
                  :help "Show block comments as /*...*/"))

    (define-key map [menu-bar javascript show-all-functions]
      '(menu-item "Show function bodies" js2-mode-toggle-hide-functions
                  :visible js2-mode-functions-hidden
                  :help "Expand all hidden function bodies"))

    (define-key map [menu-bar javascript hide-all-functions]
      '(menu-item "Hide function bodies" js2-mode-toggle-hide-functions
                  :visible (not js2-mode-functions-hidden)
                  :help "Show {...} for all top-level function bodies"))

    map)
  "Keymap used in `js2-mode' buffers.")

(defconst js2-mode-identifier-re "[a-zA-Z_$][a-zA-Z0-9_$]*")

(defvar js2-mode-//-comment-re "^\\(\\s-*\\)//.+"
  "Matches a //-comment line.  Must be first non-whitespace on line.
First match-group is the leading whitespace.")

(defvar js2-mode-ast nil "Private variable.")
(make-variable-buffer-local 'js2-mode-ast)

(defvar js2-mode-hook nil)

(defvar js2-mode-parse-timer nil "Private variable.")
(make-variable-buffer-local 'js2-mode-parse-timer)

(defvar js2-mode-buffer-dirty-p nil "Private variable.")
(make-variable-buffer-local 'js2-mode-buffer-dirty-p)

(defvar js2-mode-parsing nil "Private variable.")
(make-variable-buffer-local 'js2-mode-parsing)

(defvar js2-mode-node-overlay nil)
(make-variable-buffer-local 'js2-mode-node-overlay)

(defvar js2-mode-show-overlay js2-mode-dev-mode-p
  "Debug:  Non-nil to highlight AST nodes on mouse-down.")

(defvar js2-mode-fontifications nil "Private variable")
(make-variable-buffer-local 'js2-mode-fontifications)

(defvar js2-mode-deferred-properties nil "Private variable")
(make-variable-buffer-local 'js2-mode-deferred-properties)

(defvar js2-imenu-recorder nil "Private variable")
(make-variable-buffer-local 'js2-imenu-recorder)

(defvar js2-imenu-function-map nil "Private variable")
(make-variable-buffer-local 'js2-imenu-function-map)

(defvar js2-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")

;; Note that we also set a 'c-in-sws text property in html comments,
;; so that `c-forward-sws' and `c-backward-sws' work properly.
(defvar js2-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar js2-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar js2-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from java-mode.  Needed for some cc-engine functions.")

(defvar js2-comment-prefix-regexp
  "//+\\|\\**")

(defvar js2-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar js2-mode-verbose-parse-p js2-mode-dev-mode-p
  "Non-nil to emit status messages during parsing.")

(defvar js2-mode-functions-hidden nil "private variable")
(defvar js2-mode-comments-hidden nil "private variable")

(defvar js2-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in js2-mode buffers.")

(defvar js2-mode-abbrev-table nil
  "Abbrev table in use in `js2-mode' buffers.")
(define-abbrev-table 'js2-mode-abbrev-table ())

(defvar js2-mode-must-byte-compile (not js2-mode-dev-mode-p)
  "Non-nil to have `js2-mode' signal an error if not byte-compiled.")

(defvar js2-mode-pending-parse-callbacks nil
  "List of functions waiting to be notified that parse is finished.")

(defvar js2-mode-last-indented-line -1)

(eval-when-compile
  (defvar c-paragraph-start nil)
  (defvar c-paragraph-separate nil)
  (defvar c-syntactic-ws-start nil)
  (defvar c-syntactic-ws-end nil)
  (defvar c-syntactic-eol nil)
  (defvar running-xemacs nil)
  (defvar font-lock-mode nil)
  (defvar font-lock-keywords nil))

(eval-when-compile
  (if (< emacs-major-version 22)
      (defun c-setup-paragraph-variables () nil)))

(provide 'js2-vars)

;;; js2-vars.el ends here
