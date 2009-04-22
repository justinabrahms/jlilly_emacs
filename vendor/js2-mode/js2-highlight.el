;;; js2-highlight.el --- JavaScript syntax coloring support

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

(require 'js2-ast)

(defsubst js2-set-face (beg end face &optional record)
  "Fontify a region.  If RECORD is non-nil, record for later."
  (when (plusp js2-highlight-level)
    (setq beg (min (point-max) beg)
          beg (max (point-min) beg)
          end (min (point-max) end)
          end (max (point-min) end))
    (if record
        (push (list beg end face) js2-mode-fontifications)
      (put-text-property beg end 'face face))))

(defsubst js2-set-kid-face (pos kid len face)
  "Set-face on a child node.
POS is absolute buffer position of parent.
KID is the child node.
LEN is the length to fontify.
FACE is the face to fontify with."
  (js2-set-face (+ pos (js2-node-pos kid))
                (+ pos (js2-node-pos kid) (js2-node-len kid))
                face))

(defsubst js2-fontify-kwd (start length)
  (js2-set-face start (+ start length) 'font-lock-keyword-face))

(defsubst js2-clear-face (beg end)
  (remove-text-properties beg end '(face nil
                                    help-echo nil
                                    point-entered nil
                                    c-in-sws nil)))

(defsubst js2-record-text-property (beg end prop value)
  "Record a text property to set when parsing finishes."
  (push (list beg end prop value) js2-mode-deferred-properties))

(defconst js2-ecma-global-props
  (concat "^"
          (regexp-opt
           '("Infinity" "NaN" "undefined" "arguments") t)
          "$")
  "Value properties of the Ecma-262 Global Object.
Shown at or above `js2-highlight-level' 2.")

;; might want to add the name "arguments" to this list?
(defconst js2-ecma-object-props
  (concat "^"
          (regexp-opt
           '("prototype" "__proto__" "__parent__") t)
          "$")
  "Value properties of the Ecma-262 Object constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-global-funcs
  (concat
   "^"
   (regexp-opt
    '("decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
      "eval" "isFinite" "isNaN" "parseFloat" "parseInt") t)
   "$")
  "Function properties of the Ecma-262 Global object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-number-props
  (concat "^"
          (regexp-opt '("MAX_VALUE" "MIN_VALUE" "NaN"
                        "NEGATIVE_INFINITY"
                        "POSITIVE_INFINITY") t)
          "$")
  "Properties of the Ecma-262 Number constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-date-props "^\\(parse\\|UTC\\)$"
  "Properties of the Ecma-262 Date constructor.
Shown at or above `js2-highlight-level' 2.")


(defconst js2-ecma-math-props
  (concat "^"
          (regexp-opt
           '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2")
           t)
          "$")
  "Properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")


(defconst js2-ecma-math-funcs
  (concat "^"
          (regexp-opt
           '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
             "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
          "$")
  "Function properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-function-props
  (concat
   "^"
   (regexp-opt
    '(;; properties of the Object prototype object
      "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
      "toLocaleString" "toString" "valueOf"
      ;; properties of the Function prototype object
      "apply" "call"
      ;; properties of the Array prototype object
      "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
      "splice" "unshift"
      ;; properties of the String prototype object
      "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
      "localeCompare" "match" "replace" "search" "split" "substring"
      "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
      "toUpperCase"
      ;; properties of the Number prototype object
      "toExponential" "toFixed" "toPrecision"
      ;; properties of the Date prototype object
      "getDate" "getDay" "getFullYear" "getHours" "getMilliseconds"
      "getMinutes" "getMonth" "getSeconds" "getTime"
      "getTimezoneOffset" "getUTCDate" "getUTCDay" "getUTCFullYear"
      "getUTCHours" "getUTCMilliseconds" "getUTCMinutes" "getUTCMonth"
      "getUTCSeconds" "setDate" "setFullYear" "setHours"
      "setMilliseconds" "setMinutes" "setMonth" "setSeconds" "setTime"
      "setUTCDate" "setUTCFullYear" "setUTCHours" "setUTCMilliseconds"
      "setUTCMinutes" "setUTCMonth" "setUTCSeconds" "toDateString"
      "toLocaleDateString" "toLocaleString" "toLocaleTimeString"
      "toTimeString" "toUTCString"
      ;; properties of the RegExp prototype object
      "exec" "test"
      ;; SpiderMonkey/Rhino extensions, versions 1.5+
      "toSource" "__defineGetter__" "__defineSetter__"
      "__lookupGetter__" "__lookupSetter__" "__noSuchMethod__"
      "every" "filter" "forEach" "lastIndexOf" "map" "some")
    t)
   "$")
  "Built-in functions defined by Ecma-262 and SpiderMonkey extensions.
Shown at or above `js2-highlight-level' 3.")

(defsubst js2-parse-highlight-prop-get (parent target prop call-p)
  (let ((target-name (and target
                          (js2-name-node-p target)
                          (js2-name-node-name target)))
        (prop-name (if prop (js2-name-node-name prop)))
        (level1 (>= js2-highlight-level 1))
        (level2 (>= js2-highlight-level 2))
        (level3 (>= js2-highlight-level 3))
        pos
        face)
    (when level2
      (if call-p
          (cond
           ((and target prop)
            (cond
             ((and level3 (string-match js2-ecma-function-props prop-name))
              (setq face 'font-lock-builtin-face))
             ((and target-name prop)
              (cond
               ((string= target-name "Date")
                (if (string-match js2-ecma-date-props prop-name)
                    (setq face 'font-lock-builtin-face)))
               ((string= target-name "Math")
                (if (string-match js2-ecma-math-funcs prop-name)
                    (setq face 'font-lock-builtin-face)))))))
           (prop
            (if (string-match js2-ecma-global-funcs prop-name)
                (setq face 'font-lock-builtin-face))))
        (cond
         ((and target prop)
          (cond
           ((string= target-name "Number")
            (if (string-match js2-ecma-number-props prop-name)
                (setq face 'font-lock-constant-face)))
           ((string= target-name "Math")
            (if (string-match js2-ecma-math-props prop-name)
                (setq face 'font-lock-constant-face)))))
         (prop
          (if (string-match js2-ecma-object-props prop-name)
              (setq face 'font-lock-constant-face)))))
      (when face
        (js2-set-face (setq pos (+ (js2-node-pos parent) ; absolute
                                   (js2-node-pos prop))) ; relative
                      (+ pos (js2-node-len prop))
                      face)))))

(defun js2-parse-highlight-member-expr-node (node)
  "Perform syntax highlighting of EcmaScript built-in properties.
The variable `js2-highlight-level' governs this highighting."
  (let (face target prop name pos end parent call-p callee)
    (cond
     ;; case 1:  simple name, e.g. foo
     ((js2-name-node-p node)
      (setq name (js2-name-node-name node))
      ;; possible for name to be nil in rare cases - saw it when
      ;; running js2-mode on an elisp buffer.  Might as well try to
      ;; make it so js2-mode never barfs.
      (when name
        (setq face (if (string-match js2-ecma-global-props name)
                       'font-lock-constant-face))
        (when face
          (setq pos (js2-node-pos node)
                end (+ pos (js2-node-len node)))
          (js2-set-face pos end face))))

     ;; case 2:  property access or function call
     ((or (js2-prop-get-node-p node)
          ;; highlight function call if expr is a prop-get node
          ;; or a plain name (i.e. unqualified function call)
          (and (setq call-p (js2-call-node-p node))
               (setq callee (js2-call-node-target node)) ; separate setq!
               (or (js2-prop-get-node-p callee)
                   (js2-name-node-p callee))))
      (setq parent node
            node (if call-p callee node))
      (if (and call-p (js2-name-node-p callee))
          (setq prop callee)
        (setq target (js2-prop-get-node-left node)
              prop (js2-prop-get-node-right node)))
      (cond
       ((js2-name-node-p target)
        (if (js2-name-node-p prop)
            ;; case 2a:  simple target, simple prop name, e.g. foo.bar
            (js2-parse-highlight-prop-get parent target prop call-p)
          ;; case 2b:  simple target, complex name, e.g. foo.x[y]
          (js2-parse-highlight-prop-get parent target nil call-p)))
       ((js2-name-node-p prop)
        ;; case 2c:  complex target, simple name, e.g. x[y].bar
        (js2-parse-highlight-prop-get parent target prop call-p)))))))

(defun js2-parse-highlight-member-expr-fn-name (expr)
  "Highlight the `baz' in function foo.bar.baz(args) {...}.
This is experimental Rhino syntax.  EXPR is the foo.bar.baz member expr.
We currently only handle the case where the last component is a prop-get
of a simple name.  Called before EXPR has a parent node."
  (let (pos
        (name (and (js2-prop-get-node-p expr)
                   (js2-prop-get-node-right expr))))
    (when (js2-name-node-p name)
      (js2-set-face (setq pos (+ (js2-node-pos expr)  ; parent is absolute
                                 (js2-node-pos name)))
                    (+ pos (js2-node-len name))
                    'font-lock-function-name-face
                    'record))))

;; source:  http://jsdoc.sourceforge.net/
;; Note - this syntax is for Google's enhanced jsdoc parser that
;; allows type specifications, and needs work before entering the wild.

(defconst js2-jsdoc-param-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@"
          "\\(?:param\\|argument\\)"
          "\\)"
          "\\s-*\\({[^}]+}\\)?"         ; optional type
          "\\s-*\\([a-zA-Z0-9_$]+\\)?"  ; name
          "\\>")
  "Matches jsdoc tags with optional type and optional param name.")

(defconst js2-jsdoc-typed-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("requires" "return" "returns" "throw" "throws"))
          "\\)\\)\\s-*\\({[^}]+}\\)?")
  "Matches jsdoc tags with optional type.")

(defconst js2-jsdoc-arg-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("base" "extends" "member" "type" "version"))
          "\\)\\)\\s-+\\([^ \t]+\\)")
  "Matches jsdoc tags with a single argument.")

(defconst js2-jsdoc-empty-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("addon" "author" "class" "constructor" "deprecated" "exec"
             "exception" "fileoverview" "final" "ignore" "private"))
          "\\)\\)\\s-*")
  "Matches empty jsdoc tags.")
  
(defconst js2-jsdoc-link-tag-regexp
  "{\\(@link\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?}"
  "Matches a jsdoc link tag.")

(defconst js2-jsdoc-see-tag-regexp
  "^\\s-*\\*+\\s-*\\(@see\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?"
  "Matches a jsdoc @see tag.")

(defconst js2-jsdoc-html-tag-regexp
  "\\(</?\\)\\([a-zA-Z]+\\)\\s-*\\(/?>\\)"
  "Matches a simple (no attributes) html start- or end-tag.")

(defsubst js2-jsdoc-highlight-helper ()
  (js2-set-face (match-beginning 1)
                (match-end 1)
                'js2-jsdoc-tag-face)
  (if (match-beginning 2)
      (if (save-excursion
            (goto-char (match-beginning 2))
            (= (char-after) ?{))
          (js2-set-face (1+ (match-beginning 2))
                        (1- (match-end 2))
                        'js2-jsdoc-type-face)
        (js2-set-face (match-beginning 2)
                      (match-end 2)
                      'js2-jsdoc-value-face)))
  (if (match-beginning 3)
      (js2-set-face (match-beginning 3)
                    (match-end 3)
                    'js2-jsdoc-value-face)))

(defun js2-highlight-jsdoc (ast)
  "Highlight doc comment tags."
  (let ((comments (js2-ast-root-comments ast))
        beg end)
    (save-excursion
      (dolist (node comments)
        (when (eq (js2-comment-node-format node) 'jsdoc)
          (setq beg (js2-node-abs-pos node)
                end (+ beg (js2-node-len node)))
          (save-restriction
            (narrow-to-region beg end)
            (dolist (re (list js2-jsdoc-param-tag-regexp
                              js2-jsdoc-typed-tag-regexp
                              js2-jsdoc-arg-tag-regexp
                              js2-jsdoc-link-tag-regexp
                              js2-jsdoc-see-tag-regexp
                              js2-jsdoc-empty-tag-regexp))
              (goto-char beg)
              (while (re-search-forward re nil t)
                (js2-jsdoc-highlight-helper)))
            ;; simple highlighting for html tags
            (goto-char beg)
            (while (re-search-forward js2-jsdoc-html-tag-regexp nil t)
              (js2-set-face (match-beginning 1)
                            (match-end 1)
                            'js2-jsdoc-html-tag-delimiter-face)
              (js2-set-face (match-beginning 2)
                            (match-end 2)
                            'js2-jsdoc-html-tag-name-face)
              (js2-set-face (match-beginning 3)
                            (match-end 3)
                            'js2-jsdoc-html-tag-delimiter-face))))))))

(defun js2-highlight-assign-targets (node left right)
  "Highlight function properties and external variables."
  (let (leftpos end name)
    ;; highlight vars and props assigned function values
    (when (js2-function-node-p right)
      (cond
       ;; var foo = function() {...}
       ((js2-name-node-p left)
        (setq name left))

       ;; foo.bar.baz = function() {...}
       ((and (js2-prop-get-node-p left)
             (js2-name-node-p (js2-prop-get-node-right left)))
        (setq name (js2-prop-get-node-right left))))

      (when name
        (js2-set-face (setq leftpos (js2-node-abs-pos name))
                      (+ leftpos (js2-node-len name))
                      'font-lock-function-name-face
                      'record)))

    ;; save variable assignments so we can check for undeclared later
    ;; (can't do it here since var decls can come at end of script)
    (when (and js2-highlight-external-variables
               (setq name (js2-member-expr-leftmost-name left)))
      (push (list name js2-current-scope
                  (setq leftpos (js2-node-abs-pos name))
                  (setq end (+ leftpos (js2-node-len name))))
            js2-recorded-assignments))))

(defun js2-highlight-undeclared-vars ()
  "After entire parse is finished, look for undeclared variable assignments.
Have to wait until entire buffer is parsed, since JavaScript permits var
decls to occur after they're used.

We currently use a simple heuristic to rule out complaining about built-ins:
if the name is capitalized we don't highlight it.  This could be improved a
bit by declaring all the Ecma global object, constructor and function names
in a hashtable, but we'd still wind up complaining about all the DHTML
builtins, the Mozilla builtins, etc."
  (let (name first-char)
    (dolist (entry js2-recorded-assignments)
      (destructuring-bind (name-node scope pos end) entry
        (setq name (js2-name-node-name name-node)
              first-char (aref name 0))
        (unless (or (and (>= first-char ?A) (<= first-char ?Z))
                    (js2-get-defining-scope scope name))
          (js2-set-face pos end 'js2-external-variable-face 'record)
          (js2-record-text-property pos end 'help-echo "Undeclared variable")
          (js2-record-text-property pos end 'point-entered #'js2-echo-help))))
    (setq js2-recorded-assignments nil)))
                                  
(provide 'js2-highlight)

;;; js2-highlight.el ends here
