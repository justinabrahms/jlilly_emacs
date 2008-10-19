;;; js2-browse.el --- browsing/hierarchy support for js2-mode

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
;;
;; We currently only support imenu, but eventually should support speedbar and
;; possibly other browsing mechanisms.
;;
;; The basic strategy is to identify function assignment targets of the form
;; `foo.bar.baz', convert them to (list foo bar baz <position>), and push the
;; list into `js2-imenu-recorder'.  The lists are merged into a trie-like tree
;; for imenu after parsing is finished.
;;
;; A `foo.bar.baz' assignment target may be expressed in many ways in
;; JavaScript, and the general problem is undecidable.  However, several forms
;; are readily recognizable at parse-time; the forms we attempt to recognize
;; include:
;;
;;  function foo()  -- function declaration
;;  foo = function()  -- function expression assigned to variable
;;  foo.bar.baz = function()  -- function expr assigned to nested property-get
;;  foo = {bar: function()}  -- fun prop in object literal assigned to var
;;  foo = {bar: {baz: function()}} -- inside nested object literal
;;  foo.bar = {baz: function()}} -- obj lit assigned to nested prop get
;;  a.b = {c: {d: function()}} -- nested obj lit assigned to nested prop get
;;  foo = {get bar() {...}}  -- getter/setter in obj literal
;;  function foo() {function bar() {...}}  -- nested function
;;  foo['a'] = function()  -- fun expr assigned to deterministic element-get
;;
;; This list boils down to a few forms that can be combined recursively.
;; Top-level named function declarations include both the left-hand (name)
;; and the right-hand (function value) expressions needed to produce an imenu
;; entry.  The other "right-hand" forms we need to look for are:
;;  - functions declared as props/getters/setters in object literals
;;  - nested named function declarations
;; The "left-hand" expressions that functions can be assigned to include:
;;  - local/global variables
;;  - nested property-get expressions like a.b.c.d
;;  - element gets like foo[10] or foo['bar'] where the index
;;    expression can be trivially converted to a property name.  They
;;    effectively then become property gets.
;;
;; All the different definition types are canonicalized into the form
;; foo.bar.baz = position-of-function-keyword
;;
;; We need to build a trie-like structure for imenu.  As an example,
;; consider the following JavaScript code:
;;
;; a = function() {...}  // function at position 5
;; b = function() {...}  // function at position 25
;; foo = function() {...} // function at position 100
;; foo.bar = function() {...} // function at position 200
;; foo.bar.baz = function() {...} // function at position 300
;; foo.bar.zab = function() {...} // function at position 400
;;
;; During parsing we accumulate an entry for each definition in
;; the variable `js2-imenu-recorder', like so:
;;
;; '((a 5)
;;   (b 25)
;;   (foo 100)
;;   (foo bar 200)
;;   (foo bar baz 300)
;;   (foo bar zab 400))
;;
;; After parsing these entries are merged into this alist-trie:
;;
;; '((a . 1)
;;   (b . 2)
;;   (foo (<definition> . 3)
;;        (bar (<definition> . 6)
;;             (baz . 100)
;;             (zab . 200))))
;;
;; Note the wacky need for a <definition> name.  The token can be anything
;; that isn't a valid JavaScript identifier, because you might make foo
;; a function and then start setting properties on it that are also functions.

;;; Code:

(require 'js2-ast)

(defsubst js2-prop-node-name (node)
  "Return the name of a node that may be a property-get/property-name.
If NODE is not a valid name-node, string-node or integral number-node,
returns nil.  Otherwise returns the string name/value of the node."
  (cond
   ((js2-name-node-p node)
    (js2-name-node-name node))
   ((js2-string-node-p node)
    (js2-string-node-value node))
   ((and (js2-number-node-p node)
         (string-match "^[0-9]+$" (js2-number-node-value node)))
    (js2-number-node-value node))
   ((js2-this-node-p node)
    "this")))
      
(defsubst js2-node-qname-component (node)
  "Test function:  return the name of this node, if it contributes to a qname.
Returns nil if the node doesn't contribute."
  (copy-sequence
   (or (js2-prop-node-name node)
       (if (and (js2-function-node-p node)
                (js2-function-node-name node))
           (js2-name-node-name (js2-function-node-name node))))))
           
(defsubst js2-record-function-qname (fn-node qname)
  "Associate FN-NODE with its QNAME for later lookup.
This is used in postprocessing the chain list.  When we find a chain
whose first element is a js2-THIS keyword node, we look up the parent
function and see (using this map) whether it is the tail of a chain.
If so, we replace the this-node with a copy of the parent's qname."
  (unless js2-imenu-function-map
    (setq js2-imenu-function-map (make-hash-table :test 'eq)))
  (puthash fn-node qname js2-imenu-function-map))

(defun js2-record-imenu-functions (node &optional var)
  "Record function definitions for imenu.
NODE is a function node or an object literal.
VAR, if non-nil, is the expression that NODE is being assigned to."
  (when js2-parse-ide-mode
    (let ((fun-p (js2-function-node-p node))
          qname left fname-node pos)
      (cond
       ;; non-anonymous function declaration?
       ((and fun-p
             (not var)
             (setq fname-node (js2-function-node-name node)))
        (push (setq qname (list fname-node (js2-node-pos node)))
              js2-imenu-recorder)
        (js2-record-function-qname node qname))

       ;; for remaining forms, compute left-side tree branch first
       ((and var (setq qname (js2-compute-nested-prop-get var)))
        (cond
         ;; foo.bar.baz = function
         (fun-p
          (push (nconc qname (list (js2-node-pos node)))
                js2-imenu-recorder)
          (js2-record-function-qname node qname))
         ;; foo.bar.baz = object-literal
         ;; look for nested functions:  {a: {b: function() {...} }}
         ((js2-object-node-p node)
          (js2-record-object-literal node qname))))))))

(defun js2-compute-nested-prop-get (node)
  "If NODE is of form foo.bar.baz, return component nodes as a list.
Otherwise returns nil.  Element-gets can be treated as property-gets
if the index expression is a name, a string, or a positive integer."
  (let (left right head)
    (cond
     ((or (js2-name-node-p node)
          (js2-this-node-p node))
      (list node))
     ;; foo.bar.baz is parenthesized as (foo.bar).baz => right operand is a leaf
     ((js2-prop-get-node-p node)  ; includes elem-get nodes
      (setq left (js2-prop-get-node-left node)
            right (js2-prop-get-node-right node))
      (if (and (or (js2-prop-get-node-p left)     ; left == foo.bar
                   (js2-name-node-p left)
                   (js2-this-node-p left))        ; or left == foo
               (or (js2-name-node-p right)        ; .bar
                   (js2-string-node-p right)      ; ['bar']
                   (and (js2-number-node-p right) ; [10]
                        (string-match "^[0-9]+$"
                                      (js2-number-node-value right)))))
          (if (setq head (js2-compute-nested-prop-get left))
              (nconc head (list right))))))))

(defun js2-record-object-literal (node qname)
  "Recursively process an object literal looking for functions.
NODE is an object literal that is the right-hand child of an assignment
expression.  QNAME is a list of nodes representing the assignment target,
e.g. for foo.bar.baz = {...}, QNAME is (foo-node bar-node baz-node).
We do a depth-first traversal of NODE.  Any functions we find are prefixed
with QNAME plus the property name of the function and appended to the
variable `js2-imenu-recorder'."
  ;; Elements are relative to parent position, which is still absolute,
  ;; since the parser passes the assignment target and value expressions
  ;; to us before they are added as children of the assignment node.
  (let ((pos (js2-node-pos node))
        left right)
    (dolist (e (js2-object-node-elems node))  ; e is a `js2-object-prop-node'
      (setq left (js2-infix-node-left e))
      (cond
       ;; foo: function() {...}
       ((js2-function-node-p (setq right (js2-infix-node-right e)))
        (when (js2-prop-node-name left)
          ;; As a policy decision, we record the position of the property,
          ;; not the position of the `function' keyword, since the property
          ;; is effectively the name of the function.
          (push (append qname (list left) (list (+ pos (js2-node-pos e))))
                js2-imenu-recorder)
          (js2-record-function-qname right qname)))
       ;; foo: {object-literal} -- add foo to qname and recurse
       ((js2-object-node-p right)
        (js2-record-object-literal right
                                   (append qname (list (js2-infix-node-left e)))))))))

(defsubst js2-node-top-level-decl-p (node)
  "Return t if NODE's name is defined in the top-level scope.
Also returns t if NODE's name is not defined in any scope, since it implies
that it's an external variable, which must also be in the top-level scope."
  (let* ((name (js2-prop-node-name node))
         (this-scope (js2-node-get-enclosing-scope node))
         defining-scope)
    (cond
     ((js2-this-node-p node)
      nil)
     ((null this-scope)
      t)
     ((setq defining-scope (js2-get-defining-scope this-scope name))
      (js2-ast-root-p defining-scope))
     (t t))))

(defun js2-browse-postprocess-chains (chains)
  "Modify function-declaration name chains after parsing finishes.
Some of the information is only available after the parse tree is complete.
For instance, following a 'this' reference requires a parent function node."
  (let (result head fn parent-chain p elem)
    (dolist (chain chains)
      ;; examine the head of each node to get its defining scope
      (setq head (car chain))
      (cond
       ;; if top-level/external, keep as-is
       ((js2-node-top-level-decl-p head)
        (push chain result))
       ;; check for a this-reference
       ((eq (js2-node-type head) js2-THIS)
        (setq fn (js2-node-parent-script-or-fn head))
        ;; if there is no parent function, or if the parent function
        ;; is nested, discard the head node and keep the rest of the chain.
        (if (or (null fn) (js2-nested-function-p fn))
            (push (cdr chain) result)
          ;; else look up parent in function-map.  If not found, discard chain.
          (when (setq parent-chain (and js2-imenu-function-map
                                        (gethash fn js2-imenu-function-map)))
            ;; else discard head node and prefix parent fn qname, which is
            ;; the parent-chain sans tail, to this chain.
            (push (append (butlast parent-chain) (cdr chain)) result))))))
    ;; finally replace each node in each chain with its name.
    (dolist (chain result)
      (setq p chain)
      (while p
        (if (js2-node-p (setq elem (car p)))
            (setcar p (js2-node-qname-component elem)))
        (setq p (cdr p))))
    result))

;; Merge name chains into a trie-like tree structure of nested lists.
;; To simplify construction of the trie, we first build it out using the rule
;; that the trie consists of lists of pairs.  Each pair is a 2-element array:
;; [key, num-or-list].  The second element can be a number; if so, this key
;; is a leaf-node with only one value.  (I.e. there is only one declaration
;; associated with the key at this level.)  Otherwise the second element is
;; a list of pairs, with the rule applied recursively.  This symmetry permits
;; a simple recursive formulation.
;;
;; js2-mode is building the data structure for imenu.  The imenu documentation
;; claims that it's the structure above, but in practice it wants the children
;; at the same list level as the key for that level, which is how I've drawn
;; the "Expected final result" above.  We'll postprocess the trie to remove the
;; list wrapper around the children at each level.
;;
;; A completed nested imenu-alist entry looks like this:
;;       '(("foo"
;;          ("<definition>" . 7)
;;          ("bar"
;;           ("a" . 40)
;;           ("b" . 60))))
;;
;; In particular, the documentation for `imenu--index-alist' says that
;; a nested sub-alist element looks like (INDEX-NAME SUB-ALIST).
;; The sub-alist entries immediately follow INDEX-NAME, the head of the list.

(defsubst js2-treeify (lst)
  "Convert (a b c d) to (a ((b ((c d)))))"
  (if (null (cddr lst))  ; list length <= 2
      lst
    (list (car lst) (list (js2-treeify (cdr lst))))))
          
(defun js2-build-alist-trie (chains trie)
  "Merge declaration name chains into a trie-like alist structure for imenu.
CHAINS is the qname chain list produced during parsing. TRIE is a
list of elements built up so far."
  (let (head tail pos branch kids)
    (dolist (chain chains)
      (setq head (car chain)
            tail (cdr chain)
            pos (if (numberp (car tail)) (car tail))
            branch (js2-find-if (lambda (n)
                                  (string= (car n) head))
                                trie)
            kids (second branch))
      (cond
       ;; case 1:  this key isn't in the trie yet
       ((null branch)
        (if trie
            (setcdr (last trie) (list (js2-treeify chain)))
          (setq trie (list (js2-treeify chain)))))

       ;; case 2:  key is present with a single number entry:  replace w/ list
       ;;  ("a1" 10)  +  ("a1" 20) => ("a1" (("<definition>" 10)
       ;;                                    ("<definition>" 20)))
       ((numberp kids)
        (setcar (cdr branch)
                (list (list "<definition-1>" kids)
                      (if pos
                          (list "<definition-2>" pos)
                        (js2-treeify tail)))))

       ;; case 3:  key is there (with kids), and we're a number entry
       (pos
        (setcdr (last kids)
                (list
                 (list (format "<definition-%d>" 
                               (1+ (loop for kid in kids
                                         count (eq ?< (aref (car kid) 0)))))
                       pos))))
                                   
       ;; case 4:  key is there with kids, need to merge in our chain
       (t
        (js2-build-alist-trie (list tail) kids))))
    trie))

(defun js2-flatten-trie (trie)
  "Convert TRIE to imenu-format.
Recurses through nodes, and for each one whose second element is a list,
appends the list's flattened elements to the current element.  Also
changes the tails into conses.  For instance, this pre-flattened trie

'(a ((b 20)
     (c ((d 30)
         (e 40)))))

becomes

'(a (b . 20)
    (c (d . 30)
       (e . 40)))

Note that the root of the trie has no key, just a list of chains.
This is also true for the value of any key with multiple children,
e.g. key 'c' in the example above."
  (cond
   ((listp (car trie))
    (mapcar #'js2-flatten-trie trie))
   (t
    (if (numberp (second trie))
        (cons (car trie) (second trie))
      ;; else pop list and append its kids
      (apply #'append (list (car trie)) (js2-flatten-trie (cdr trie)))))))

(defun js2-build-imenu-index ()
  "Turn `js2-imenu-recorder' into an imenu data structure."
  (unless (eq js2-imenu-recorder 'empty)
    (let* ((chains (js2-browse-postprocess-chains js2-imenu-recorder))
           (result (js2-build-alist-trie chains nil)))
      (js2-flatten-trie result))))

(defun js2-test-print-chains (chains)
  "Print a list of qname chains.
Each element of CHAINS is a list of the form (NODE [NODE *] pos);
i.e. one or more nodes, and an integer position as the list tail."
  (mapconcat (lambda (chain)
               (concat "("
                       (mapconcat (lambda (elem)
                                    (if (js2-node-p elem)
                                        (or (js2-node-qname-component elem)
                                            "nil")
                                      (number-to-string elem)))
                                  chain
                                  " ")
                       ")"))
             chains
             "\n"))


(provide 'js2-browse)

;;; js2-browse.el ends here
