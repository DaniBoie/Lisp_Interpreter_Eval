;; To run this, load this file into the lisp interpreter, then call
;; (my-top)
;; then type in expressions to be evaluated.

;; The homework assignment gives a list of good test cases
;; listed in order that you should probably implement thim in to ensure you
;; enough working to go on to the next test case.

;; We will use association lists, alists, for the stack of variables and
;; their values.  An alist is a list of this form:
;; ((var1 . val1) (var2 . val2) ... (varN . valN))
;; where each vari is a symbol representing a variable (or parameter) name
;; and each vali is the value of the variable.  assoc returns the association
;; of a given symbol, e.g,
;; (assoc 'myvar '((a . 10)(b a b c)(myvar d e f)))
;; returns (myvar d e f) and you take the cdr of that to get myvar's value
;; (d e f)

;; Assoc returns the association (binding) of a variable in the association
;; list. An association list may contain multiple definitions of a variable
;; with the same name, for example with parameters to a recursive function.
;; Assoc always finds the first association of a variable, and this is how we
;; implement dynamic scoping. 

;; As evaluation proceeds deeper into recursion, new variables are added onto
;; the front of the current association list.  New defintions of a variable 
;; will hide previously made definitions effectively hiding them from access.
;; The previously made definitions will come back into scope when
;; recursive evaluation unwinds.

;; We us a special global variable, called global-alist, for saving top-level
;; definitions made using defun or setq. Note the global-alist is passed in
;; to my-eval only in the call made by my-top defined below.


;; You need to write this one.

(defun my-assoc (v alist) ;; Takes Atom A and association list ALIST : Returns the association pair for A in ALIST. If there is no association, it returns nil
    (cond
    ((null alist) ;; Base Case : If null association list return nil
      nil
    )
    ((equal v (caar alist)) ;; If atom A == current alist.key return (alist.key, alist.value)
      (car alist)
    )
    (T ;; else recurse down searching for match
      (my-assoc v (cdr alist))
    )
  )
)

;; This one is done

(defun my-eval (e alist)
    ;;(print "MY-EVAL CALL TO:")
    ;;(print e)
    (cond ((atom e) (my-eval-atom e alist))
          (t (my-apply (car e) (cdr e) alist))
    )
)

;; You need to write this one.
;; !!
(defun my-eval-atom (e alist)
;; how do you evaluate an atom???
;; Remember there are special cases: T, NIL, MY-SYMBOL, 10, "Hello"
  (cond
  
    ((eq T e)
      T
    )
    ((null e)
      nil
    )
    ((stringp e)
      e
    )
    ((numberp e)
      e
    )
    ((symbolp e)
      (cond
        ((equal nil (my-assoc e alist))
          nil
        )
        (T
          (cdr (my-assoc e alist))
        )
      )
    )
  )
)

;; This one is done, but you must write the functions it calls

(defun my-apply (fn args alist)
    (cond ((atom fn) (my-apply-atom fn args alist))
          ( t (my-apply-lambda fn args alist)))
)

;; You need to write this one.
;; Utility function for eval-cond and apply-lambda.  Evaluates each expression
;; in l and returns the value of the last expression

(defun my-eval-list (l alist) ;; STILL NOT TESTED IN IMPLEMENTATION PHASE
  (cond

    ((null l)
      nil
    )
    ((null (cdr l))
      (my-eval (car l) alist)
    )
    (T
      (my-eval (car l) alist)
      (my-eval-list (cdr l) alist)
    )
  )
)

;; You need to write this one.

(defun my-apply-lambda (fn args alist)
;; bind the formals to the evaluated actuals then evaluate the body in that
;; new scoping context (i.e., that becomes the new alist for recursive
;; evaluation of the function body.  Return the value of the last 
;; expression in the body (using eval-list)).

  ; (print "my-apply-lambda")
  ; (print (car fn))
  ; (print (cdr fn))
  ; (print args)
  
  
  (my-eval-list (cdr fn) (my-bind-formals (car fn) args alist))

)

;; You need to write this one.

(defun my-bind-formals (formals actuals alist)
;; This takes a list of formals and unevaluated actuals.  It should evaluate
;; each actual and bind it to its corresponding formal placing them all on
;; the front of the alist.  It should return the alist with the new bindings
;; on the front.  This will be used to evaluate calls to functions defined
;; via defun.
;; e.g., (my-bind-formals '(a c) '((add 1 b) (add 2 b)) '((b . 10)))
;; will return ((a . 11) (b . 10))
;; Note there will be one actual parameter for each formal parameter.
  (cond
    ((null formals)
      alist
    )
    (T
      (my-bind-formals 
        (cdr formals) 
        (cdr actuals) 
        (cons (cons (car formals) (my-eval (car actuals) alist)) alist) 
      )
      ;;(cons 
      ;;  (cons (car formals) (my-eval (car actuals) alist))
      ;;  (my-bind-formals (cdr formals) (cdr actuals) alist)
      ;;)
    )
  )
)

;; You need to write this one.  Handle the primitives as special cases, then
;; handle user defined functions (defined via defun) in the default case.
;; Handle car, cdr, cons, eq, quote, cond, defun, setq, eval, print, atom, null,
;; listp, apply, equal, +, -, mod, floor and user defined functions (defined via defun).
;; This should allow you to interpret your functions from HW4.

(defun my-apply-atom (fn args alist)
    (cond ((eq fn 'eq)
            (eq (my-eval (car args) alist) (my-eval (cadr args) alist))
          )
          ;; I wrote the first one, eq, for you, you write the rest
          ((eq fn 'car)
            (car (my-eval (car args) alist))
          )
          ((eq fn 'cdr)
            (cdr (my-eval (car args) alist))
          )
          ((eq fn 'cons)
            (cons (my-eval (car args) alist) (my-eval (cadr args) alist))
          )
          ((eq fn 'quote)
            (car args)
          )
          ((eq fn 'null)
            (eq nil (my-eval (car args) alist))
          )
          ((eq fn 'atom)
            (atom (my-eval (car args) alist))
          )
          ((eq fn 'print)
            (print (my-eval (car args) alist))
          )
          ((eq fn 'listp)
            (listp (my-eval (car args) alist))
          )
          ((or (eq fn '+) (eq fn 'add))
            (+ (my-eval (car args) alist) (my-eval (cadr args) alist))
          )
          ((eq fn '-)
            (- (my-eval (car args) alist) (my-eval (cadr args) alist))
          )
          ((eq fn 'mod)
            (mod (my-eval (car args) alist))
          )
          ((eq fn 'floor)
            (floor (my-eval (car args) alist))
          )
          ((eq fn 'setq) (my-eval-setq ;; you'll have to figure out the arguments to pass
                                (car args) (my-eval (cadr args) alist)))
          ((eq fn 'cond) (my-eval-cond args alist))
          ((eq fn 'defun) (my-eval-defun args alist))
          ((eq fn 'eval) (my-eval (my-eval (car args) alist) alist))
          (T (my-apply (cdr (my-assoc fn alist)) ;; get the lambda from the alist
                       args alist))
    )
)


;; setq and defun will push a new association on the global-alist.
;; whenever we apply a function, we will bind the formals to the evaluated
;; actuals pushing these new bindings onto the local alist and then
;; evaluate the body of the function in that new scoping context.

;; You need to write this one.

(defun my-eval-setq (var val)
  ; (print var)
  ; (print val)
  (setq global-alist (cons (cons var val) global-alist))
;; just push a new association of the var and its evaluated val onto the
;; global alist
)

;; You need to write this one.  You should know how cond works at this point.
;; Remember, cond clauses have one or more expressions in each clause.

(defun my-eval-cond (clauses alist)
  (cond
    ((null clauses)
      nil
    )
    ((my-eval (caar clauses) alist)
      (my-eval-list (cdar clauses) alist)
    )
    (T
      (my-eval-cond (cdr clauses) alist)
    )
  )
)

;; You need to write this one.
;; Hint: just push the function body onto the global alist.  It is already an
;; association, e.g., (equal (L1 L2) (cond (...))) and (assoc 'equal in
;; the global alist will return this.  You can then take the cdr and you
;; have a list containing the formal parameters and the expressions in
;; the function body. defun returns the name of the function.

(defun my-eval-defun (body alist)
  (setq global-alist (cons body global-alist))
  ;; (print (caar global-alist)) -> function name
  ;; (print (cdar global-alist)) -> ((function params), functiondef)
  (car body)
)

;; This one is done, it just initializes the global alist where global
;; settings, like those defined via setq and defun, go.

(defvar global-alist nil)

;; to push a new value, (setq global-alist (cons (cons 'newvar 'newval) global-alist))

;; This one is done, it will become the new top-level for LISP.  After you
;; load this file, call (my-top) and then you can type in expressions and
;; define and call functions to test your my-eval. Note it uses the prog which
;; allows defining local variables, labels and goto looping similar to features
;; found in imperative languages.

(defun my-top ()
    (prog ()
        top
            ;; read an s-expression, evaluate it using my-eval passing in the global-alist,
            ;; then print the result, functions and global variables will be on global-alist

            (print (my-eval (read) global-alist))
            (terpri) ;; prints a newline
            (go top) ;; loops forever
    )
)


(defun testallhw5 ()
    (my-test t)
    (my-test nil)
    (my-test "hello")
    (my-test 10)
    (my-test '(eq t t))
    (my-test '(eq nil nil))
    (my-test '(eq t nil))
    (my-test '(null nil))
    (my-test '(null t))
    (my-test '(quote (a b c)))
    (my-test '(eq 'a 'a))
    (my-test '(eq '(a b) '(a b)))
    (my-test '(car '(a b c)))
    (my-test '(cdr '(a b c)))
    (my-test '(cons 'foo '(a b c)))
    (my-test '(setq a '(a b c)))
    (my-test '(print '(a b c)))
    (my-test 'a)
    (my-test '(cond (nil 1)(t 2) (t 3)))
    (my-test '(cond ((eq t nil) (print "in case 1") 1)((eq t t) (print "in case 2") 2)(t (print "in case 3") 3)))
    (my-test '(defun rev (L R) (cond ((null L) R) (t (rev (cdr L) (cons (car L) R))))))
    (my-test '(rev a nil))
    (my-test '(rev (rev  a nil) nil))
    (my-test '(defun app (L R)(cond ((null L) R)(t (cons (car L) (app (cdr L) R))))))
    (my-test '(app (app a a) (app a a)))
)

(defun my-error (msg)
    (princ "Error: ")
    (princ msg)
    (terpri)
    nil
)

;; (trace my-eval my-apply-lambda my-eval-cond my-apply my-eval-list my-bind-formals)

(defun my-test (exp)
    (print exp )
    (print (my-eval exp global-alist))
    (terpri)
    (terpri)
)