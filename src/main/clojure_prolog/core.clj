(ns clojure-prolog.core
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure-prolog.unify :refer :all]))
;; =============================================================================
;; projure

;; Projure is based on the common lisp implementation of prolog found in PAIP chapter 12.
;; The code is split into sections corresponding roughly to the calling sequence of the compiler:
;;
;; prolog-compile-symbols
;;   prolog-compile
;;     compile-predicate
;;       compile-clause
;;         compile-body
;;           compile-call
;;         compile-arg
;;           compile-unify
;;             compile-arg
;;
;; This is the part of glossary Norvig made for the Prolog compiler.
;; I didn't like the forward declarations in my code, so I put them all here.
(declare prolog-compile-symbols) ; Compile a list of Prolog symbols.
(declare prolog-compile)         ; Compile a symbol; make a separate function for each arity.
(declare compile-predicate)      ; Compile all the clauses for a given symbol/arity.
(declare compile-clause)         ; Transform away the head and compile the resulting body.
(declare compile-body)           ; Compile the body of a clause.
(declare compile-call)           ; Compile a call to a Prolog predicate.
(declare compile-arg)            ; Generate code for an argument to a goal in the body.
(declare compile-unify)          ; Return code that tests if var and term unify.
(declare compile-unify-var)      ; Compile the unification of a Prolog variable.
(declare prolog-compiler-macro)  ; Fetch the compiler macro for a Prolog predicate.                        ;

;; =============================================================================
;; prolog auxiliary functions

;; Clauses are represented as (head body) lists
(defn clause-head [clause] (first clause))
(defn clause-body [clause] (rest clause))

(def db-predicates (atom {}))

(def uncompiled (atom #{}))

;; Clauses are indexed by the predicate
(defn get-clauses [pred] (get @db-predicates pred []))
(defn predicate [relation] (first relation))

(defn args
  "The arguments of a relation."
  [x]
  (rest x))

(defn relation-arity
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  [relation]
  (count (args relation)))

(defn clause-arity [clause]
  (relation-arity (clause-head clause)))

(defn arities
  "Return a set of arities for the given symbol."
  [sym]
  (reduce #(conj %1 (clause-arity %2)) #{} (get-clauses sym)))

(defn clauses-with-arity
  "Return all clauses whose head has a given arity"
  [clauses test arity]
  (into []
        (filter (fn [c] (test arity (relation-arity (first c))))
                clauses)))

(defn add-clause
  "Add a clause to the database, indexed by the head's predicate."
  [clause]
  ;;The predicate must be a non-variable symbol.
  (let [pred (predicate (clause-head clause))
        clauses (conj (get-clauses pred) clause)]
    (swap! db-predicates assoc pred clauses)
    (swap! uncompiled conj pred)
    pred))

(defn retract-same-arity-clause
  "Retract a clause from the database."
  [clause]
  (let [head (clause-head clause)
        arity (relation-arity head)
        pred (predicate head)]
    (swap! db-predicates assoc pred
           (clauses-with-arity (get-clauses pred) not= arity))))

(defn clear-predicate
  "Remove the clauses for a single predicate."
  [predicate]
  (swap! db-predicates dissoc predicate))

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= (first (name x)) \?)))

(defn var-counts [expr]
  "How many times does each variable occur in expr?"
  (frequencies (filter variable? (flatten (list expr)))))

(defn vars-in
  "Return a list of all the variables in the expression expr."
  [expr]
  (keys (var-counts expr)))

;; =============================================================================
;; prolog-compile

(defn prolog-compile
  "Compile a symbol; make a separate function for each arity"
  ([symbol]
   (prolog-compile symbol (get-clauses symbol)))
  ([symbol clauses]
   (when (seq clauses)
     (let [arity (relation-arity (clause-head (first clauses)))]
       ;; Compile the clauses with this arity
       (compile-predicate
         symbol arity (clauses-with-arity clauses = arity))
       ;; Compile all the clauses with any other arity
       (prolog-compile
         symbol (clauses-with-arity clauses not= arity))))))

;; =============================================================================
;; compile-predicate

;; define symbols for trail and continuation
(def trail 'trail)
(def cont 'cont)

(defn make-parms
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  [arity]
  (map #(symbol (str "?arg" (+ % 1))) (range arity)))

(defn make-predicate
  "Return the symbol: symbol-arity"
  [sym arity]
  (symbol (str (name sym) "-" arity)))

(defn single? [x]
  "Is x a list with one element?"
  (and (list? x) (empty? (rest x))))

(defn maybe-add-undo-bindings
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  [compiled-exps]
  (if (single? compiled-exps)
    compiled-exps
    `(~'let [~'old-trail (~'size ~trail)]
       ~@(butlast
           (apply concat
                  (map (fn [exp]
                         `(~exp (~'undo-bindings! ~trail ~'old-trail)))
                       compiled-exps))))))

(defn gen-predicate
  "Generate a single Clojure function
   for the clauses of a given symbol/arity"
  [sym arity clauses]
  (let [pred (make-predicate sym arity)
        parms (make-parms arity)]
    `(~'defn ~pred [~@parms ~trail ~cont]
       ~(maybe-add-undo-bindings
          (map #(compile-clause parms % cont) clauses)))))

(defn compile-predicate
  "Compile the clauses for a given symbol/arity
   into a single Clojure function."
  [sym arity clauses]
  (eval (gen-predicate sym arity clauses)))

;; =============================================================================
;; compile-clause

(defn make-= [x y]
  (list '= x y))

(defn self-binds [vars]
  (into {}
        (map (fn [v] [v v]) vars)))

(defn bind-unbound-vars
  "If there are any variables in expr (besides the parms)
  then bind them to new vars."
  [parms expr]
  (let [vars (remove #(some #{%} parms) (remove #{'?} (vars-in expr)))]
    (if (empty? vars)
      expr
      `(~'let [~@(apply concat (map #(list % '(?)) vars))]
         ~expr))))

(defn compile-clause
  "Transform away the head, and compile the resulting body."
  [parms clause cont]
  (bind-unbound-vars
    parms
    (compile-body
      (concat
        (map make-= parms (args (clause-head clause)))
        (clause-body clause))
      cont
      (self-binds parms))))

;; =============================================================================
;; compile-body

(defn bind-new-vars
  "Extend bindings to include any unbound vars in goal."
  [binds goal]
  (merge (self-binds (vars-in goal)) binds))

(defn compile-body
  "Compile the body of a clause."
  [body cont binds]
  (cond
    (empty? body) (list cont)
    :else (let [goal (first body)
                macro (prolog-compiler-macro (predicate goal))
                macro-val (if macro
                            (macro goal (rest body)
                                   cont binds))]
            (if (and macro (not= macro-val :pass))
              macro-val
              (compile-call
                (make-predicate (predicate goal)
                                (relation-arity goal))
                (map (fn [arg]
                       (compile-arg arg binds))
                     (args goal))
                (if (empty? (rest body))
                  cont
                  `(~'fn []
                     ~(compile-body
                        (rest body) cont
                        (bind-new-vars binds goal)))))))))

(defn compile-call
  "Compile a call to a Prolog predicate."
  [pred args cont]
  `(~pred ~@args ~trail ~cont))

;; =============================================================================
;; compile-arg

(defn improper-list?
  "Is x a improper (dotted) list?"
  [x]
  (and (list? x) (some #(= '. %) x)))
(defn no-vars?
  "Is there no variable anywhere in the expression x?"
  [x]
  (empty? (vars-in x)))

(defn compile-arg
  "Generate code for an argument to a goal in the body."
  [arg binds]
  (cond
    (nil? arg) nil
    (= arg '?) '(?)
    (variable? arg)
    (let [bind (get binds arg)]
      (if (and (not (nil? bind)) (not= bind arg))
        (compile-arg bind binds)
        arg))
    (improper-list? arg)
    (if (= (first arg) '.)
      (compile-arg (second arg) binds)
      `(~'pair ~(compile-arg (first arg) binds)
         ~(compile-arg (next arg) binds)))
    (no-vars? arg) `'~arg
    :else `(list ~@(map #(compile-arg % binds) arg))))

;; =============================================================================
;; compile-unify

(defn compile-if
  "Compile a Clojure IF form."
  [pred then-part]
  (case pred
    true then-part
    false nil
    (list 'if pred then-part)))

(defn find-anywhere
  "Does item occur anywhere in tree? If so, return it."
  [item tree]
  (some #(when (= item %) item) (flatten tree)))

(defn extend-bindings
  "Add a {var val} entry to binding map."
  [var val binds]
  (assoc binds var val))

(defn bind-vars-in
  "Bind all the variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  [exp binds]
  (loop [binds binds vars (vars-in exp)]
    (if (empty? vars)
      binds
      (let [[var & rest] vars]
        (recur (assoc binds var (get binds var var)) rest)))))

(defn follow-binding
  "Get the ultimate binding of var according to binds"
  [var binds]
  (when-let [b (find binds var)]
    (if (= (key b) (val b))
      b
      (or (follow-binding (val b) binds) b))))

;; We want to generate the following unification code for member/2
;;
;; (<- (member ?item (?item . ?rest))
;;
;; (let [?item (?)]
;;   (if (unify! ?arg1 ?item trail)
;;     (if (unify! ?arg1 (pair ?item (?)) trail)
;;       (cont))))
;;
;;      Unification        Code                     Bindings
;;   1  (= 3 3)            true                     --
;;   2  (= 3 4)            false                    --
;;   3  (= (f ?x) (?p 3))  true                     {?x 3, ?p f}
;;   4  (= ?arg1 ?y)       true                     {?y ?arg1}
;;   5  (= ?arg1 ?arg2)    (unify! ?arg1 ?arg2 tr)  {?arg1 ?arg2}
;;   6  (= ?arg1 3)        (unify! ?arg1 3 tr)      {?arg1 3}
;;   7  (= ?arg1 (f ?y))   (unify! ?arg1 ...)       {?y ?y}
;;   8  (= ?x ?y)          true                     {?x ?y}
;;   9  (= ?x 3)           true                     {?x 3}
;;  10  (= ?x (f ?y))      (unify! ?x ...)          {?y ?y}
;;  11  (= ?x (f ?x))      false                    --
;;  12  (= ?x ?)           true                     --
(defn compile-unify
  "Return a vector with code to test
   if x and y unify and a new binding map"
  [x y binds]
  (cond
    ;; Unify constants and sequences:
    (and (no-vars? x) (no-vars? y))                         ; Case
    [(= x y) binds]                                         ; 1,2
    (and (seq? x) (seq? y))                                 ; 3
    (let [[code1 binds1]
          (compile-unify (first x) (first y) binds)
          [code2 binds2]
          (compile-unify (rest x) (rest y) binds1)]
      [(compile-if code1 code2) binds2])
    ;; Here x or y is a var. Pick the right one:
    (variable? x) (compile-unify-var x y binds)
    (variable? y) (compile-unify-var y x binds)
    :else (println "compile-unify failed." x y binds)))

(defn compile-unify-var
  "X is a variable, and Y may be."
  [x y binds]
  (let [xb (follow-binding x binds)
        x1 (if xb (val xb) x)
        yb (if (variable? y) (follow-binding y binds))
        y1 (if yb (val yb) y)]
    (cond                                                   ; Case
      (or (= x '?) (= y '?)) [true binds]                   ; 12
      (or (not= x x1) (not= y y1)) (compile-unify x1 y1 binds) ; deref
      (find-anywhere x1 y1) [false binds]                   ; 11
      (and (list? y1) (seq y1))                             ; 7,10
      [`(~'unify! ~x1 ~(compile-arg y1 binds) ~trail)
       (bind-vars-in y1 binds)]
      (not (nil? xb))
      ;; i.e. x is an ?arg variable
      (if (and (variable? y1) (nil? yb))
        [true (extend-bindings y1 x1 binds)]                ; 4
        [`(~'unify! ~x1 ~(compile-arg y1 binds) ~trail)
         (extend-bindings x1 y1 binds)])                    ; 5,6
      (not (nil? yb))
      (compile-unify-var y1 x1 binds)
      :else [true (extend-bindings x1 y1 binds)])))         ; 8,9

;; =============================================================================
;; prolog-compiler-macro

;; macro table
(def prolog-compiler-macros (atom {}))

(defn prolog-compiler-macro [name]
  "Lookup Prolog compiler macro by name."
  (get @prolog-compiler-macros name))

(defmacro def-prolog-compiler-macro
  "Define and install a compiler macro for Prolog."
  [name doc arglist & body]
  `(swap! ~'prolog-compiler-macros assoc
          '~name (fn ~arglist ~@body)))

(def-prolog-compiler-macro =
  "Generate code for an equality predicate that
  succeeds when the arguments can be unified.
  (defn =-2 [?arg1 ?arg2 trail cont]
    (if (unify! ?arg1 ?arg2 trail)
      (cont)))"
  [goal body cont binds]
  (let [[x y :as args] (args goal)]
    (if (not= (count args) 2)
      :pass ;; decline to handle this goal
      (let [[code1 binds1] (compile-unify x y binds)]
        (compile-if code1 (compile-body body cont binds1))))))

(defn lisp-expr [expr binds]
  "Make a function with body `expr` and apply to the dereferenced vars."
  (let [args (vars-in expr)]
    `((fn [~@args] ~expr)
      ~@(map (fn [arg] `(deref-copy ~(compile-arg arg binds)))args))))

(defn deref-copy
  "Dereference all variables in the expression and convert pairs
   to lists. In other words, convert our internal representation
   to suitable form for use in the `lisp` functor."
  [expr]
  (let [x (deref-var expr)]
    (cond
      (pvar? x) nil ;unbound
      (coll? x) (map deref-copy x)
      (pair? x) (cons (deref-copy (car x)) (deref-copy (cdr x)))
      :else x)))

(def-prolog-compiler-macro lispp
  "Convenience predicate that fails if the form returns false.
   For example, (lispp (<= ?x ?y) gets compiled to something like
   (if ((fn [?x ?y] (<= ?x ?y) (deref-copy ?x) (deref-copy ?arg2)) (cont))"
   [goal body cont binds]
   (let [args (args goal)]
     (if (not= (count args) 1)
       :pass ;; decline to handle this goal
       `(if ~(lisp-expr (first args) binds)
          ~(compile-body body cont binds)))))

(def-prolog-compiler-macro lisp
  "Compiler macro for lisp/1 and lisp/2."
  [goal body cont binds]
  (let [args (args goal)]
    (case (count args)
      ;; lisp/1
      1 `(do ~(lisp-expr (first args) binds)
             ~(compile-body body cont binds))
      ;; lisp/2
      2 (let [[var expr] args]
          (compile-if
            `(unify! ~(compile-arg var binds) ~(lisp-expr expr args) ~trail)
            (compile-body body cont binds)))
      ;; decline to handle this goal
      :pass)))

;; =============================================================================
;; user interface

(declare make-anonymous)

;; facts and rules

(defmacro <-
  "Add a clause to the database."
  [& clause]
  `(add-clause '~(make-anonymous clause)))

(defmacro <--
  "Add a clause to the database, but first retract all
   facts and rules for the functor with the same arity."
  [& clause]
  `(let [clause# '~(make-anonymous clause)]
     (do
       (retract-same-arity-clause clause#)
       (add-clause clause#))))

(defmacro fact
  "Add a clause to the database. Same as <-"
  [& clause]
  `(add-clause '~(make-anonymous clause)))

(defmacro rule [& clause]
  `(add-clause '~(make-anonymous clause)))

(declare compile-anonymous-clause ignore)

;; interface between lisp and prolog

(defmacro prolog
  "Invoke Prolog from Clojure to solve a conjunction of clauses.
   The Clojure environment can be accessed through the `lisp` functor.
   Example:
   (defn friends-of [person]
     (let [friends (transient[])]
       (prolog (lisp ?person person)
               (likes ?person ?x)
               (lisp (conj! friends ?x)))
       (persistent! friends)))"
  [& goals]
  (prolog-compile-symbols @uncompiled)
  `(~(compile-anonymous-clause (make-anonymous goals)) (~new-trail) ignore))

(defmacro query
  "Invoke Prolog in the surrounding Clojure
   environment and return a vector of selected variables.
   Example:
   (defn friends-of [person]
     (query ?x (lisp ?person person)
               (likes ?person ?x)))"
  [var & goals]
  (let [vec-sym (gensym)]
    `(let [~vec-sym (transient [])]
       (prolog ~@goals
               (~'lisp (conj! ~vec-sym ~var)))
       (persistent! ~vec-sym))))

(declare replace-?-vars top-level-query-0)

;; interactive environment

(defmacro ?-
  "Make a query and print the answers."
  [& goals]
  `(~'top-level-prove '~(replace-?-vars goals)))

(defn top-level-prove
  "Prove the list of goals by compiling and calling it."
  [goals]
  ;; First redefine the top-level-query
  (clear-predicate 'top-level-query)
  (let [vars (remove #{'?} (vars-in goals))]
    (add-clause `((~'top-level-query)
                  ~@goals
                  (~'show-prolog-vars ~(map name vars)
                    ~vars))))
  ;; Compile anything else that needs it
  (prolog-compile-symbols @uncompiled)
  ;; Finally, call the query
  (top-level-query-0 (new-trail) ignore))

(defn replace-?-vars
  "Replace any ? within the expr with a var of the form ?123."
  [expr]
  (walk/postwalk #(if (= % '?) (gensym "?") %) expr))

(defn continue?
  "Ask user if we should continue looking for solutions."
  []
  (case (read-line)
    ";" true
    "n" false
    (do
      (println " Type ';' to see more or 'n' to stop")
      (continue?))))

(defn show-prolog-vars-2
  "Display the variables, and prompt the user to see
  if we should continue. If not, return to top level."
  [var-names vars trail cont]
  (if (empty? vars)
    (println "No.")
    (dotimes [i (count vars)]
      (println (nth var-names i) "=" (deref-var (nth vars i)))))
  (if (continue?)
    (cont)))

;; compile prolog symbols

(defn anonymous-variables-in [tree]
  "Return a list of all variables that occur only once in tree."
  (map first
       (filter #(= (second %) 1) (var-counts tree))))

(defn make-anonymous
  "Replace variables that are only use once with ?."
  ([expr] (make-anonymous expr (anonymous-variables-in expr)))
  ([expr anon-vars]
   (walk/postwalk #(if (some #{%} anon-vars) '? %) expr)))

(defn compile-anonymous-clause [goals]
  `(fn [~trail ~cont]
     ~(compile-clause () `(() ~@goals) cont)))

(defn prolog-compile-symbols
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  [symbols]
  (do
    ;; Declare the symbols about to be compiled
    (eval
      `(declare
         ~@(mapcat (fn [sym]
                     (map (fn [a] (make-predicate sym a))
                          (arities sym)))
                   symbols)))
    ;; Now compile them
    (doseq [sym (into [] symbols)]
      (prolog-compile sym)
      (swap! uncompiled set/difference symbols))))

(defn ignore [& rest] nil)

;; =============================================================================
;; prolog primitives

(<-- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(comment
  (defn prolog-=-2
    [?arg1 ?arg2 trail cont]
    (if (unify! ?arg1 ?arg2 trail)
      (cont)))

  (defn deref-equal
    "Are the two arguments = with no unification but with dereferencing"
    [x y]
    )

  (defn prolog-==-2
    [?arg1 ?arg2 trail cont]
    (if (deref-equal ?arg1 ?arg2)
      (cont)))

  (defn prolog-append-3
    "(<- (append nil ?ys ?ys))
   (<- (append (?x . ?xs) ?ys (?x . ?zs))(append ?xs ?ys ?zs))"
    [?arg1 ?arg2 ?arg3 trail cont]
    )

  (defn prolog-bagof-3
    [expr goal result trail cont]
    )

  (defn prolog-call-1
    "Try to prove a goal by calling it."
    [?arg1 trail cont]

    )

  (defn prolog-and-2)

  (<-- (member ?item (?item . ?rest)))
  (<- (member ?item (?x . ?rest)) (member ?item ?rest))

  (defn prolog-setof-3
    [expr goal result trail cont]
    )
  )



