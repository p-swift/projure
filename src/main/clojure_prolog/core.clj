(ns clojure-prolog.core
  (:require [clojure.set :as set]
            [clojure.walk :as walk])
  (:import (java.util ArrayDeque Deque))
  (:refer-clojure :exclude [bound? cons deref]))

;; =============================================================================
;; destructive unification

(defprotocol IVar
  (bound? [_])
  (set-bound! [_ val])
  (get-binding [_])
  (set-binding! [_ val]))

(deftype LVar [^{:unsynchronized-mutable true} bound
               ^{:unsynchronized-mutable true} binding]
  IVar
  (bound? [_] bound)
  (set-bound! [_ x] (set! bound x))
  (get-binding [_] binding)
  (set-binding! [_ x] (set! binding x))
  Object
  (toString [this]
    (str "bound: " bound
         " binding: " binding)))

(defn ?
  "Construct an anonymous logic variable"
  []
  (LVar. false nil))

(defn lvar?
  "Is x a logic variable?"
  [x]
  (instance? LVar x))

(defn deref
  "Follow the pointers for bound variables."
  [exp]
  (loop [var exp]
    (if (and (lvar? var) (bound? var))
      (recur (get-binding var))
      var)))

(defn new-trail []
  (new ArrayDeque))

(defn old-trail
  "Return the size of trail"
  [^Deque trail]
  (.size trail))

(defn bind-var!
  "Set the var's binding to value after saving
   the variable in the trail. Always return true."
  [var val ^Deque trail]
  (when (not= var val)
    (.push trail var)
    (set-bound! var true)
    (set-binding! var val)
    true))

(defn undo-bindings! [old-trail ^Deque trail]
  "Undo all bindings back to a given point in the trail"
  (while (> (.size trail) old-trail)
    (set-bound! (.pop trail) false)))

(deftype Cons [car cdr]
  Object
  (toString [_]
    (str "(" car " . " cdr ")"))
  (equals [this that]
    (identical? this that)))

(defn cons [x y]
  (Cons. x y))

(defn car [^Cons x]
  (.car x))

(defn cdr [^Cons x]
  (.cdr x))

(defn cons? [x]
  (instance? Cons x))

(defn unify!
  "Destructively unify two expressions"
  [u v trail]
  (let [x (deref u)
        y (deref v)]
    (cond
      (lvar? x)
      (bind-var! x y trail)
      (lvar? y)
      (bind-var! y x trail)
      (cons? x)
      (if (cons? y)
        (and (unify! (car x) (car y) trail)
             (unify! (cdr x) (cdr y) trail))
        (if (coll? y)
          (and (unify! (car x) (first y) trail)
               (unify! (cdr x) (next y) trail))))
      (coll? x)
      (if (coll? y)
        (and (unify! (first x) (first y) trail)
             (unify! (next x) (next y) trail))
        (if (cons? y)
          (and (unify! (first x) (car y) trail)
               (unify! (next x) (cdr y) trail))))
      :else (= x y))))

;; =============================================================================
;; prolog auxiliary functions

;;  Clauses are represented as (head body) lists
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

(def trailsym 'trail)

(def contsym 'cont)

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
;; prolog compile

(declare compile-predicate)

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
;; compile predicate

(declare compile-clause)

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
    `(~'let [~'old-trail (~'old-trail ~trailsym)]
       ~@(butlast
           (apply concat
                  (map (fn [exp]
                         `(~exp (~'undo-bindings! ~'old-trail ~trailsym)))
                       compiled-exps))))))

(defn gen-predicate
  "Generate a single Clojure function
   for the clauses of a given symbol/arity"
  [sym arity clauses]
  (let [pred (make-predicate sym arity)
        parms (make-parms arity)]
    `(~'defn ~pred [~@parms ~trailsym ~contsym]
       ~(maybe-add-undo-bindings
          (map #(compile-clause parms % contsym) clauses)))))

(defn compile-predicate
  "Compile the clauses for a given symbol/arity
   into a single Clojure function."
  [sym arity clauses]
  (eval (gen-predicate sym arity clauses)))

;; =============================================================================
;; compile clause

(declare compile-body)

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

(declare compile-arg prolog-compiler-macro)
;; =============================================================================
;; compile body

(defn bind-new-vars
  "Extend bindings to include any unbound vars in goal."
  [binds goal]
  (merge (self-binds (vars-in goal)) binds))

(defn compile-call
  "Compile a call to a Prolog predicate."
  [pred args cont]
  `(~pred ~@args ~trailsym ~cont))

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

(declare compile-arg compile-if compile-unify)
;; =============================================================================
;; prolog compiler macros

(def prolog-compiler-macros (atom {}))

(defn prolog-compiler-macro [name]
  (get @prolog-compiler-macros name))

(defmacro def-prolog-compiler-macro
  "Define a compiler macro for Prolog."
  [name doc arglist & body]
  `(swap! ~'prolog-compiler-macros assoc
          '~name (fn ~arglist ~@body)))

(def-prolog-compiler-macro =
  "the doc for  ="
  [goal body cont binds]
  (let [[x y :as args] (args goal)]
    (if (not= (count args) 2)
      :pass ;; decline to handle this goal
      (let [[code1 binds1] (compile-unify x y binds)]
        (compile-if code1 (compile-body body cont binds1))))))

(def-prolog-compiler-macro lispp
  "the doc for  is
   (lisp (<= ?x ?y) gets compiled to
   (if (apply (fn [?x ?y] (<= ?x ?y)) (map deref (list ?x ?arg2)))"
   [goal body cont binds]
   (let [args (args goal)]
     (if (not= (count args) 1)
       :pass ;; decline to handle this goal
       (let [lisp-expr (first args)
             lisp-args (vars-in lisp-expr)]
         `(if (~'apply (~'fn [~@lisp-args] ~lisp-expr)
                 (map deref ~(compile-arg lisp-args binds)))
                      ~(compile-body body cont binds))))))

(def-prolog-compiler-macro lisp
  "the doc for lisp"
  [goal body cont binds]
  (let [args (args goal)]
    (case (count args)
      ;; lisp/1
      1 (let [lisp-expr (first args)
              lisp-args (vars-in lisp-expr)]
          `(do
             (~'apply (~'fn [~@lisp-args] ~lisp-expr)
               (map deref ~(compile-arg lisp-args binds)))
             ~(compile-body body cont binds)))
      ;; lisp/2
      2 (let [[var lisp-expr] args
              lisp-args (vars-in lisp-expr)]
          (compile-if
            `(~'unify! ~(compile-arg var binds)
               (~'apply (~'fn [~@lisp-args] ~lisp-expr)
                 ~(compile-arg lisp-args binds))
               ~trailsym)
            (compile-body body cont binds)))
      ;; decline to handle this goal
      :pass)))

(declare compile-unify-var compile-arg)
;; =============================================================================
;; compile unify

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

(defn no-vars?
  "Is there no variable anywhere in the expression x?"
  [x]
  (empty? (vars-in x)))

(defn improper-list?
  "Is x a improper (dotted) list?"
  [x]
  (and (list? x) (some #(= '. %) x)))

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

; (<- (member ?item (?item . ?rest))
;
; (let [?item (?)]
;   (if (unify! ?arg1 ?item trail)
;     (if (unify! ?arg1 (con ?item (?)) trail)
;       (cont))))
;
;      Unification        Code                     Bindings
;   1  (= 3 3)            true                     --
;   2  (= 3 4)            false                    --
;   3  (= (f ?x) (?p 3))  true                     {?x 3, ?p f}
;   4  (= ?arg1 ?y)       true                     {?y ?arg1}
;   5  (= ?arg1 ?arg2)    (unify! ?arg1 ?arg2 tr)  {?arg1 ?arg2}
;   6  (= ?arg1 3)        (unify! ?arg1 3 tr)      {?arg1 3}
;   7  (= ?arg1 (f ?y))   (unify! ?arg1 ...)       {?y ?y}
;   8  (= ?x ?y)          true                     {?x ?y}
;   9  (= ?x 3)           true                     {?x 3}
;  10  (= ?x (f ?y))      (unify! ?x ...)          {?y ?y}
;  11  (= ?x (f ?x))      false                    --
;  12  (= ?x ?)           true                     --
(defn compile-unify
  "Return a vector with code to test
   if x and y unify and a new binding map"
  [x y binds]
  (cond
    ;; Unify constants and conses:
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
      [`(~'unify! ~x1 ~(compile-arg y1 binds) ~trailsym)
       (bind-vars-in y1 binds)]
      (not (nil? xb))
      ;; i.e. x is an ?arg variable
      (if (and (variable? y1) (nil? yb))
        [true (extend-bindings y1 x1 binds)]                ; 4
        [`(~'unify! ~x1 ~(compile-arg y1 binds) ~trailsym)
         (extend-bindings x1 y1 binds)])                    ; 5,6
      (not (nil? yb))
      (compile-unify-var y1 x1 binds)
      :else [true (extend-bindings x1 y1 binds)])))         ; 8,9

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
      `(~'cons ~(compile-arg (first arg) binds)
         ~(compile-arg (next arg) binds)))
    (no-vars? arg) `'~arg
    :else `(list ~@(map #(compile-arg % binds) arg))))

;; =============================================================================
;; user interface utils

(defn deref-expr [expr]
  (cond
    (seq? expr) (map deref-expr expr)
    (lvar? expr) (deref-expr (deref expr))
    :else (deref expr)))

(defn replace-?-vars
  "Replace any ? within the expr with a var of the form ?123."
  [expr]
  (walk/postwalk #(if (= % '?) (gensym "?") %) expr))

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
  `(fn [~trailsym ~contsym]
     ~(compile-clause () `(() ~@goals) contsym)))

(defn ignore [& rest] nil)

(defn prolog-compile-symbols
  "Compile new Prolog functions ."
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
      (println (prolog-compile sym))
      (swap! uncompiled set/difference symbols))))

(defn prolog-source
  "Print the code generated from the clauses for a symbol/arity."
  [symbol arity]
  (gen-predicate symbol arity (clauses-with-arity (get-clauses symbol) = arity)))

;; =============================================================================
;; user interface

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
  "Add a clause to the database."
  [& clause]
  `(add-clause '~(make-anonymous clause)))

(defmacro rule [& clause]
  `(add-clause '~(make-anonymous clause)))

(defmacro ?-
  "Make a query and print the answers."
  [& goals]
  `(~'top-level-prove '~(replace-?-vars goals)))

(declare continue? top-level-query-0)

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

(defn show-prolog-vars-2
  "Display the variables, and prompt the user to see
  if we should continue. If not, return to top level."
  [var-names vars trail cont]
  (if (empty? vars)
    (println "No.")
    (dotimes [i (count vars)]
      (println (nth var-names i) "=" (deref (nth vars i)))))
  (if (continue?)
    (cont)))

(defn continue?
  "Ask user if we should continue looking for solutions."
  []
  (case (read-line)
    ";" true
    "n" false
    (do
      (println " Type ';' to see more or 'n' to stop")
      (continue?))))

(defmacro prolog
  "Invoke Prolog from Clojure to solve a conjunction of clauses.
   The Clojure environment can be accessed through the `lisp` functor.

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

   (defn friends-of [person]
     (query ?x (lisp ?person person)
               (likes ?person ?x)))"
  [var & goals]
  (let [vec-sym (gensym)]
    `(let [~vec-sym (transient [])]
       (prolog ~@goals
               (~'lisp (conj! ~vec-sym ~var)))
       (persistent! ~vec-sym))))

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



