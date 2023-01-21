(ns clojure-prolog.unify
  (:import (java.util ArrayDeque Deque)))
;; =============================================================================
;; destructive unification

;; This package implements an efficient form of unification.
;; Prolog variables have a mutable pointer to their binding which can be
;; bound and unbound as needed. Instead of maintaining a binding list, we use
;; a stack to keep track of which variables need to be unbound when backtracking.
;; It goes without saying the use of mutable state is not idiomatic Clojure.
;; Anyway we define following types:
;; - PVar is a Prolog variable which points directly to its binding.
;; - Trail keeps track of bound variables, so we can unbind them later.
;; - Pair is a 'cons' in other lisps (more below).

(defprotocol IVar
  "Represent a Prolog variable."
  (bound-var? [_])
  (set-bound! [_ val])
  (get-binding [_])
  (set-binding! [_ val]))

(deftype PVar [^{:unsynchronized-mutable true} bound
               ^{:unsynchronized-mutable true} binding]
  IVar
  (bound-var? [_] bound)
  (set-bound! [_ x] (set! bound x))
  (get-binding [_] binding)
  (set-binding! [_ x] (set! binding x))
  Object
  (toString [_]
    (str "bound: " bound
         " binding: " binding))
  (equals [this that]
    (identical? this that)))

(defn ?
  "Construct an anonymous Prolog variable."
  []
  (PVar. false nil))

(defn pvar?
  "Is x a Prolog variable?"
  [x]
  (instance? PVar x))

(defn deref-var
  "Follow the pointers for bound variables."
  [exp]
  (loop [var exp]
    (if (and (pvar? var) (bound-var? var))
      (recur (get-binding var))
      var)))

(defprotocol Trail
  (size [_])
  (bind-var! [_ var val])
  (undo-bindings! [_ size]))

(extend-type Deque
  Trail
  (size [this]
    "Return the size of trail"
    (.size this))
  (bind-var! [this var val]
    "Set the var's binding to value after saving\n the variable in the trail. Always return true."
    (when (not= var val)
      (.push this var)
      (set-bound! var true)
      (set-binding! var val)
      true))
  (undo-bindings! [this size]
    " Undo all bindings back to a given point in the trail"
    (while (> (.size this) size)
      (set-bound! (.pop this) false))))

(defn new-trail []
  (new ArrayDeque))

;; Pair is like a list where the last element might not be nil.
;; In other lisps, it is called 'cons' and it is used to build
;; lists which can proper (if they end with nil) or improper/dotted
;; if they don't. The reason we need Pair is that Prolog predicates
;; like member/2 and append/3 often recurse down a list by destructing
;; the parameter list, and then unifying the first and the rest of it.
;; Think about the first clause of member/2 which says that an item
;; is a member of list if it is the first element in a list:
;;
;; (<-- (member ?item (?item . ?rest))
;;
;; (defn member-2 [?arg1 ?arg2 trail cont]
;;   (let [old-trail (size trail)]
;;     (if (unify! ?arg2 (pair ?arg1 (?)) trail)
;;       (cont))
;;           ... ))
;;
;; If we call (?- (member ?x [1 2 3])), then ?arg2 is bound to [1 2 3];
;; then if unification succeeds, arg1? will be bound to 1 and the anonymous
;; variable will be bound to [2 3]. If we had tried 'list' instead, then
;; unify!(?arg2 (list ?arg1 (?)) trail) would fail because ?arg1 bind to 1,
;; (?) to 2 but the whole unification would fail when we tried to unify 3 with nil.
;;
(deftype Pair [first rest]
  Object
  (toString [_]
    (str "(" first " . " rest))
  (equals [this that]
    (and (instance? Pair that)
         (= (first this) (first that))
         (= (rest this) (rest that)))))

(defn pair
  "Make a new Pair with first and rest."
  [first rest]
  (Pair. first rest))

(defn pair?
  "Is x a Pair?"
  [x]
  (instance? Pair x))

(defn paired? [x]
  "Is x a Pair or Collection?"
  (or (pair? x) (coll? x)))

(defn car [x]
  "Return the first element in a pair or collection."
  (if (pair? x) (.first x) (first x)))

(defn cdr [x]
  "Return the other element of a pair or the next item in a collection."
  (if (pair? x) (.rest x) (next x)))

;; finally we can define unify!

(defn unify!
  "Destructively unify two expressions."
  [x y trail]
  (let [x (deref-var x) y (deref-var y)]
    (cond
      (pvar? x) (bind-var! trail x y)
      (pvar? y) (bind-var! trail y x)
      (and (paired? x) (paired? y))
      (and (unify! (car x) (car y) trail)
           (unify! (cdr x) (cdr y) trail))
      :else (= x y))))
