(ns clojure-prolog.core-test
  (:require [clojure.test :refer :all]
            [clojure-prolog.core :refer :all]
            [clojure-prolog.unify :refer :all]))

(defn gen-prolog
  "Generate Prolog code from the clauses for a symbol/arity."
  [symbol arity]
  (gen-predicate symbol arity (clauses-with-arity (get-clauses symbol) = arity)))

; ******************** likes/2 ******************************
(<-- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))

(deftest likes-test
  ; generate the right code
  (is (= (gen-prolog 'likes 2)
         '(defn likes-2 [?arg1 ?arg2 trail cont]
            (let [old-trail (size trail)]
              (if (unify! ?arg1 'Robin trail)
                (if (unify! ?arg2 'cats trail)
                  (cont)))
              (undo-bindings! trail old-trail)
              (if (unify! ?arg1 'Sandy trail)
                (likes-2 ?arg2 'cats trail cont))
              (undo-bindings! trail old-trail)
              (if (unify! ?arg1 'Kim trail)
                (likes-2 ?arg2 'Lee trail
                         (fn [] (likes-2 ?arg2 'Kim trail cont))))))))
  ; who does sandy like?
  (is (= (query ?x (likes Sandy ?x)) '(Robin)))
  )

; ******************** member/2 *******************************

(<-- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(deftest member-test
  ; code for member/2
  (is (= (gen-prolog 'member 2)
         '(defn member-2 [?arg1 ?arg2 trail cont]
            (let [old-trail (size trail)]
              (if (unify! ?arg2 (pair ?arg1 (?)) trail)
                (cont))
              (undo-bindings! trail old-trail)
              (let [?rest (?)]
                (if (unify! ?arg2 (pair (?) ?rest) trail)
                  (member-2 ?arg1 ?rest trail cont)))))))

  (is (= (query ?x (member ?x [1 2 3])) [1 2 3]))
  )

; ******************** lispp/1 *************************
(<-- (positives ?x ?list) (member ?x ?list) (lispp (> ?x 0)))

(deftest lispp-test
  (is (= (query ?x (positives ?x [-2 -1 0 1 2 3])) [1 2 3]))
  )

; *********************** Quicksort ***********************

(<-- (append nil ?ys ?ys))
(<- (append (?x . ?xs) ?ys (?x . ?zs)) (append ?xs ?ys ?zs))

(deftest append-test
  (is (= (query ?xs (append ?xs [2 3] [1 2 2 3])) ['(1 2)]))
  (is (= (query ?ys (append [1 2] ?ys [1 2 2 3])) ['(2 3)]))
  (is (= (query ?zs (append [1 2] [2 3] ?zs)) ['(1 2 2 3)]))
  )

(<-- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
     (lispp (<= ?x ?y))
     (partition ?xs ?y ?ls ?bs))
(<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
    (lispp (> ?x ?y))
    (partition ?xs ?y ?ls ?bs))
(<- (partition nil ?y nil nil))
(<-- (quicksort (?x . ?xs) ?ys)
     (partition ?xs ?x ?littles ?bigs)
     (quicksort ?littles ?ls)
     (quicksort ?bigs ?bs)
     (append ?ls (?x . ?bs) ?ys))
(<- (quicksort nil nil))

(deftest quicksort-test
  (is (= (query ?x (quicksort [3 2 1] ?x)) [[1 2 3]]))
  )

; ************************ Zebra Puzzle ******************

(<-- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<-- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<-- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest)) (iright ?left ?right ?rest))

(<- (= ?x ?x))

(<-- (zebra ?h ?w ?z)
     ;; Each house is of the form:
     ;; (house nationality pet cigarette drink house-color)
     (= ?h ((house norwegian ? ? ? ?)                       ;1,10
            ?
            (house ? ? ? milk ?) ? ?))                      ; 9
     (member (house englishman ? ? ? red) ?h)               ; 2
     (member (house spaniard dog ? ? ?) ?h)                 ; 3
     (member (house ? ? ? coffee green) ?h)                 ; 4
     (member (house ukrainian ? ? tea ?) ?h)                ; 5
     (iright (house ? ? ? ? ivory)                          ; 6
             (house ? ? ? ? green) ?h)
     (member (house ? snails winston ? ?) ?h)               ; 7
     (member (house ? ? kools ? yellow) ?h)                 ; 8
     (nextto (house ? ? chesterfield ? ?)                   ;11
             (house ? fox ? ? ?) ?h)
     (nextto (house ? ? kools ? ?)                          ;12
             (house ? horse ? ? ?) ?h)
     (member (house ? ? luckystrike oj ?) ?h)               ;13
     (member (house japanese ? parliaments ? ?) ?h)         ;14
     (nextto (house norwegian ? ? ? ?)                      ;15
             (house ? ? ? ? blue) ?h)
     (member (house ?w ? ? water ?) ?h)                     ;Q1
     (member (house ?z zebra ? ? ?) ?h))                    ;Q2

(deftest zebra-tset
  (is (= (query ?z (zebra ?h ?w ?z)) ['japanese]))
  (is (= (query ?w (zebra ?h ?w ?x)) ['norwegian])))
