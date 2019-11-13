(ns clojure-prolog.core-test
  (:require [clojure.test :refer :all]
            [clojure-prolog.core :refer :all]))

(<-- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))

(deftest likes-predicate

  (is (= (prolog-source 'likes 2)
         '(defn likes-2 [?arg1 ?arg2 trail cont]
            (let [old-trail (old-trail trail)]
              (if (unify! ?arg1 'Robin trail)
                (if (unify! ?arg2 'cats trail)
                  (cont)))
              (undo-bindings! old-trail trail)
              (if (unify! ?arg1 'Sandy trail)
                (likes-2 ?arg2 'cats trail cont))
              (undo-bindings! old-trail trail)
              (if (unify! ?arg1 'Kim trail)
                (likes-2 ?arg2 'Lee trail
                         (fn [] (likes-2 ?arg2 'Kim trail cont))))))))

  (is (= (query ?x (likes Sandy ?x)) '(Robin))))


(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(deftest member-predicate

  (is (= (prolog-source 'member 2)
         '(defn member-2 [?arg1 ?arg2 trail cont]
            (let [old-trail (old-trail trail)]
              (if (unify! ?arg2 (cons ?arg1 (?)) trail)
                (cont))
              (undo-bindings! old-trail trail)
              (let [?rest (?)]
                (if (unify! ?arg2 (cons (?) ?rest) trail)
                  (member-2 ?arg1 ?rest trail cont)))))))

  (is (= (query ?x (member ?x [1 2 3])) [1 2 3])))

(comment
  (<-- (likes Kim Robin))
  (<- (likes Sandy Lee))
  (<- (likes Sandy Kim))
  (<- (likes Robin cats))
  (<- (likes Sandy ?x) (likes ?x cats))
  (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))

  (defn friends-of [person]
    (let [friends (transient [])]
      (prolog (lisp ?person person)
              (likes ?person ?x)
              (lisp (conj! friends ?x)))
      (persistent! friends)))

  (deftest test-prolog
    (is (= (friends-of 'Kim) '[Robin Sandy Kim])))
  )

; ******* Quicksort *******

(<-- (append nil ?ys ?ys))
(<- (append (?x . ?xs) ?ys (?x . ?zs))(append ?xs ?ys ?zs))

(<-- (quicksort (?x . ?xs) ?ys)
     (partition ?xs ?x ?littles ?bigs)
     (quicksort ?littles ?ls)
     (quicksort ?bigs ?bs)
     (append ?ls (?x . ?bs) ?ys))
(<- (quicksort nil nil))

(<-- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
     (lispp (<= ?x ?y))
     (partition ?xs ?y ?ls ?bs))
(<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
    (lispp (> ?x ?y))
    (partition ?xs ?y ?ls ?bs))
(<- (partition nil ?y nil nil))

(deftest test-quicksort

  )

; ******* Zebra Puzzle *******

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
     (= ?h ((house norwegian ? ? ? ?)                     ;1,10
            ?
            (house ? ? ? milk ?) ? ?))                    ; 9
     (member (house englishman ? ? ? red) ?h)             ; 2
     (member (house spaniard dog ? ? ?) ?h)               ; 3
     (member (house ? ? ? coffee green) ?h)               ; 4
     (member (house ukrainian ? ? tea ?) ?h)              ; 5
     (iright (house ? ? ? ? ivory)                        ; 6
             (house ? ? ? ? green) ?h)
     (member (house ? snails winston ? ?) ?h)             ; 7
     (member (house ? ? kools ? yellow) ?h)               ; 8
     (nextto (house ? ? chesterfield ? ?)                 ;11
             (house ? fox ? ? ?) ?h)
     (nextto (house ? ? kools ? ?)                        ;12
             (house ? horse ? ? ?) ?h)
     (member (house ? ? luckystrike oj ?) ?h)             ;13
     (member (house japanese ? parliaments ? ?) ?h)       ;14
     (nextto (house norwegian ? ? ? ?)                    ;15
             (house ? ? ? ? blue) ?h)
     (member (house ?w ? ? water ?) ?h)                   ;Q1
     (member (house ?z zebra ? ? ?) ?h))                  ;Q2

(deftest test-zebra
  (is (= (query ?z (zebra ?h ?w ?z)) ['japanese]))
  (is (= (query ?w (zebra ?h ?w ?x)) ['norwegian])))
