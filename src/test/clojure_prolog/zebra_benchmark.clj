(ns clojure-prolog.zebra_benchmark
  (:require [criterium.core :refer :all]
            [clojure-prolog.core :refer :all]))

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

(bench (query ?z (zebra ?h ?w ?z)))