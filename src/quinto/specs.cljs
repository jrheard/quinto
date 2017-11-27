(ns quinto.specs
  (:require [clojure.spec.alpha :as s]))

; a cell is "empty" if its value is nil
; a cell is "filled" if it has a non-nil value
; a cell is "playable" if it is possible for a player to make a move on that cell
; a cell is "blocked" if it is not possible for a player to make a move on that cell

(s/def ::cell (s/cat :x nat-int? :y nat-int?))
(s/def ::value (s/and nat-int? #(<= % 9)))

(s/def ::move-component (s/cat :cell (s/spec ::cell) :value ::value))
(s/def ::move (s/coll-of ::move-component))

; several "filled" cell
(s/def ::run-length nat-int?)
(s/def ::run-sum pos-int?)
(s/def ::run (s/cat :length ::run-length :sum ::run-sum))

(s/def ::grid (s/coll-of (s/coll-of (s/or :value ::value :nil nil?))))

(comment
  (s/valid? ::cell [1 5])
  (s/valid? ::value -1)
  (s/explain ::move-component [[1 5] 3])
  (s/valid? ::move [[[1 5] 3] [[1 6] 9]])
  (s/valid? ::grid [[5 3 2] [nil 2 3]])
  (s/valid? ::run [5 10])
  (s/describe ::move)
  )