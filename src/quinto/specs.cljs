(ns quinto.specs
  (:require [clojure.spec.alpha :as s]))

; a cell is "empty" if its value is nil
; a cell is "filled" if it has a non-nil value
; a cell is "playable" if it is possible for a player to make a move on that cell
; a cell is "blocked" if it is not possible for a player to make a move on that cell

(s/def ::cell (s/cat :x nat-int? :y nat-int?))
(s/def ::value (s/and nat-int? #(<= % 9)))

(s/def ::deck (s/coll-of ::value))
(s/def ::hand (s/coll-of ::value :min-count 0 :max-count 5))

(s/def ::move-component (s/cat :cell (s/spec ::cell) :value ::value))
(s/def ::move (s/coll-of ::move-component))

; A number of filled cells in a row is called a "run".
(def MAX-RUN-LENGTH 5)
(s/def ::run-length (and nat-int? #(<= % MAX-RUN-LENGTH)))
(s/def ::run-sum nat-int?)
(s/def ::run (s/cat :length ::run-length :sum ::run-sum))

; There's some confusion as to what the canonical board size is. Picking 13x13 arbitrarily.
; See https://boardgamegeek.com/thread/24859/tile-distribution for some actual board sizes.
(def GRID-WIDTH 13)
(def GRID-HEIGHT 13)
(s/def ::grid (s/coll-of
                (s/coll-of (s/or :value ::value :nil nil?) :count GRID-HEIGHT)
                :count GRID-WIDTH))

(comment
  (s/valid? ::cell [1 5])
  (s/valid? ::value -1)
  (s/explain ::move-component [[1 5] 3])
  (s/valid? ::move [[[1 5] 3] [[1 6] 9]])
  (s/valid? ::grid [[5 3 2] [nil 2 3]])
  (s/valid? ::run [5 10])

  (s/explain
    (s/cat :horizontal-run ::run :vertical-run ::run)
    [[0 0] [0 0]])

  (s/describe ::move)
  )