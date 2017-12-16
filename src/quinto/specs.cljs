(ns quinto.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::cell (s/cat :x nat-int? :y nat-int?))
(s/def ::value (s/and nat-int? #(<= % 9)))

(s/def ::deck (s/coll-of ::value))
(s/def ::hand (s/coll-of ::value :min-count 0 :max-count 5))

(s/def ::move-component (s/cat :cell (s/spec ::cell) :value ::value))
(s/def ::move (s/coll-of ::move-component))

; A number of filled cells in a row is called a "run".
(def MAX-RUN-LENGTH 5)
(s/def ::run-length nat-int?)
(s/def ::run-sum nat-int?)
(s/def ::run (s/cat :length ::run-length :sum ::run-sum))

; There's some confusion as to what the canonical board size is. Picking 13x13 arbitrarily.
; See https://boardgamegeek.com/thread/24859/tile-distribution for some actual board sizes.
(def GRID-WIDTH 13)
(def GRID-HEIGHT 13)
(s/def ::grid (s/coll-of
                (s/coll-of (s/or :value ::value :nil nil?) :count GRID-HEIGHT)
                :count GRID-WIDTH))

