(ns quinto.specs
  (:require [clojure.spec.alpha :as s]))


(s/def ::cell (s/tuple nat-int? nat-int?))
(s/def ::value (s/and nat-int? #(<= % 9)))

(s/def ::move-component (s/cat :cell ::cell :value ::value))
(s/def ::move (s/coll-of ::move-component))

(s/def ::grid (s/coll-of (s/coll-of (s/or :value ::value :nil nil?))))

(comment
  (s/valid? ::cell [1 5])
  (s/valid? ::value -1)
  (s/valid? ::move-component [[1 5] 3])
  (s/valid? ::move [[[1 5] 3] [[1 6] 9]])
  (s/valid? ::grid [[5 3 2] [nil 2 3]])
  )