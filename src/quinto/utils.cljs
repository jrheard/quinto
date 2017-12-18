(ns quinto.utils
  (:require [clojure.spec.alpha :as s]
            [quinto.specs :refer [GRID-WIDTH GRID-HEIGHT]]))

(defn remove-item [xs x]
  "Removes the first instance of the value x from the collection xs."
  (cond (empty? xs) xs
        (= x (first xs)) (rest xs)
        :else (cons (first xs) (remove-item (rest xs) x))))

(defn bound-between
  [number lower upper]
  (cond
    (< number lower) lower
    (> number upper) upper
    :else number))

(defn cell-is-on-grid [x y]
  (and (>= x 0)
       (< x GRID-WIDTH)
       (>= y 0)
       (< y GRID-HEIGHT)))

(s/fdef cell-is-on-grid
  :args (s/cat :cell (s/cat :x int? :y int?))
  :ret boolean?)
