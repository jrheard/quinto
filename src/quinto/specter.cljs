(ns quinto.specter
  (:require [com.rpl.specter :refer [multi-path nthpath STOP]]
            [quinto.grid :refer [cell-is-on-grid]]
            [quinto.utils :refer [bound-between]]
            [quinto.specs :refer [GRID-WIDTH GRID-HEIGHT]]))

(defn ^:direct-nav grid-values
  [x1 y1 x2 y2]
  (if (cell-is-on-grid x1 y1)
    (let [x2 (bound-between x2 0 (dec GRID-WIDTH))
          y2 (bound-between y2 0 (dec GRID-HEIGHT))]

      (reduce
        multi-path
        (for [x (if (< x1 x2)
                  (range x1 (inc x2))
                  (reverse (range x2 (inc x1))))
              y (if (< y1 y2)
                  (range y1 (inc y2))
                  (reverse (range y2 (inc y1))))]
          (nthpath x y))))

    STOP))
