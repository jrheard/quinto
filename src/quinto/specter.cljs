(ns quinto.specter
  (:require [com.rpl.specter :refer [defnav multi-path nthpath STOP]]
            [quinto.utils :refer [bound-between cell-is-on-grid]]
            [quinto.specs :refer [GRID-WIDTH GRID-HEIGHT]]))

(defnav
  grid-values
  [x1 y1 x2 y2]
  (select* [this structure next-fn]
           (assert (or (= x1 x2)
                       (= y1 y2)))

           (next-fn
             (if (cell-is-on-grid x1 y1)
               (let [x2 (bound-between x2 0 (dec GRID-WIDTH))
                     y2 (bound-between y2 0 (dec GRID-HEIGHT))]
                 (if (= x1 x2)
                   (let [column (nth structure x1)]
                     (if (< y1 y2)
                       (subvec column y1 (inc y2))
                       (reverse (subvec column y2 (inc y1)))))

                   (for [x (if (< x1 x2)
                             (range x1 (inc x2))
                             (reverse (range x2 (inc x1))))]
                     (-> structure
                         (nth x)
                         (nth y1)))))
               [])))

  (transform* [this structure next-fn]
              (assert false)))
