(ns quinto.specter
  (:require [com.rpl.specter :refer [defnav multi-path nthpath STOP]]
            [quinto.utils :refer [bound-between cell-is-on-grid]]
            [quinto.specs :refer [GRID-WIDTH GRID-HEIGHT]]))

(defnav
  ^{:doc "Navigates to values in a _straight, axis-aligned line_ between two points on a grid.
  Tuned to be pretty fast."}
  grid-values
  [x1 y1 x2 y2]

  (select* [this structure next-fn]
           (assert (or (= x1 x2)
                       (= y1 y2)))

           (if (not (cell-is-on-grid x1 y1))
             ; If your starting cell isn't on the grid, you get nothing.
             []

             (let [x2 (bound-between x2 0 (dec GRID-WIDTH))
                   y2 (bound-between y2 0 (dec GRID-HEIGHT))]

               ; If you're just going vertically, things are easy,
               ; we can just grab your column and subvec it.
               (if (= x1 x2)
                 (let [column (nth structure x1)]
                   (doseq [value (if (< y1 y2)
                                   (subvec column y1 (inc y2))
                                   (reverse (subvec column y2 (inc y1))))]
                     (next-fn value)))


                 ; If we're going horizontally, we need to do two `nth` calls per element.
                 ; This isn't great for performance, so we have some hand-optimizied `loop`s here.
                 (if (< x1 x2)
                   ; If x1 is less than x2, then loop forward across the x-axis.
                   (loop [x x1]
                     (next-fn (-> structure
                                  (nth x)
                                  (nth y1)))
                     (when (< x x2)
                       (recur (inc x))))

                   ; Otherwise, loop backward across the x-axis.
                   (loop [x x1]
                     (next-fn (-> structure
                                  (nth x)
                                  (nth y1)))

                     (when (> x x2)
                       (recur (dec x)))))))))

  (transform* [this structure next-fn]
              (assert false)))

; indexed-grid-values duplicates a lot of stuff from grid-values. This is fine and not worth stressing about.
(defnav
  ^{:doc "Navigates to positions and values in a _straight, axis-aligned line_ between two points on a grid.
  Not fast - don't call this in a super-tight loop."}
  indexed-grid-values
  [x1 y1 x2 y2]

  (select* [this structure next-fn]
           (assert (or (= x1 x2)
                       (= y1 y2)))

           (if (not (cell-is-on-grid x1 y1))
             ; If your starting cell isn't on the grid, you get nothing.
             []

             (let [x2 (bound-between x2 0 (dec GRID-WIDTH))
                   y2 (bound-between y2 0 (dec GRID-HEIGHT))]
               (doseq [x (if (= x1 x2)
                         [x1]
                         (if (< x1 x2)
                           (range x1 (inc x2))
                           (reverse (range x2 (inc x1)))))
                     y (if (= y1 y2)
                         [y1]
                         (if (< y1 y2)
                           (range y1 (inc y2))
                           (reverse (range y2 (inc y1)))))]
                 (next-fn [[x y] (get-in structure [x y])])))))

  (transform* [this structure next-fn]
              (assert false)))
