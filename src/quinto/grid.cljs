(ns quinto.grid
  (:require [com.rpl.specter :refer [select collect-one selected? INDEXED-VALS FIRST LAST ALL]]
            [clojure.spec.alpha :as s]
            [quinto.specs :as sp :refer [MAX-RUN-LENGTH GRID-HEIGHT GRID-WIDTH]]
            [quinto.specter :refer [grid-values]]))

(def empty-grid (vec (repeat GRID-WIDTH (vec (repeat GRID-HEIGHT nil)))))

(defn make-move
  "Applies `move` to `grid`."
  [grid move]
  (reduce (fn [grid [[x y] value]]
            (assert (nil? (get-in grid [x y])))
            (assoc-in grid [x y] value))
          grid
          move))

(s/fdef make-move
  :args (s/cat :grid ::sp/grid :move ::sp/move)
  :ret ::sp/grid)

(defn find-empty-cells [grid]
  (select [INDEXED-VALS (collect-one FIRST) LAST INDEXED-VALS (selected? LAST nil?) FIRST]
          grid))

(s/fdef find-empty-cells
  :args (s/cat :grid ::sp/grid)
  :ret (s/coll-of ::sp/cell))

(defn find-runs
  "Returns a list of [horizontal-run vertical-run], indicating the state of the board
  around this x,y position. For instance, on a board like this:

  1 5 4
      3
      3

  If you asked find-runs about the position immediately above that board's 4, you'd get
  [[0 0] [3 10]].

  If you asked find-runs about the position occupied by that board's bottommost 3, you'd get
  [[1 3] [3 10]]."
  [grid x y]
  (let [run-in-direction (fn [xdir ydir]
                           (let [values-in-direction (select [(grid-values (+ x xdir)
                                                                           (+ y ydir)
                                                                           (+ x (* xdir MAX-RUN-LENGTH))
                                                                           (+ y (* ydir MAX-RUN-LENGTH)))
                                                              ALL]
                                                             grid)
                                 run-values (take-while some? values-in-direction)]
                             [(count run-values) (apply + run-values)]))
        [[x-length-1 x-sum-1] [x-length-2 x-sum-2]] [(run-in-direction -1 0) (run-in-direction 1 0)]
        [[y-length-1 y-sum-1] [y-length-2 y-sum-2]] [(run-in-direction 0 -1) (run-in-direction 0 1)]
        cell-value (get-in grid [x y])]

    [[(+ x-length-1 x-length-2 (if (nil? cell-value) 0 1))
      (+ x-sum-1 x-sum-2 cell-value)]
     [(+ y-length-1 y-length-2 (if (nil? cell-value) 0 1))
      (+ y-sum-1 y-sum-2 cell-value)]]))

(s/fdef find-runs
  :args (s/cat :grid ::sp/grid :cell ::sp/cell)
  :ret (s/cat :horizontal-run (s/spec ::sp/run) :vertical-run (s/spec ::sp/run)))

(defn cell-is-playable? [grid [x y]]
  (let [[[x-run-length _] [y-run-length _]] (find-runs grid x y)]
    (and (nil? (get-in grid [x y]))
         (or (> x-run-length 0) (> y-run-length 0))
         (and (< x-run-length MAX-RUN-LENGTH) (< y-run-length MAX-RUN-LENGTH)))))

(defn find-playable-cells
  "Returns a list of all of the cells where a move can be started."
  [grid]
  ; XXX this should return [[middle-cell-x middle-cell-y]] if the grid is empty
  (filter #(cell-is-playable? grid %) (find-empty-cells grid)))

(defn cell-is-blocked? [grid [x y]]
  (let [[[x-run-length _] [y-run-length _]] (find-runs grid x y)]
    (and (nil? (get-in grid [x y]))
         (or (>= x-run-length MAX-RUN-LENGTH) (>= y-run-length MAX-RUN-LENGTH)))))

(defn find-blocked-cells
  "Returns a list of all of the cells that no move can include."
  [grid]
  (filter #(cell-is-blocked? grid %) (find-empty-cells grid)))

(defn is-grid-valid? [grid]
  (every?
    identity
    (apply concat
           (for [x (range (count grid))]
             (for [y (range (count (grid x)))]
               (let [[[x-run-length x-run-sum] [y-run-length y-run-sum]] (find-runs grid x y)]
                 (and (<= 0 x-run-length MAX-RUN-LENGTH)
                      (<= 0 y-run-length MAX-RUN-LENGTH)

                      ; If this cell isn't part of a 2+ length run, its run's sum
                      ; doesn't have to be a multiple of 5.
                      ; e.g. if the only filled cells on the board have values [5 3 2],
                      ; and you're examining the cell with value 3 from an axis that's
                      ; perpendicular to the axis of that run, it's fine that 3 isn't a multiple of 5.
                      (or (<= x-run-length 1)
                          (= 0 (mod x-run-sum 5)))
                      (or (<= y-run-length 1)
                          (= 0 (mod y-run-sum 5))))))))))

(s/fdef is-grid-valid?
  :args (s/cat :grid ::sp/grid)
  :ret boolean?)
