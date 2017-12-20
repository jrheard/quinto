(ns quinto.grid
  (:require [com.rpl.specter :refer [select collect-one selected? INDEXED-VALS FIRST LAST ALL]]
            [clojure.set :refer [intersection]]
            [clojure.spec.alpha :as s]
            [quinto.specs :as sp :refer [MAX-RUN-LENGTH GRID-HEIGHT GRID-WIDTH]]
            [quinto.specter :refer [grid-values indexed-grid-values]]))

(def empty-grid (vec (repeat GRID-WIDTH (vec (repeat GRID-HEIGHT nil)))))

(declare is-grid-valid?)

(defn make-move
  "Applies `move` to `grid`."
  [grid move]
  (let [new-grid (reduce (fn [grid [[x y] value]]
                           (assert (nil? (get-in grid [x y])))
                           (assoc-in grid [x y] value))
                         grid
                         move)]

    (assert (is-grid-valid? new-grid))
    new-grid))

(s/fdef make-move
  :args (s/cat :grid ::sp/grid :move ::sp/move)
  :ret ::sp/grid)

(defn find-empty-cells [grid]
  (select [INDEXED-VALS (collect-one FIRST) LAST
           INDEXED-VALS (selected? LAST nil?) FIRST]
          grid))

(s/fdef find-empty-cells
  :args (s/cat :grid ::sp/grid)
  :ret (s/coll-of ::sp/cell))

(defn -find-runs
  "Returns a list of [horizontal-run vertical-run], indicating the state of the board
  around this x,y position. For instance, on a board like this:

  1 5 4
      3
      3

  If you ask find-runs about the position immediately above that board's 4, you'll get
  [[0 0] [3 10]].

  If you ask find-runs about the position occupied by that board's bottommost 3, you'll get
  [[1 3] [3 10]]."
  [grid x y]
  (let [run-in-direction (fn [xdir ydir]
                           (let [values-in-direction (select (grid-values (+ x xdir)
                                                                          (+ y ydir)
                                                                          (+ x (* xdir MAX-RUN-LENGTH))
                                                                          (+ y (* ydir MAX-RUN-LENGTH)))
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

(def find-runs (memoize -find-runs))

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

(defn find-next-open-cells-for-move
  "Returns a vector like [[3 5] [3 7] [4 6]] indicating which cells on `grid` can
  be used as part of a `move` that's being assembled by the user."
  [grid move]
  (let [[[x y] _] (first move)
        playable-cells (set (find-playable-cells grid))
        directions-to-check (if (= (count move) 1)
                              [[-1 0] [1 0] [0 -1] [0 1]]

                              (let [xs (set (select [ALL FIRST FIRST] move))]
                                (if (= (count xs) 1)
                                  [[0 1] [0 -1]]
                                  [[1 0] [-1 0]])))]

    (for [[xdir ydir] directions-to-check

          :let [nil-cells (select [(indexed-grid-values (+ x xdir)
                                                        (+ y ydir)
                                                        (+ x (* xdir MAX-RUN-LENGTH))
                                                        (+ y (* ydir MAX-RUN-LENGTH)))
                                   (selected? LAST nil?)
                                   FIRST]
                                  grid)
                first-playable-cell-in-direction (first (filter playable-cells nil-cells))]

          :when first-playable-cell-in-direction]
      first-playable-cell-in-direction)))

(s/fdef find-next-open-cells-for-move
  :args (s/cat :grid ::sp/grid :move ::sp/move)
  :ret (s/coll-of ::sp/cell))

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
