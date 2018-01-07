(ns quinto.grid
  (:require [com.rpl.specter :refer [select collect-one selected? INDEXED-VALS FIRST LAST ALL]]
            [clojure.set :refer [intersection]]
            [clojure.spec.alpha :as s]
            [quinto.specs :as sp :refer [MAX-RUN-LENGTH GRID-HEIGHT GRID-WIDTH]]
            [quinto.specter :refer [grid-values indexed-grid-values]]
            [quinto.utils :refer [index-of-first-truthy-item]]))

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
  (let [playable-cells (filter #(cell-is-playable? grid %) (find-empty-cells grid))]
    (if (seq playable-cells)
      playable-cells
      [[(quot GRID-WIDTH 2) (quot GRID-HEIGHT 2)]])))

(defn cell-is-blocked? [grid [x y]]
  (let [[[x-run-length _] [y-run-length _]] (find-runs grid x y)]
    (and (nil? (get-in grid [x y]))
         (or (>= x-run-length MAX-RUN-LENGTH) (>= y-run-length MAX-RUN-LENGTH)))))

(defn find-blocked-cells
  "Returns a list of all of the cells that no move can include."
  [grid]
  (filter #(cell-is-blocked? grid %) (find-empty-cells grid)))

(defn find-filled-cells
  "Returns a list of all of the cells that contain a non-nil value."
  [grid]
  (select [ALL ALL some?] grid))

(defn find-next-open-cells-for-move
  "Returns a vector like [[3 5] [3 7] [4 6]] indicating which cells on `grid` can
  be used as part of a `move` that's being assembled by the user."
  [grid move]
  (let [[[x y] _] (first move)
        playable-cells (set (find-playable-cells grid))
        blocked-cells (set (find-blocked-cells grid))
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

                index-of-first-blocked-cell (index-of-first-truthy-item (map blocked-cells nil-cells))
                index-of-first-playable-cell (index-of-first-truthy-item (map playable-cells nil-cells))

                first-playable-cell-in-direction (first (filter playable-cells nil-cells))]

          :when (and first-playable-cell-in-direction
                     ; If there's a blocked cell between us and the first playable cell,
                     ; that cell is not in fact playable for this move.
                     ; Blocked cells are empty, and moves can't have an empty cell in the middle.
                     (not
                       (and (> index-of-first-blocked-cell -1)
                            (< index-of-first-blocked-cell index-of-first-playable-cell))))]

      first-playable-cell-in-direction)))

(s/fdef find-next-open-cells-for-move
  :args (s/cat :grid ::sp/grid :move ::sp/move)
  :ret (s/coll-of ::sp/cell))

(defn score-move
  "Returns a number (a multiple of 5, like 10 or 25, etc) indicating the score that would
  be earned by a player who applies `move` to `grid`."
  [grid move]
  (if (and (= (count (find-empty-cells grid)) (* GRID-WIDTH GRID-HEIGHT))
           (= (count move) 1))
    ; Special case to support calculating a player's tentative score if they place
    ; a 5 on an empty board.
    (second (first move))

    (let [move-x-values (select [ALL 0 0] move)
          move-direction (if (apply = move-x-values) :vertical :horizontal)

          grid-with-move (make-move grid move)
          runs (for [[[x y] _] move]
                 (find-runs grid-with-move x y))
          horizontal-runs (select [ALL 0] runs)
          vertical-runs (select [ALL 1] runs)

          primary-direction-runs (if (= move-direction :horizontal) horizontal-runs vertical-runs)
          perpendicular-runs (if (= move-direction :horizontal) vertical-runs horizontal-runs)
          [a-primary-run-length a-primary-run-sum] (first primary-direction-runs)]

      ; A move's score is:
      ; The sum of the run of values along its "primary direction", plus
      ; all the sums of the runs of values along its "perpendicular direction".
      ;
      ; So on a board like this:
      ; 5 4 1
      ;     2
      ;     2 3
      ;
      ; If you make a move that would cause the board to look like this:
      ; 5 4 1
      ;   1 2 7 5
      ;     2 3
      ;
      ; Then that move's score is:
      ; The run sum along that move's horizontal axis (1 + 2 + 7 + 5 = 15), plus
      ; The run sum along the vertical axis of the first cell in the move (1 + 4), plus
      ; The run sum along the vertical axis of the second cell in the move (7 + 3), plus
      ; The run sum along the vertical axis of the third and final cell in the move.
      ; But the third cell of the move is that single number 5, and that cell's vertical run
      ; has length 1, so it doesn't qualify to be included in the score; perpendicular-axis
      ; run sums only count if those runs are of length > 1.
      ; So the final score for that move is
      ; (1 + 2 + 7 + 5) = 15, plus
      ; (1 + 4) = 5, plus
      ; (7 + 3) = 10,
      ; which is 15 + 5 + 10, which is 30.

      (apply +
             (if (> a-primary-run-length 1) a-primary-run-sum 0)
             (select [ALL (fn [[run-length _]] (> run-length 1)) 1]
                     perpendicular-runs)))))

(s/fdef score-move
  :args (s/cat :grid ::sp/grid :move ::sp/move)
  :ret pos-int?)

(defn is-grid-valid? [grid]
  (let [filled-cells (find-filled-cells grid)]
    (if (= (count filled-cells) 1)
      (= (mod (first filled-cells) 5) 0)

      (every?
        identity
        (apply concat
               (for [x (range (count grid))]
                 (for [y (range (count (grid x)))]
                   (let [[[x-run-length x-run-sum] [y-run-length y-run-sum]] (find-runs grid x y)]
                     (or (nil? (get-in grid [x y]))
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
                                  (= 0 (mod y-run-sum 5)))))))))))))

(s/fdef is-grid-valid?
  :args (s/cat :grid ::sp/grid)
  :ret boolean?)

(defn is-move-valid?
  [grid move]
  (and (is-grid-valid? (make-move grid move))
       (= (mod (score-move grid move) 5) 0)))
