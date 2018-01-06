(ns quinto.ai
  (:require [clojure.set :refer [intersection]]
            [clojure.spec.alpha :as s]
            [com.rpl.specter :refer [select ALL]]
            [quinto.grid :as g]
            [quinto.specs :as sp :refer [MAX-RUN-LENGTH]]
            [quinto.utils :refer [remove-item cell-is-on-grid]]))

(s/def ::move-direction #{:horizontal :vertical})

(defn -cell-value-is-definitely-invalid?
  "Used when speculatively placing a value at a particular cell. Returns true
  if the runs created by placing this value at this cell would _definitely_ cause the board
  to end up in an invalid state, false otherwise."
  [[horizontal-length horizontal-sum] [vertical-length vertical-sum] move-direction]
  (or
    ; A cell value is definitely invalid if it would create a too-long run.
    (> horizontal-length MAX-RUN-LENGTH)
    (> vertical-length MAX-RUN-LENGTH)

    ; A cell value is definitely invalid if it's part of a horizontal move
    ; and it causes an invalid vertical run to exist.
    (and
      (= move-direction :horizontal)
      (> vertical-length 1)
      (not= (mod vertical-sum 5) 0))

    ; The same thing goes for vertical moves and horizontal runs.
    (and
      (= move-direction :vertical)
      (> horizontal-length 1)
      (not= (mod horizontal-sum 5) 0))))

(s/fdef -cell-value-is-definitely-invalid?
  :args (s/cat :horizontal-run (s/spec ::sp/run) :vertical-run (s/spec ::sp/run) :move-direction ::move-direction)
  :ret boolean?)

(defn all-moves-for-cells-and-hand
  "Takes a grid, a hand, and a sequence of cells to try placing `hand`'s values at,
  and returns a sequence of all of the possible valid moves for this configuration of grid+cells+hand."
  ([grid available-cells-for-move hand move-direction]
   (all-moves-for-cells-and-hand grid available-cells-for-move hand move-direction #{} #{}))

  ([grid available-cells-for-move hand move-direction valid-moves-seen move-so-far]
   (if (or (empty? available-cells-for-move)
           (empty? hand))
     ; If there aren't any cells or values left for us to use, that's the end of this particular path of investigation.
     valid-moves-seen

     ; Otherwise, let's try placing a value at the first cell.
     (let [[x y] (first available-cells-for-move)]
       (if (not (nil? (get-in grid [x y])))
         ; If this cell's value is non-nil, we can't place any of our tiles here, so let's move on.
         (all-moves-for-cells-and-hand grid
                                       (rest available-cells-for-move)
                                       hand
                                       move-direction
                                       valid-moves-seen
                                       move-so-far)

         ; If this cell's value _is_ nil, let's try placing each value in `hand` at this cell, one at a time.
         (apply concat
                (for [value hand]
                  ; Try placing this value at x, y and see what happens.
                  (let [grid-with-value (assoc-in grid [x y] value)
                        [[horizontal-length horizontal-sum]
                         [vertical-length vertical-sum]] (g/find-runs grid-with-value x y)]

                    (cond
                      ; If placing this value here clearly makes for an invalid board state,
                      ; then this move and all further moves in this direction are invalid, so we should
                      ; bail here and just return the list of any valid moves that have been recorded so far.
                      (-cell-value-is-definitely-invalid? [horizontal-length horizontal-sum]
                                                          [vertical-length vertical-sum]
                                                          move-direction)
                      valid-moves-seen

                      ; If placing this specific value here is a valid move, record it and keep looking for more!
                      (and (or (= horizontal-length 1)
                               (= (mod horizontal-sum 5) 0))
                           (or (= vertical-length 1)
                               (= (mod vertical-sum 5) 0)))
                      (all-moves-for-cells-and-hand grid-with-value
                                                    (rest available-cells-for-move)
                                                    (remove-item hand value)
                                                    move-direction
                                                    (conj valid-moves-seen
                                                          (conj move-so-far [[x y] value]))
                                                    (conj move-so-far [[x y] value]))

                      ; If placing this specific value here is not a valid move, it still might be made valid
                      ; later on, so keep looking.
                      :else
                      (all-moves-for-cells-and-hand grid-with-value
                                                    (rest available-cells-for-move)
                                                    (remove-item hand value)
                                                    move-direction
                                                    valid-moves-seen
                                                    (conj move-so-far [[x y] value])))))))))))

(s/fdef all-moves-for-cells-and-hand
  :args (s/cat :grid ::sp/grid :available-cells-for-move (s/coll-of ::sp/cell) :hand ::sp/hand
               :move-direction ::move-direction :valid-moves-seen (s/? (s/coll-of ::sp/move))
               :move-so-far (s/? ::sp/move))
  :ret (s/coll-of ::sp/move))

(defn moves-in-direction
  "Returns all of the possible moves starting at [x, y] and heading in [xdir, ydir]
  with the given values of `grid` and `hand`."
  [grid hand x y xdir ydir]
  (let [[[horizontal-length _] [vertical-length _]] (g/find-runs grid x y)
        move-direction (if (= xdir 0) :vertical :horizontal)
        relevant-run-length (if (= move-direction :vertical) vertical-length horizontal-length)

        ; Naively generate a list of all the possible cells that are
        ; potentially available for a move in this direction.
        potential-cells-for-move (for [i (range MAX-RUN-LENGTH)]
                                   [(+ x (* xdir i))
                                    (+ y (* ydir i))])

        ; Filter that list for only the cells that would allow this move to be valid.
        valid-cells-for-move (reduce (fn [available-cells [x y]]
                                       ; A move can only consist of cells that are _on_ the grid in the first place,
                                       ; and a move can't create a run that's >= MAX-RUN-LENGTH.
                                       (if (and (cell-is-on-grid x y)
                                                (<= (+ relevant-run-length (count available-cells))
                                                    MAX-RUN-LENGTH))
                                         (conj available-cells [x y])
                                         (reduced available-cells)))
                                     []
                                     potential-cells-for-move)]

    ; valid-cells-for-move should contain _at least_ one cell,
    ; because it's a precondition of this function that [x y] must be a playable cell.
    (assert (seq valid-cells-for-move))
    (all-moves-for-cells-and-hand grid
                                  valid-cells-for-move
                                  hand
                                  move-direction)))

(s/fdef moves-in-direction
  :args (s/cat :grid ::sp/grid :hand ::sp/hand :cell ::sp/cell :xdir int? :ydir int?)
  :ret (s/coll-of ::sp/move))

(defn moves-for-cell
  "Returns all of the possible valid moves that can begin at [x y], using the values in `hand`."
  [grid hand x y]
  (let [moves-in-direction (partial moves-in-direction grid hand x y)]
    (concat
      (moves-in-direction 1 0)
      (moves-in-direction -1 0)
      (moves-in-direction 0 1)
      (moves-in-direction 0 -1))))

(s/fdef moves-for-cell
  :args (s/cat :grid ::sp/grid :hand ::sp/hand :cell ::sp/cell)
  :ret (s/coll-of ::sp/move))

(defn pick-move
  "Returns the highest-scoring move that can be made using this hand on this board."
  [grid hand]
  (let [playable-cells (g/find-playable-cells grid)
        moves (into #{}
                    (mapcat #(apply moves-for-cell grid hand %) playable-cells))]

    (apply max-key #(g/score-move grid %) moves)))

(s/fdef pick-move
  :args (s/cat :grid ::sp/grid :hand ::sp/hand)
  :ret ::sp/move)
