(ns quinto.ai
  (:require [clojure.set :refer [intersection]]
            [clojure.spec.alpha :as s]
            [quinto.grid :as g]
            [quinto.specs :as sp :refer [MAX-RUN-LENGTH]]))

; research project: write this in core.logic (haha don't though)

(defn remove-value-from-hand [hand value]
  (concat
    (take-while #(not= % value) hand)
    (rest (drop-while #(not= % value) hand))))

; TODO - how do we collect all the disparate generated values of move-so-far?
(defn all-moves-for-cells
  ([grid hand available-cells-for-move]
   (all-moves-for-cells grid hand available-cells-for-move []))

  ([grid hand available-cells-for-move move-so-far]
   (js/console.log "all-moves-for-cells" hand available-cells-for-move move-so-far)
   (if (not (seq available-cells-for-move))
     ; If there aren't any cells left for us to use, that's the end of this move.
     [move-so-far]

     (let [[x y] (first (available-cells-for-move))
           [[horizontal-length horizontal-sum] [vertical-length vertical-sum]] (g/find-runs grid x y)
           valid-values-for-cell (filter (fn [value]
                                           (and
                                             (= (mod (+ horizontal-sum value) 5) 0)
                                             (= (mod (+ vertical-sum value) 5) 0)))
                                         (range 10))
           valid-values-in-hand (intersection (set hand) (set valid-values-for-cell))]

       (cond
         (nil? valid-values-in-hand)
         [move-so-far]

         (or (>= horizontal-length MAX-RUN-LENGTH)
             (>= vertical-length MAX-RUN-LENGTH))
         [move-so-far]

         (not (nil? (get-in grid [x y])))
         (all-moves-for-cells grid
                              hand
                              (rest available-cells-for-move)
                              move-so-far)

         :else
         (for [value valid-values-in-hand]
           (all-moves-for-cells (assoc-in grid [x y] value)
                                (remove-value-from-hand hand value)
                                (rest available-cells-for-move)
                                (conj move-so-far [[x y] value]))))))))

(s/fdef all-moves-for-cells
  :args (s/cat :grid ::sp/grid :hand ::sp/hand :available-cells-for-move (s/coll-of ::sp/cell) :move-so-far ::sp/move)
  :ret (s/coll-of ::sp/move))

(defn moves-in-direction [grid hand x y xdir ydir]
  (let [[[horizontal-length _] [vertical-length _]] (g/find-runs grid x y)
        relevant-run-length (if (= xdir 0) vertical-length horizontal-length)
        ; Naively generate a list of all the possible cells that are potentially available for a move in this direction.
        potential-cells-for-move (for [i (range MAX-RUN-LENGTH)]
                                   [(+ x (* xdir i))
                                    (+ y (* ydir i))])
        ; Filter that list for only the cells that would allow this move to be valid.
        available-cells-for-move (reduce (fn [available-cells [x y]]
                                           ; A move can only consist of cells that are _on_ the grid in the first place,
                                           ; and a move can't create a run that's >= MAX-RUN-LENGTH.
                                           (if (and (g/cell-is-on-grid grid x y)
                                                    (< (+ relevant-run-length (count available-cells))
                                                       MAX-RUN-LENGTH))
                                             (conj available-cells [x y])
                                             (reduced available-cells)))
                                         []
                                         potential-cells-for-move)]
    ; available-cells-for-move should contain _at least_ the cell [x y],
    ; because it's a precondition of this function that [x y] must be a playable cell.
    (assert (seq available-cells-for-move))

    (all-moves-for-cells grid hand available-cells-for-move)))

(s/fdef moves-in-direction
  :args (s/cat :grid ::sp/grid :hand ::sp/hand :cell ::sp/cell :xdir int? :ydir int?)
  :ret (s/coll-of ::sp/move))

(defn moves-for-cell [grid hand x y]
  (let [moves-in-direction (partial moves-in-direction grid hand x y)]
    (concat
      (moves-in-direction 1 0)
      (moves-in-direction -1 0)
      (moves-in-direction 0 1)
      (moves-in-direction 0 -1))))

(s/fdef moves-for-cell
  :args (s/cat :grid ::sp/grid :hand ::sp/hand :cell ::sp/cell)
  :ret (s/coll-of ::sp/move))

(defn pick-move [grid hand]
  (let [playable-cells (g/find-playable-cells grid)
        ; first, generate all possible moves
        moves (mapcat #(apply moves-for-cell grid hand %) playable-cells)]

    ; then, compare them and return the one with the highest score
    ; TODO - a score-move function (in grid? maybe put it in grid to start, then eventually move to quinto.score if necessary)
    moves
    )
  )

(s/fdef pick-move
  :args (s/cat :grid ::sp/grid :hand ::sp/hand)
  :ret ::sp/move)
