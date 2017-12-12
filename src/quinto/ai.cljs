(ns quinto.ai
  (:require [clojure.set :refer [intersection]]
            [clojure.spec.alpha :as s]
            [quinto.grid :as g]
            [quinto.specs :as sp :refer [MAX-RUN-LENGTH]]))

; research project: rewrite this module in core.logic (haha don't though)

(defn remove-item [xs x]
  (cond (empty? xs) xs
        (= x (first xs)) (rest xs)
        :else (cons (first xs) (remove-item (rest xs) x))))

(defn all-moves-for-cells
  "DOCUMENT ME"
  ([grid hand available-cells-for-move xdir ydir]
    ; TODO consider making this a set, there may be dupes
   (filter #(not (empty? %))
           (all-moves-for-cells grid hand available-cells-for-move xdir ydir [] [])))

  ([grid hand available-cells-for-move xdir ydir valid-moves-seen move-so-far]
   (if (empty? available-cells-for-move)
     ; If there aren't any cells left for us to use, that's the end of this particular path of investigation.
     valid-moves-seen

     (let [[x y] (first available-cells-for-move)]
       (if (not (nil? (get-in grid [x y])))
         ; If this cell's value is non-nil, we can't place any of our tiles there, so let's move on.
         (all-moves-for-cells grid
                              hand
                              (rest available-cells-for-move)
                              xdir
                              ydir
                              valid-moves-seen
                              move-so-far)

         ; If this cell's value _is_ nil, let's try placing each value in `hand` at this cell, one at a time.
         (apply concat
           (for [value hand]
             ; Try placing this value at x, y and see what happens.
             (let [grid-with-value (assoc-in grid [x y] value)
                   [[horizontal-length horizontal-sum] [vertical-length vertical-sum]] (g/find-runs grid-with-value x y)]

               (cond
                 ; If placing a value here would cause a run to be longer than MAX-RUN-LENGTH,
                 ; this move and all further moves in this direction are invalid, so we should
                 ; bail here and just return the list of any valid moves that have been recorded so far.
                 (or (>= horizontal-length MAX-RUN-LENGTH)
                     (>= vertical-length MAX-RUN-LENGTH))
                 valid-moves-seen

                 ; If placing this specific value here is a valid move, record it and keep looking for more!
                 (and (= (mod horizontal-sum 5) 0)
                      (= (mod vertical-sum 5) 0))
                 (all-moves-for-cells grid-with-value
                                      (remove-item hand value)
                                      (rest available-cells-for-move)
                                      xdir
                                      ydir
                                      (conj valid-moves-seen
                                            (conj move-so-far [[x y] value]))
                                      (conj move-so-far [[x y] value]))

                 ; If placing a tile here messes up the horizontal run's sum,
                 #_(or (and (not= (mod horizontal-sum 5) 0)
                          (not= xdir 0)
                          )
                     )

                 ; If placing this specific value here is not a valid move, it still might be made valid
                 ; later on, so keep looking.
                 ;
                 ; xxx i'm not sure this is right - it depends on the direction we're heading, right?
                 :else
                 (all-moves-for-cells grid-with-value
                                      (remove-item hand value)
                                      (rest available-cells-for-move)
                                      xdir
                                      ydir
                                      valid-moves-seen
                                      (conj move-so-far [[x y] value])))))))))))

(s/fdef all-moves-for-cells
  :args (s/cat :grid ::sp/grid :hand ::sp/hand :available-cells-for-move (s/coll-of ::sp/cell)
               :xdir int? :ydir int? :valid-moves-seen (s/coll-of ::sp/move) :move-so-far ::sp/move)
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

    (all-moves-for-cells grid hand available-cells-for-move xdir ydir [] [])))

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
    (first moves)
    )
  )

(s/fdef pick-move
  :args (s/cat :grid ::sp/grid :hand ::sp/hand)
  :ret ::sp/move)
