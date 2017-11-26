(ns quinto.grid
  (:require [clojure.spec.alpha :as s]
            [quinto.specs :as sp]))
; terminology:
; a cell is "empty" if its value is nil
; a cell is "filled" if it has a non-nil value
; a cell is "playable" if it is possible for a player to make a move on that cell
; a cell is "blocked" if it is not possible for a player to make a move on that cell
; several "filled" cells in a row are a "run" of filled cells


; There's some confusion as to what the canonical board size is. Picking 13x13 arbitrarily.
; See https://boardgamegeek.com/thread/24859/tile-distribution for some actual board sizes.
(def GRID-WIDTH 13)
(def GRID-HEIGHT 13)
(def empty-grid (vec (repeat GRID-WIDTH (vec (repeat GRID-HEIGHT nil)))))


(defn make-move [grid move]
  (reduce (fn [grid [[x y] value]]
            (assoc-in grid [x y] value))
          grid
          move))

(s/fdef make-move
  :args (s/cat :grid ::sp/grid :move ::sp/move)
  :ret nat-int?)

(defn find-empty-cells [grid]
  ; XXXX take a look at this fn, likely rewrite
  (filter identity
          (mapcat identity
                  (for [x (range (count grid))]
                    (for [y (range (count (grid x)))]
                      (when (nil? (get-in grid [x y]))
                        [x y]))))))

(s/fdef find-empty-cells
  :args (s/cat :grid ::sp/grid)
  :ret (s/coll-of ::sp/cell))

(defn cell-is-on-grid [grid [x y]]
  (and (>= x 0)
       (< x (count grid))
       (>= y 0)
       (< y (count (first grid)))))

(s/fdef cell-is-on-grid
  :args (s/cat :grid ::sp/grid :cell (s/tuple int? int?))
  :ret boolean?)

(defn cell-is-playable? [grid [x y]]
  ; a cell is playable if filling it will _not_ cause a streak of 6 or more filled cells to exist
  (let [length-of-run-in-direction (fn [xdir ydir]
                                     ; xxx there's gotta be a saner way to write this
                                     ; TODO will need to reuse this function when calculating
                                     ; the possible values that a cell can hold
                                     ; break it out into its defn and have it return
                                     ; a 2-tuple of [run-length run-sum]
                                     ; and always return runs in N E S W order
                                     (reduce (fn [cells-in-run steps-in-direction]
                                               (let [run-x (+ x (* xdir steps-in-direction))
                                                     run-y (+ y (* ydir steps-in-direction))]
                                                 (if (or (not (cell-is-on-grid grid [run-x run-y]))
                                                         (nil? (get-in grid [run-x run-y])))
                                                   (reduced cells-in-run)
                                                   (inc cells-in-run))))
                                             0
                                             (map inc (range))))]

    ; XXXXX needs to be >= 1
    ; make vars like horizontal-run-length, vertical-run-length
    ; also check if cell value is nil
    ; maybe this can't be reused by both playable and empty fns, maybe these three functions' relationship needs to be rethought
    (cond (> (+ (length-of-run-in-direction 1 0)
                (length-of-run-in-direction -1 0))
             ; TODO break this magic number out into its own well-named self-documenting var
             4)
          false
          (> (+ (length-of-run-in-direction 0 1)
                (length-of-run-in-direction 0 -1))
             4)
          false
          :else true)))

(s/fdef cell-is-playable?
  :args (s/cat :grid ::sp/grid :cell ::sp/cell)
  :ret boolean?)

(defn find-playable-cells [grid]
  (filter #(cell-is-playable? grid %) (find-empty-cells grid)))

(defn find-blocked-cells [grid]
  (filter #(not (cell-is-playable? grid %)) (find-empty-cells grid)))

; TODO a validate-grid function
