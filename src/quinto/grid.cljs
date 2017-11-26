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

; XXXX document
(def MAX-RUN-LENGTH 5)


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

(defn find-run [grid [x y] axis]
  (let [run-in-direction (fn [xdir ydir]
                           ; xxx there's gotta be a saner way to write this
                           (reduce (fn [[run-length run-sum] steps-in-direction]
                                     (let [run-x (+ x (* xdir steps-in-direction))
                                           run-y (+ y (* ydir steps-in-direction))]
                                       (if (or (not (cell-is-on-grid grid [run-x run-y]))
                                               (nil? (get-in grid [run-x run-y])))
                                         (reduced [run-length run-sum])
                                         [(inc run-length) (+ run-sum (get-in grid [run-x run-y]))])))
                                   [0 0]
                                   (map inc (range))))
        [first-half-length first-half-sum] (if (= axis :x)
                                             (run-in-direction -1 0)
                                             (run-in-direction 0 -1))
        [second-half-length second-half-sum] (if (= axis :x)
                                               (run-in-direction 1 0)
                                               (run-in-direction 0 1))]
    [(+ first-half-length second-half-length)
     (+ first-half-sum second-half-sum)]))

(s/fdef find-run
  :args (s/cat :grid ::sp/grid :cell ::sp/cell :axis #{:x :y})
  :ret ::sp/run)

(defn cell-is-playable? [grid [x y]]
  ; a cell is playable if at least one axis has a run length between 1 and 4 inclusive
  (let [[x-run-length _] (find-run grid [x y] :x)
        [y-run-length _] (find-run grid [x y] :y)]
    (and (or (> x-run-length 0) (> y-run-length 0))
         (and (< x-run-length MAX-RUN-LENGTH) (< y-run-length MAX-RUN-LENGTH)))))

(defn find-playable-cells [grid]
  (filter #(cell-is-playable? grid %) (find-empty-cells grid)))

(defn cell-is-blocked? [grid [x y]]
  ; a cell is blocked if either axis has a run length of 5 or more
  (let [[x-run-length _] (find-run grid [x y] :x)
        [y-run-length _] (find-run grid [x y] :y)]
    ; XXXXX double-check to make sure there isn't an off-by-one error here
    (or (>= x-run-length MAX-RUN-LENGTH) (>= y-run-length MAX-RUN-LENGTH))))

(defn find-blocked-cells [grid]
  (filter #(cell-is-blocked? grid %) (find-empty-cells grid)))

; TODO a validate-grid function
