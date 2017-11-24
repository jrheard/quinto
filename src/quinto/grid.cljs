(ns quinto.grid)
; terminology to spec
; a cell is an [x y] pair
; a move is a [cell value] pair
; a value is a number between 0 and 9 or nil
; a cell is "empty" if its value is nil
; a cell is "open" if it is possible for a player to make a move on that cell
; a cell is "blocked" if it is not possible for a player to make a move on that cell
; a cell is "filled" if it has a non-nil value
; several "filled" cells in a row are a "run" of filled cells


(def BOARD-SIZE 11)
(def empty-grid (vec (repeat BOARD-SIZE (vec (repeat BOARD-SIZE nil)))))


(defn make-move [grid move]
  ; a move is a list of [[x y] value] lists indicating where tiles should be placed.
  (reduce (fn [grid [[x y] value]]
            (assoc-in grid [x y] value))
          grid
          move))


(defn find-empty-cells [grid]
  ; return a list of [x y] values
  (mapcat identity
          (for [x (range (count grid))]
            (for [y (range (count (grid x)))]
              (when (nil? (get-in grid [x y]))
                [x y])))))

(defn cell-is-off-grid [grid [x y]]
  (and (>= x 0)
       (< x (count grid))
       (>= y 0)
       (< y (count (first grid)))))


(defn cell-is-open? [grid [x y]]
  ; a cell is open if filling it will _not_ cause a streak of 6 or more filled cells to exist
  (let [length-of-run-in-direction (fn [xdir ydir]
                                     ; xxx there's gotta be a saner way to write this
                                     (reduce (fn [cells-in-run steps-in-direction]
                                               (let [run-x (+ x (* xdir (inc steps-in-direction)))
                                                     run-y (+ y (* ydir (inc steps-in-direction)))]
                                                 (if (or (cell-is-off-grid grid [run-x run-y])
                                                         (nil? (get-in grid [run-x run-y])))
                                                   (reduced cells-in-run)
                                                   (inc cells-in-run))))
                                             0
                                             (range)))]

    (when (= [x y] [0 5])
      (js/console.log "HI")
      (js/console.log (length-of-run-in-direction 0 1) (length-of-run-in-direction 0 -1))
      )
    (cond (> (+ (length-of-run-in-direction 1 0)
                (length-of-run-in-direction -1 0))
             4)
          false
          (> (+ (length-of-run-in-direction 0 1)
                (length-of-run-in-direction 0 -1))
             4)
          false
          :else true)))

(defn find-open-cells [grid]
  (filter #(cell-is-open? grid %) (find-empty-cells grid)))
