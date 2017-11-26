(ns quinto.grid)
; terminology to spec
; a cell is an [x y] pair
; a move is a [cell value] pair
; a value is a number between 0 and 9 or nil
; a cell is "empty" if its value is nil
; a cell is "filled" if it has a non-nil value
; a cell is "playable" if it is possible for a player to make a move on that cell
; a cell is "blocked" if it is not possible for a player to make a move on that cell
; several "filled" cells in a row are a "run" of filled cells


; XX rename grid-size
; note: should be an odd number!
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

(defn cell-is-on-grid [grid [x y]]
  (and (>= x 0)
       (< x (count grid))
       (>= y 0)
       (< y (count (first grid)))))


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

(defn find-playable-cells [grid]
  (filter #(cell-is-playable? grid %) (find-empty-cells grid)))

(defn find-blocked-cells [grid]
  (filter #(not (cell-is-playable? grid %)) (find-empty-cells grid)))

; TODO a validate-grid function? perhaps unnecessary but likely very useful
