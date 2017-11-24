(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.html :refer [draw-game]]))

; terminology to spec
; a cell is an [x y] pair
; a move is a [cell value] pair
; a value is a number between 0 and 9 or nil
; a cell is "empty" if its value is nil
; a cell is "open" if it is possible for a player to make a move on that cell
; a cell is "filled" if it has a non-nil value


(def BOARD-SIZE 11)
(def empty-grid (vec (repeat BOARD-SIZE (vec (repeat BOARD-SIZE nil)))))

(defonce app-state
         (r/atom {:grid empty-grid}))

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

(defn cell-is-open? [grid cell]
  ; TODO a cell is open if filling it will _not_ cause a streak of 6 or more filled cells to exist
  )

(defn find-open-cells [grid]
  (filter cell-is-open? (find-empty-cells grid)))



(defn ^:export main []
  (swap! app-state update-in [:grid] make-move [[[5 5] 0]
                                                [[5 4] 9]
                                                [[5 3] 1]
                                                [[5 2] 5]
                                                [[5 1] 5]])

  (r/render-component [draw-game @app-state]
                      (js/document.getElementById "app")))

(defn on-js-reload []
  (main))

(comment
  (take 5 (find-empty-cells (@app-state :grid)))

  )
