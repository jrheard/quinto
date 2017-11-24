(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.html :refer [draw-game]]))

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
  )

; XXX change to find-valid-moves
; make (is-valid-move? grid x y) function
(defn find-invalid-moves [grid]
  ; an invalid move is one that would cause a series
  ; of more than 5 tiles in a row to be connected.




  ; TODO first find only the empty cells, then check those


  )



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

  )
