(ns quinto.core
  (:require [reagent.core :as r])
  )

(def BOARD-SIZE 10)

(defonce app-state
         (atom {:grid (vec (repeat BOARD-SIZE (vec (repeat BOARD-SIZE nil))))}))

(defn draw-cell [cell]
  [:div.cell (if (nil? cell)
               ""
               cell)])

(defn draw-row [row]
  [:div.row
   (for [index (range (count row))]
     ^{:key index} [draw-cell (row index)])])

(defn draw-grid [grid]
  [:div#grid
   (for [index (range (count grid))]
     ^{:key index} [draw-row (grid index)])])

(defn draw-game [state]
  [draw-grid (state :grid)]

  )

(defn ^:export main []
  (r/render-component [draw-game @app-state]
                      (js/document.getElementById "app")))



(defn on-js-reload []
  (main))

(comment
  (map-indexed identity (@app-state :grid))

  )
