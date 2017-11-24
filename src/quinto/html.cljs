(ns quinto.html
  (:require [reagent.core :as r]))

(defn draw-cell [cell]
  (let [cell-class (str "cell "
                        (if (nil? cell)
                          "empty"
                          "full"))]

    [:div {:class cell-class}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-row [row]
  [:div.row
   (for [index (range (count row))]
     ^{:key index} [draw-cell (row index)])])

(defn draw-grid [grid]
  [:div#grid
   (for [index (range (count grid))]
     ^{:key index} [draw-row (grid index)])])

(defn draw-game [state]
  [draw-grid (state :grid)])

