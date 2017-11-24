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

(defn draw-column [column]
  [:div.column
   (for [index (range (count column))]
     ^{:key index} [draw-cell (column index)])])

(defn draw-grid [grid]
  [:div#grid
   (for [index (range (count grid))]
     ^{:key index} [draw-column (grid index)])])

(defn draw-game [state]
  [draw-grid (state :grid)])

