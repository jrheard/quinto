(ns quinto.html
  ; XXXX REARRANGE CALLS INTO THIS MODULE SO THAT HTML NO LONGER IMPORTS GRID
  (:require [quinto.grid :as g]
            [reagent.core :as r]))

(defn draw-cell [grid x y open-cells]
  (let [cell (get-in grid [x y])
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when-not (contains? (set open-cells) [x y])
                          "blocked"))]

    [:div {:class cell-class}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-column [grid x open-cells]
  [:div.column
   (for [y (range (count (grid x)))]
     ^{:key y} [draw-cell grid x y open-cells])])

(defn draw-grid [grid]
  (let [open-cells (g/find-open-cells grid)]
    [:div#grid
     (for [x (range (count grid))]
       ^{:key x} [draw-column grid x open-cells])]))

(defn draw-game [state]
  [draw-grid (state :grid)])

