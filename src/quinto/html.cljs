(ns quinto.html
  (:require [reagent.core :as r]))

(defn draw-cell [grid x y blocked-cells]
  (let [cell (get-in grid [x y])
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when (and (contains? blocked-cells [x y])
                                   (nil? cell))
                          "blocked"))]

    [:div {:class cell-class}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-column [grid x blocked-cells]
  [:div.column
   (for [y (range (count (grid x)))]
     ^{:key y} [draw-cell grid x y blocked-cells])])

(defn draw-grid [grid blocked-cells]
  [:div#grid
   (for [x (range (count grid))]
     ^{:key x} [draw-column grid x blocked-cells])])

(defn draw-game [state blocked-cells]
  [draw-grid (state :grid) blocked-cells])

