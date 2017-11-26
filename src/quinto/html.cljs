(ns quinto.html
  (:require [reagent.core :as r]))

(defn draw-cell [grid x y playable-cells blocked-cells]
  (let [cell (get-in grid [x y])
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when (and (contains? blocked-cells [x y])
                                   ; XXXX this nil check shouldn't be necessary, move it into find-blocked-cells
                                   (nil? cell))
                          "blocked ")
                        (when (and (contains? playable-cells [x y])
                                   ; XXX same
                                   (nil? cell))
                          "playable "))]

    [:div {:class cell-class}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-column [grid x playable-cells blocked-cells]
  [:div.column
   (for [y (range (count (grid x)))]
     ^{:key y} [draw-cell grid x y playable-cells blocked-cells])])

(defn draw-grid [grid playable-cells blocked-cells]
  [:div#grid
   (for [x (range (count grid))]
     ^{:key x} [draw-column grid x playable-cells blocked-cells])])

(defn draw-game [state playable-cells blocked-cells]
  [draw-grid (state :grid) playable-cells blocked-cells])

