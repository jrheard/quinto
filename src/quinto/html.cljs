(ns quinto.html
  (:require [com.rpl.specter :refer [select ALL]]
            [quinto.ai :as ai]
            [quinto.deck :as deck]
            [quinto.grid :as g]
            [quinto.utils :refer [remove-item]]))

(defn draw-cell [grid x y playable-cells blocked-cells]
  (let [cell (get-in grid [x y])
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when (contains? blocked-cells [x y])
                          "blocked ")
                        (when (contains? playable-cells [x y])
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

(defn draw-controls [state hand]
  [:div#controls
   [:div#hand
    (for [[index value] (map-indexed vector hand)]
      ^{:key index} [:div.tile value])]
   [:div.button
    ; TODO make this saner
    {:on-click #(do
                  (swap! state
                         (fn [state]
                           (let [move (ai/pick-move (state :grid) (state :hand))
                                 move-tiles (select [ALL 1] move)
                                 spent-hand (reduce remove-item (state :hand) move-tiles)
                                 [new-deck new-hand] (deck/draw-tiles (state :deck)
                                                                      spent-hand
                                                                      (count move-tiles))]
                             (-> state
                                 (assoc :grid (g/make-move (state :grid) move))
                                 (assoc :hand new-hand)
                                 (assoc :deck new-deck)))))
                  nil)}
    "make a move"]])

(defn draw-game [state]
  [:div.game
   [draw-controls state (@state :hand)]
   [draw-grid
    (@state :grid)
    (set (g/find-playable-cells (@state :grid)))
    (set (g/find-blocked-cells (@state :grid)))]])

