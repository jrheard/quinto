(ns quinto.html
  (:require [com.rpl.specter :refer [select ALL LAST]]
            [cljs.core.async :refer [chan <! put!]]
            [reagent.core :as r]
            [quinto.ai :as ai]
            [quinto.deck :as deck]
            [quinto.grid :as g]
            [quinto.mode :as m]
            [quinto.utils :refer [remove-item]])
  (:require-macros [cljs.core.async :refer [go-loop]]))

(defn draw-cell [game-event-chan mode grid x y playable-cells blocked-cells selected-cell]
  (let [cell (get-in grid [x y])
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when (contains? blocked-cells [x y])
                          "blocked ")
                        (when (contains? playable-cells [x y])
                          "playable ")
                        (when (= selected-cell [x y])
                          "selected "))]

    ; xxxxx if we're in assembling mode and this cell is playable, put an event in the channel

    [:div
     {:class    cell-class
      :on-click (when (and (contains? playable-cells [x y])
                           (nil? (mode :selected-cell)))
                  #(do
                     (put! game-event-chan
                           {:event/type :select-cell
                            :cell       [x y]})
                     nil))}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-grid [game-event-chan mode grid playable-cells blocked-cells selected-cell]
  [:div#grid
   (for [x (range (count grid))]
     ^{:key x}
     [:div.column

      (for [y (range (count (grid x)))]
        ^{:key y}
        [draw-cell game-event-chan mode grid x y playable-cells blocked-cells selected-cell])])])

(defn draw-tile [game-event-chan value mode]
  [:div.tile
   {:on-click (when (= (mode :mode/type) :assembling-move)
                #(do
                   (put! game-event-chan value)
                   nil))}
   value])

(defn draw-controls [state hand game-event-chan]
  (let [mode (@state :mode)]
    [:div#controls
     {:class (when (= (mode :mode/type) :assembling-move)
               "assembling-move")}

     [:div#hand
      (for [[index value] (map-indexed vector hand)]
        ^{:key index} [draw-tile game-event-chan value mode])]

     [:div.button
      {:on-click #(do
                    (swap! state
                           (fn [state]
                             (let [move (ai/pick-move (state :grid) (state :hand))
                                   move-tiles (select [ALL LAST] move)
                                   spent-hand (reduce remove-item (state :hand) move-tiles)
                                   [new-deck new-hand] (deck/draw-tiles (state :deck)
                                                                        spent-hand
                                                                        (count move-tiles))]
                               (-> state
                                   (assoc :grid (g/make-move (state :grid) move))
                                   (assoc :hand new-hand)
                                   (assoc :deck new-deck)))))
                    nil)}
      "make a move"]]))

(defn draw-game [state game-event-chan]
  (let [playable-cells (if (= (get-in @state [:mode :mode/type]) :default)
                         (set (g/find-playable-cells (@state :grid)))
                         (get-in @state [:mode :available-cells]))]

    [:div.game
     [draw-controls state (@state :hand) game-event-chan]

     [draw-grid
      game-event-chan
      (@state :mode)
      (@state :grid)
      playable-cells
      (set (g/find-blocked-cells (@state :grid)))
      (get-in @state [:mode :selected-cell])]]))

; TODO if the user presses escape while we're in assembling-move mode, back out and go to default

(defn handle-game-events [state game-event-chan]
  (go-loop []
    (let [event (<! game-event-chan)]
      (js/console.log event)
      (condp = (event :event/type)
        :select-cell (swap! state m/enter-assembling-move-mode (event :cell))
        :cancel-mode (swap! state m/cancel-mode)
        nil))
    (recur)))

(defn render-game [state]
  (assert (g/is-grid-valid? (@state :grid)))

  (let [game-event-chan (chan)]
    (r/render-component [draw-game state game-event-chan]
                        (js/document.getElementById "app"))

    ; Back out of modes if the user hits the escape key.
    (.addEventListener js/document
                       "keyup"
                       (fn [event]
                         (let [key-code (.-keyCode event)]
                           (if (= key-code 27) (put! game-event-chan
                                                     {:event/type :cancel-mode})))))

    (handle-game-events state game-event-chan)))
