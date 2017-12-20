(ns quinto.html
  (:require [com.rpl.specter :refer [select ALL LAST FIRST]]
            [cljs.core.async :refer [chan <! put!]]
            [reagent.core :as r]
            [quinto.ai :as ai]
            [quinto.deck :as deck]
            [quinto.grid :as g]
            [quinto.mode :as m]
            [quinto.utils :refer [remove-item]])
  (:require-macros [cljs.core.async :refer [go-loop]]))

;;; HTML rendering

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
                        (when (contains? (set (select [ALL FIRST]
                                                      (mode :move-so-far)))
                                         [x y])
                          "speculative ")
                        (when (= selected-cell [x y])
                          "selected "))]

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
   {:class    (when-not (mode :selected-cell)
                "inactive")
    :on-click (when (mode :selected-cell)
                #(do
                   (put! game-event-chan
                         {:event/type :select-tile
                          :value      value})
                   nil))}
   value])

#_[:div.button
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
   "make a move"]

(defn draw-controls [state hand game-event-chan]
  (let [mode (@state :mode)
        confirm-button-active (and (not= (mode :mode/type) :default)
                                   (g/is-grid-valid? (@state :grid))
                                   (> (count (mode :move-so-far))
                                      0))]
    [:div#controls
     {:class (when (mode :selected-cell)
               "assembling-move")}

     [:div#hand
      (for [[index value] (map-indexed vector hand)]
        ^{:key index} [draw-tile game-event-chan value mode])]

     [:div.button.confirm
      {:class    (when (not confirm-button-active)
                   "inactive")
       :on-click #(do
                    (when confirm-button-active
                      (put! game-event-chan {:event/type :confirm-move}))
                    nil)}
      "✔"]

     [:div.button.back
      {:class    (when (= (mode :mode/type) :default)
                   "inactive ")
       :on-click #(do
                    (when (not= (mode :mode/type) :default)
                      (put! game-event-chan {:event/type :go-back}))
                    nil)}
      "◀"]

     [:div.button.cancel
      {:class    (when (= (mode :mode/type) :default)
                   "inactive ")
       :on-click #(do
                    (when (not= (mode :mode/type) :default)
                      (put! game-event-chan
                            {:event/type :cancel-mode}))
                    nil)}
      "✖"]]))

(defn draw-scores [scores whose-score]
  [:div.scores
   [:p whose-score]
   [:ul
    (for [[index value] (map-indexed vector scores)]
      ^{:key index} [:li value])]])

(defn draw-game [state game-event-chan]
  (let [playable-cells (set
                         (if (= (get-in @state [:mode :mode/type]) :default)
                           (g/find-playable-cells (@state :grid))
                           (get-in @state [:mode :available-cells])))]

    [:div.game
     [draw-controls state (@state :hand) game-event-chan]

     [:div.board-container
      [draw-scores (@state :player-scores) "Player"]

      [draw-grid
       game-event-chan
       (@state :mode)
       (@state :grid)
       playable-cells
       (set (g/find-blocked-cells (@state :grid)))
       (get-in @state [:mode :selected-cell])]

      [draw-scores (@state :player-scores) "Computer"]]]))

;;; Event handling

(defn handle-game-events [state game-event-chan]
  (go-loop []
    (let [event (<! game-event-chan)]
      (js/console.log event)
      (condp = (event :event/type)
        :select-cell (if (= (get-in @state [:mode :mode/type])
                            :default)
                       (swap! state m/enter-assembling-move-mode (event :cell))
                       (swap! state m/select-cell (event :cell)))
        :select-tile (swap! state m/select-tile (event :value))
        :confirm-move (swap! state m/confirm-move)
        :go-back (swap! state m/go-back)
        :cancel-mode (swap! state m/cancel-mode)
        nil))
    (recur)))

; Atom used for removing preexisting event handlers when fighweel reloads our code.
(defonce keyup-handler (atom nil))
(def ESCAPE-KEY-CODE 27)

;;; Public API

(defn render-game [state]
  (when @keyup-handler
    (.removeEventListener js/document "keyup" @keyup-handler))

  (let [game-event-chan (chan)
        escape-handler (fn [event]
                         (let [key-code (.-keyCode event)]
                           (when (= key-code ESCAPE-KEY-CODE)
                             (put! game-event-chan
                                   {:event/type :cancel-mode}))))]

    (r/render-component [draw-game state game-event-chan]
                        (js/document.getElementById "app"))

    ; Back out of modes if the user hits the escape key.
    (.addEventListener js/document "keyup" escape-handler)
    (reset! keyup-handler escape-handler)

    (handle-game-events state game-event-chan)))
