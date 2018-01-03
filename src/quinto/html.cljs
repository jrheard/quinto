(ns quinto.html
  (:require [com.rpl.specter :refer [select ALL LAST FIRST]]
            [cljs.core.async :refer [chan <! put!]]
            [reagent.core :as r]
            [quinto.grid :as g]
            [quinto.mode :as m]
            [quinto.utils :refer [remove-item]])
  (:require-macros [cljs.core.async :refer [go-loop]]))

;;; Helper functions

(defn can-go-back? [state]
  (not= (get-in state [:mode :mode/type]) :default))

(defn can-confirm-move? [state]
  (and (not= (get-in state [:mode :mode/type]) :default)
       (g/is-grid-valid? (state :grid))
       (> (count (get-in state [:mode :move-so-far]))
          0)))

(defn can-select-a-tile? [state]
  (some? (get-in state [:mode :selected-cell])))

;;; HTML rendering

(defn draw-cell [game-event-chan state grid x y playable-cells blocked-cells selected-cell]
  (let [cell (get-in grid [x y])
        mode (state :mode)
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when (contains? blocked-cells [x y])
                          "blocked ")
                        (when (contains? playable-cells [x y])
                          "playable ")
                        (when (contains? (set (select [ALL FIRST]
                                                      (state :most-recent-computer-move)))
                                         [x y])
                          "just-played ")
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
                  #(put! game-event-chan
                         {:event/type :select-cell
                          :cell       [x y]}))}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-grid [game-event-chan state grid playable-cells blocked-cells selected-cell]
  [:div#grid
   (for [x (range (count grid))]
     ^{:key x}
     [:div.column

      (for [y (range (count (grid x)))]
        ^{:key y}
        [draw-cell game-event-chan state grid x y playable-cells blocked-cells selected-cell])])])

(defn draw-tile [game-event-chan state value mode]
  [:div.tile
   {:class    (when-not (mode :selected-cell)
                "inactive")
    :on-click (when (can-select-a-tile? state)
                #(put! game-event-chan
                       {:event/type :select-tile
                        :value      value}))}
   value])

(defn draw-controls [state hand game-event-chan]
  (let [mode (state :mode)
        confirm-button-active (can-confirm-move? state)]
    [:div#controls
     {:class (when (mode :selected-cell)
               "assembling-move")}

     [:div#hand
      (for [[index value] (map-indexed vector hand)]
        ^{:key index} [draw-tile game-event-chan state value mode])]

     [:div.button.confirm
      {:class    (when (not confirm-button-active)
                   "inactive")
       :on-click #(when confirm-button-active
                    (put! game-event-chan {:event/type :confirm-move}))}
      "✔"]

     [:div.button.back
      {:class    (when-not (can-go-back? state)
                   "inactive ")
       :on-click #(when (can-go-back? state)
                    (put! game-event-chan {:event/type :go-back}))}
      "◀"]

     [:div.button.cancel
      {:class    (when (= (mode :mode/type) :default)
                   "inactive ")
       :on-click #(when (not= (mode :mode/type) :default)
                    (put! game-event-chan
                          {:event/type :cancel-mode}))}
      "✖"]]))

(defn draw-scores [scores mode whose-score]
  (let [tentative-score (mode :tentative-score)
        scores (if (and (not (seq scores))
                        (not tentative-score))
                 [0]
                 scores)]
    [:div.scores
     [:h3 whose-score]
     [:ul
      (for [[index value] (map-indexed vector scores)]
        ^{:key index} [:li
                       {:class (when (and (= index (dec (count scores)))
                                          (not= scores [0])
                                          (= whose-score "Computer"))
                                 "most-recent-score")}
                       value])

      (when (and tentative-score
                 (= whose-score "Player"))
        [:li.tentative-score
         (when (and (g/is-move-valid? (mode :original-grid)
                                      (mode :move-so-far))
                    (> tentative-score 0))
           {:class "valid"})
         tentative-score])]

     (when (> (count scores) 1)
       [:hr])
     (when (> (count scores) 1)
       [:p (apply + scores)])]))

(defn draw-game [state game-event-chan]
  (let [playable-cells (set
                         (if (= (get-in @state [:mode :mode/type]) :default)
                           (g/find-playable-cells (@state :grid))
                           (get-in @state [:mode :available-cells])))]

    [:div.game
     [draw-controls @state (@state :player-hand) game-event-chan]

     [:div.board-container
      [draw-scores (@state :player-scores) (@state :mode) "Player"]

      [draw-grid
       game-event-chan
       @state
       (@state :grid)
       playable-cells
       (set (g/find-blocked-cells (@state :grid)))
       (get-in @state [:mode :selected-cell])]

      [draw-scores (@state :ai-scores) (@state :mode) "Computer"]]]))

;;; Event handling

(defn handle-game-events [state game-event-chan]
  (go-loop []
    (let [event (<! game-event-chan)]
      ;(js/console.log event)
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
(def LEFT-ARROW-KEY-CODE 37)
(def ENTER-KEY-CODE 13)
(def NUMBER-KEY-CODES {49 1
                       50 2
                       51 3
                       52 4
                       53 5})

;;; Public API

(defn render-game [state]
  (when @keyup-handler
    (.removeEventListener js/document "keyup" @keyup-handler))

  (let [game-event-chan (chan)
        key-handler (fn [event]
                      (let [key-code (.-keyCode event)

                            event (condp contains? key-code
                                    #{ESCAPE-KEY-CODE} {:event/type :cancel-mode}

                                    #{LEFT-ARROW-KEY-CODE} (when (can-go-back? @state)
                                                             {:event/type :go-back})

                                    #{ENTER-KEY-CODE} (when (can-confirm-move? @state)
                                                        {:event/type :confirm-move})

                                    NUMBER-KEY-CODES (when (can-select-a-tile? @state)
                                                       (let [hand (@state :player-hand)
                                                             hand-index (dec (NUMBER-KEY-CODES key-code))]
                                                         (when (< hand-index (count hand))
                                                           {:event/type :select-tile
                                                            :value      (nth hand hand-index)})))
                                    nil)]

                        (when event
                          (put! game-event-chan event))))]

    (r/render-component [draw-game state game-event-chan]
                        (js/document.getElementById "app"))

    ; Back out of modes if the user hits the escape key.
    (.addEventListener js/document "keyup" key-handler)
    (reset! keyup-handler key-handler)

    (handle-game-events state game-event-chan)))
