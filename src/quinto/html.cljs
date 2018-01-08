(ns quinto.html
  "Draws the game on the screen and allows the user to interact with it."
  (:require [com.rpl.specter :refer [select ALL LAST FIRST]]
            [cljs.core.async :refer [chan put!]]
            [reagent.core :as r]
            [quinto.grid :as g]
            [quinto.input :as i]
            [quinto.mode :as m]
            [quinto.utils :refer [remove-item]]))

(defn draw-cell [game-event-chan x y value cell-attributes]
  (let [cell-attributes (or cell-attributes #{})
        cell-class (str "cell "
                        (if (nil? value)
                          "empty "
                          "full ")

                        (apply str (interleave (map name cell-attributes)
                                               (repeat " "))))]

    [:div
     {:class    cell-class
      :on-click #(when (cell-attributes :playable)
                   (put! game-event-chan
                         {:event/type :select-cell
                          :cell       [x y]}))}
     (if (nil? value)
       ""
       value)]))

(defn draw-grid [game-event-chan grid cell-attributes-map]
  [:div.grid
   (for [x (range (count grid))]
     ^{:key x}
     [:div.column

      (for [y (range (count (grid x)))]
        ^{:key y}
        [draw-cell game-event-chan x y (get-in grid [x y]) (cell-attributes-map [x y])])])])

(defn draw-ghost-grid [grid move optimal-move]
  ; If `move` _was_ an optimal move, then `optimal-move` will be nil.
  (let [relevant-cell-map (into {} (or optimal-move move))]

    [:div#ghost-grid
     (for [x (range (count grid))]
       ^{:key x}
       [:div.column

        (for [y (range (count (grid x)))]
          (let [cell (relevant-cell-map [x y])
                cell-class (if (contains? relevant-cell-map [x y])
                             (if optimal-move
                               ; If `optimal-move` is non-nil, then we're drawing both the
                               ; optimal move and the move that the player actually _made_.
                               ; Since the two might overlap in that situation, we give the
                               ; optimal move the "ghostly" class so that it can be a bit transparent.
                               "ghostly"
                               "optimal-historical-move")
                             "hidden")]
            ^{:key y} [:div.cell
                       {:class cell-class}
                       (if (nil? cell)
                         ""
                         cell)]))])]))

(defn draw-tile [game-event-chan state value mode]
  [:div.tile
   {:class    (when-not (mode :selected-cell)
                "inactive")
    :on-click #(when (m/can-select-a-tile? state)
                 (put! game-event-chan
                       {:event/type :select-tile
                        :value      value}))}
   value])

(defn draw-controls [state hand game-event-chan]
  (let [mode (state :mode)
        confirm-button-active (m/can-confirm-move? state)]
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
      {:class    (when-not (m/can-go-back? state)
                   "inactive ")
       :on-click #(when (m/can-go-back? state)
                    (put! game-event-chan {:event/type :go-back}))}
      "◀"]

     [:div.button.cancel
      {:class    (when (not= (mode :mode/type) :assembling-move)
                   "inactive ")
       :on-click #(when (= (mode :mode/type) :assembling-move)
                    (put! game-event-chan
                          {:event/type :cancel-mode}))}
      "✖"]]))

(defn draw-scores [scores mode whose-score game-event-chan]
  (let [tentative-score (mode :tentative-score)
        DUMMY-SCORE {:value 0}
        scores (if (and (not (seq scores))
                        (not tentative-score))
                 [DUMMY-SCORE]
                 scores)]
    [:div.scores
     {:on-mouse-leave #(put! game-event-chan {:event/type :stop-viewing-move})}
     [:h3 whose-score]
     [:ul
      (for [[index score] (map-indexed vector scores)]
        ^{:key index} [:li
                       {:class          (str (when (and (= index (dec (count scores)))
                                                        (not= scores [DUMMY-SCORE])
                                                        (= whose-score "Computer"))
                                               "most-recent-score ")
                                             (when (and (nil? (score :optimal-move))
                                                        (= whose-score "Player")
                                                        (not= score DUMMY-SCORE))
                                               "optimal "))
                        :on-mouse-enter #(when (not= score DUMMY-SCORE)
                                           (put! game-event-chan {:event/type   :view-move
                                                                  :grid         (score :grid)
                                                                  :move         (score :move)
                                                                  :optimal-move (if (= whose-score "Computer")
                                                                                  ; The computer's moves
                                                                                  ; are _always_ optimal. :)
                                                                                  nil
                                                                                  (score :optimal-move))}))}
                       (score :value)])

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
       [:p (apply + (map :value scores))])]))

(defn assemble-cell-attributes-map
  "Yields a map like {[0 5] #{:playable :selected}}."
  [state]
  (if (= (get-in @state [:mode :mode/type]) :viewing-historical-move)
    (into {} (map #(vector (first %) #{:historical-move-cell})
                  (get-in @state [:mode :move])))

    (let [playable-cells (if (@state :game-over)
                           []
                           (if (= (get-in @state [:mode :mode/type]) :default)
                             (g/find-playable-cells (@state :grid))
                             (get-in @state [:mode :available-cells])))
          blocked-cells (g/find-blocked-cells (@state :grid))
          recent-computer-move-cells (select [ALL FIRST] (@state :most-recent-computer-move))
          speculative-cells (select [ALL FIRST] (get-in @state [:mode :move-so-far]))]

      (merge-with into {}
                  (map #(vector % #{:playable}) playable-cells)
                  (map #(vector % #{:blocked}) blocked-cells)
                  (map #(vector % #{:just-played}) recent-computer-move-cells)
                  (map #(vector % #{:speculative}) speculative-cells)
                  {(get-in @state [:mode :selected-cell]) #{:selected}}))))

(defn draw-game-over
  [state game-event-chan]
  (let [player-score (apply + (map :value (state :player-scores)))
        ai-score (apply + (map :value (state :ai-scores)))
        result (cond
                 (> player-score ai-score) :player-won
                 (= player-score ai-score) :draw
                 (< player-score ai-score) :ai-won)]
    [:div.game-over
     {:class (name result)}
     [:div.message
      (condp = result
        :player-won "You win!"
        :draw "It's a draw!"
        :ai-won "The computer wins!")]

     [:div.new-game-button.button
      {:on-click #(put! game-event-chan {:event/type :new-game})}
      "New Game"]]))

(defn draw-game
  "Top-level Reagent component. Draws the game."
  [state game-event-chan]
  [:div.game
   [draw-controls @state (@state :player-hand) game-event-chan]
   (when (@state :game-over)
     [draw-game-over @state game-event-chan])

   [:div.board-container
    [draw-scores (@state :player-scores) (@state :mode) "Player" game-event-chan]


    [draw-grid
     game-event-chan
     (@state :grid)
     (assemble-cell-attributes-map state)]

    [draw-ghost-grid
     (@state :grid)
     (get-in @state [:mode :move])
     (get-in @state [:mode :optimal-move])]

    [draw-scores (@state :ai-scores) (@state :mode) "Computer" game-event-chan]]])

; Atom used for removing preexisting event handlers when fighweel reloads our code.
(defonce keyup-handler-fn (atom nil))

;;; Public API

(defn render-game [state]
  (when @keyup-handler-fn
    (.removeEventListener js/document "keyup" @keyup-handler-fn))

  (let [game-event-chan (chan)
        key-handler (i/make-key-handler state game-event-chan)]

    (r/render-component [draw-game state game-event-chan]
                        (js/document.getElementById "app"))

    (.addEventListener js/document "keyup" key-handler)

    ; Clare likes to play the game in a small window while watching Youtube or something.
    ; Prevent the left arrow key from scrolling the viewfinder to the left.
    (.addEventListener js/document "keydown" #(when (= (.-keyCode %)
                                                       i/LEFT-ARROW-KEY-CODE)
                                                (.preventDefault %)))
    (reset! keyup-handler-fn key-handler)

    (i/handle-game-events state game-event-chan)))
