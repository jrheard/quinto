(ns quinto.html
  (:require [com.rpl.specter :refer [select ALL LAST FIRST]]
            [cljs.core.async :refer [chan put!]]
            [reagent.core :as r]
            [quinto.grid :as g]
            [quinto.input :as i]
            [quinto.utils :refer [remove-item]]))

;;; HTML rendering

; xxxxx remove `state` arg, move stuff to cell-attributes
; xxxxx can remove grid arg too, in favor of `value`
(defn draw-cell [game-event-chan state grid x y cell-attributes]
  (let [cell (get-in grid [x y])
        cell-attributes (or cell-attributes #{})
        mode (state :mode)
        cell-class (str "cell "
                        (if (nil? cell)
                          "empty "
                          "full ")
                        (when (cell-attributes :blocked)
                          "blocked ")
                        (when (cell-attributes :playable)
                          "playable ")
                        (when (cell-attributes :historical-move-cell)
                          "historical-move-cell ")
                        (when (contains? (set (select [ALL FIRST]
                                                      (state :most-recent-computer-move)))
                                         [x y])
                          "just-played ")
                        (when (contains? (set (select [ALL FIRST]
                                                      (mode :move-so-far)))
                                         [x y])
                          "speculative ")
                        (when (cell-attributes :selected)
                          "selected "))]

    [:div
     {:class    cell-class
      :on-click (when (cell-attributes :playable)
                  #(put! game-event-chan
                         {:event/type :select-cell
                          :cell       [x y]}))}
     (if (nil? cell)
       ""
       cell)]))

(defn draw-grid [game-event-chan state grid cell-attributes-map]
  [:div#grid
   (for [x (range (count grid))]
     ^{:key x}
     [:div.column

      (for [y (range (count (grid x)))]
        ^{:key y}
        [draw-cell game-event-chan state grid x y (cell-attributes-map [x y])])])])

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

    (let [playable-cells (set
                           (if (= (get-in @state [:mode :mode/type]) :default)
                             (g/find-playable-cells (@state :grid))
                             (get-in @state [:mode :available-cells])))
          blocked-cells (set (g/find-blocked-cells (@state :grid)))]

      (merge-with into {}
                  (map #(vector % #{:playable}) playable-cells)
                  (map #(vector % #{:blocked}) blocked-cells)
                  {(get-in @state [:mode :selected-cell]) #{:selected}}))))

(defn draw-game
  "Top-level Reagent component. Draws the game."
  [state game-event-chan]
  [:div.game
   [draw-controls @state (@state :player-hand) game-event-chan]

   [:div.board-container
    [draw-scores (@state :player-scores) (@state :mode) "Player" game-event-chan]


    [draw-grid
     game-event-chan
     @state
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
                                                       LEFT-ARROW-KEY-CODE)
                                                (.preventDefault %)))
    (reset! keyup-handler-fn key-handler)

    (i/handle-game-events state game-event-chan)))
