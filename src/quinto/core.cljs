(ns quinto.core
  (:require [com.rpl.specter :refer [select ALL srange nthpath multi-path STOP collect-one selected? FIRST LAST INDEXED-VALS]]
            [cljs.core.async :refer [chan <! put! close! go >!]]
            [reagent.core :as r]
            [orchestra-cljs.spec.test :as st]
            [quinto.deck :refer [make-deck draw-tiles MAX-HAND-SIZE]]
            [quinto.html :refer [render-game can-confirm-move? can-select-a-tile?]]
            [quinto.mode :refer [make-ai-move confirm-move]]
            [quinto.specter :refer [grid-values indexed-grid-values]]
            [quinto.ai :as ai]
            [quinto.grid :as g]))

(defonce app-state
         (r/atom {:grid                      g/empty-grid
                  :deck                      (make-deck)
                  :player-scores             []
                  :player-hand               []
                  :ai-scores                 []
                  :ai-hand                   []
                  :most-recent-computer-move []
                  :mode                      {:mode/type :default}}))

(defonce game-event-chan (chan))

(defn ^:export main []
  ;(st/instrument)
  (let [[new-deck new-hand] (draw-tiles (@app-state :deck) (@app-state :player-hand) MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :player-hand new-hand))

  (let [[new-deck new-hand] (draw-tiles (@app-state :deck) (@app-state :ai-hand) MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :ai-hand new-hand))

  (when (= (rand-nth [:heads :tails]) :tails)
    (swap! app-state make-ai-move))

  (render-game app-state game-event-chan))

(defn on-js-reload []
  (render-game app-state game-event-chan))

; https://stackoverflow.com/questions/14464011/idiomatic-clojure-for-picking-between-random-weighted-choices
(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))

(defn generate-next-game-event
  [state]
  (let [mode (get-in state [:mode :mode/type])]
    (cond
      (= mode :default)
      {:event/type :select-cell
       :cell       (rand-nth (g/find-playable-cells (state :grid)))}

      (= mode :assembling-move)
      (let [options-map {{:event/type :go-back}     2
                         {:event/type :cancel-mode} 2}

            options-map (if (seq (get-in state [:mode :available-cells]))
                          (assoc options-map
                                 {:event/type :select-cell
                                  :cell       (rand-nth (get-in state [:mode :available-cells]))}
                                 5)
                          options-map)
            options-map (if (and (can-select-a-tile? state)
                                 (seq (state :player-hand)))
                          (assoc options-map
                                 {:event/type :select-tile
                                  :value      (rand-nth (state :player-hand))}
                                 10)
                          options-map)

            options-map (if (can-confirm-move? state)
                          (assoc options-map
                                 {:event/type :confirm-move}
                                 10)
                          options-map)]

        (weighted-rand-choice options-map)))))

(defn timeout [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(comment

  (go
    (dotimes [i 100]
      (<! (timeout 50))
      (let [event (generate-next-game-event @app-state)]
        (>! game-event-chan event))))


  (identity @app-state)
  (@app-state :grid)
  (g/is-grid-valid? (@app-state :grid))

  (pr-str @app-state)

  (subvec '(1 2 3 4) 0 3)

  (nth '(1 2 3) 4)

  (select [ALL ALL some?]
          (@app-state :grid))

  (let [good-move (ai/pick-move (@app-state :grid) (@app-state :player-hand))]
    (swap! app-state assoc-in [:mode :move-so-far] good-move)
    (swap! app-state confirm-move)
    )

  (js/console.log nil)


  (let [good-move (ai/pick-move (@app-state :grid) (@app-state :player-hand))]
    (js/console.log good-move)
    (swap! app-state assoc-in [:mode :move-so-far] good-move)
    (swap! app-state update :player-hand #(drop-last (count good-move) %))
    (swap! app-state assoc-in [:mode :original-hand] (@app-state :grid))
    (swap! app-state assoc-in [:mode :original-grid] (@app-state :player-hand))
    (swap! app-state confirm-move)
    )

  (select [(grid-values 0 0 1 0)]
          [])
  )
