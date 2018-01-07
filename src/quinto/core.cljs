(ns quinto.core
  (:require [com.rpl.specter :refer [select ALL srange nthpath multi-path STOP collect-one selected? FIRST LAST INDEXED-VALS]]
            [cljs.core.async :refer [chan <! put! close! go >!]]
            [reagent.core :as r]
            [orchestra-cljs.spec.test :as st]
            [quinto.deck :refer [make-deck draw-tiles MAX-HAND-SIZE]]
            [quinto.html :refer [render-game play-game]]
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

(comment

  (play-game app-state game-event-chan 100)




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
