(ns quinto.core
  (:require [com.rpl.specter :refer [select ALL srange nthpath multi-path STOP collect-one selected? FIRST LAST INDEXED-VALS]]
            [reagent.core :as r]
            [quinto.deck :refer [make-deck draw-tiles MAX-HAND-SIZE]]
            [quinto.html :refer [render-game]]
            [quinto.mode :refer [make-ai-move]]
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

(defn ^:export main []
  (let [[new-deck new-hand] (draw-tiles (@app-state :deck) (@app-state :player-hand) MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :player-hand new-hand))

  (let [[new-deck new-hand] (draw-tiles (@app-state :deck) (@app-state :ai-hand) MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :ai-hand new-hand))

  (when (= (rand-nth [:heads :tails]) :tails)
    (swap! app-state make-ai-move))

  (render-game app-state))

(defn on-js-reload []
  (render-game app-state))

(comment
  (identity @app-state)
  (@app-state :grid)
  (g/is-grid-valid? (@app-state :grid))

  (pr-str @app-state)

  (subvec '(1 2 3 4) 0 3)

  (nth '(1 2 3) 4)

  (select [ALL ALL some?]
          (@app-state :grid))


  (ai/pick-move (@app-state :grid) (@app-state :player-hand))
  )
