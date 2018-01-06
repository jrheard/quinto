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

  (reset! app-state '{:grid [[nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil 8 1 8 8 nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil]], :deck (2 7 7 0 3 3 7 1 9 1 9 3 2 8 3 9 8 5 0 5 1 3 1 8 0 4 9 7 9 8 1 8 4 2 5 0 7 7 4 9 3 0 4 2 7 4 0 9 7 8 7 4 8 2 4 7 5 9 4 9 9 7 3 9 7 5), :player-scores [], :player-hand (4 7 0), :ai-scores [{:value 25, :move #{[[6 6] 8] [[6 7] 1] [[6 8] 8] [[6 9] 8]}, :grid [[nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil]]}], :ai-hand (4 8 8 5 2), :most-recent-computer-move [], :mode {:mode/type :viewing-historical-move, :move #{[[6 6] 8] [[6 7] 1] [[6 8] 8] [[6 9] 8]}, :optimal-move nil, :original-state {:grid [[nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil 7 9 nil nil nil nil nil] [nil nil nil nil nil nil 8 1 8 8 nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil]], :deck (2 7 7 0 3 3 7 1 9 1 9 3 2 8 3 9 8 5 0 5 1 3 1 8 0 4 9 7 9 8 1 8 4 2 5 0 7 7 4 9 3 0 4 2 7 4 0 9 7 8 7 4 8 2 4 7 5 9 4 9 9 7 3 9 7 5), :player-scores [], :player-hand (4 7 0), :ai-scores [{:value 25, :move #{[[6 6] 8] [[6 7] 1] [[6 8] 8] [[6 9] 8]}, :grid [[nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil]]}], :ai-hand (4 8 8 5 2), :most-recent-computer-move #{[[6 6] 8] [[6 7] 1] [[6 8] 8] [[6 9] 8]}, :mode {:mode/type :assembling-move, :selected-cell nil, :available-cells ([5 8] [5 5]), :move-so-far [[[5 6] 7] [[5 7] 9]], :tentative-score 41, :original-hand (4 9 7 7 0), :original-grid [[nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil 8 1 8 8 nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil] [nil nil nil nil nil nil nil nil nil nil nil nil nil]]}}}})

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
