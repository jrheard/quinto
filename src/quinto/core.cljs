(ns quinto.core
  (:require [com.rpl.specter :refer [select ALL srange nthpath multi-path STOP collect-one selected? FIRST LAST]]
            [orchestra-cljs.spec.test :as stest]
            [reagent.core :as r]
            [quinto.deck :refer [make-deck draw-tiles MAX-HAND-SIZE]]
            [quinto.html :refer [render-game]]
            [quinto.specter :refer [grid-values indexed-grid-values]]
            [quinto.grid :as g]))

(defonce app-state
         (r/atom {:grid          g/empty-grid
                  :deck          (make-deck)
                  :player-scores []
                  :player-hand   []
                  :ai-scores     []
                  :ai-hand       []
                  :mode          {:mode/type                 :default
                                  :most-recent-computer-move []}}))

(defn ^:export main []
  ;(stest/instrument)

  ; xxxx function in deck
  (let [[new-deck new-hand] (draw-tiles (@app-state :deck)
                                        (@app-state :player-hand)
                                        MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :player-hand new-hand))

  (let [[new-deck new-hand] (draw-tiles (@app-state :deck)
                                        (@app-state :ai-hand)
                                        MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :ai-hand new-hand))

  (swap! app-state update-in [:grid] g/make-move [[[6 4] 7]
                                                  [[6 5] 3]
                                                  [[6 6] 5]
                                                  [[6 7] 4]
                                                  [[6 8] 1]])

  (swap! app-state update-in [:grid] g/make-move [[[5 4] 5]
                                                  [[4 4] 3]
                                                  [[3 4] 2]
                                                  [[2 4] 8]])

  (render-game app-state))

(defn on-js-reload []
  (render-game app-state))

(comment
  (identity @app-state)
  (@app-state :deck)
  (g/is-grid-valid?
    (get-in @app-state [:grid]))

  (g/score-move
    (@app-state :grid)
    [[[6 10] 0] [[5 10] 5] [[4 10] 1]]
    )

  (set (select [ALL FIRST]
                ((@app-state :mode) :most-recent-computer-move)))

  )
