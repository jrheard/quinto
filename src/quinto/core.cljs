(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.ai :as ai]
            [quinto.html :refer [render-game]]
            [quinto.mode :as m]))

(defonce app-state (r/atom (m/fresh-game-state)))

(defn ^:export main []
  (render-game app-state))

(defn on-js-reload []
  (render-game app-state))

(comment
  (swap! app-state m/maybe-end-game)

  (identity @app-state)

  (@app-state :game-over)

  (ai/pick-move (@app-state :grid) (@app-state :player-hand))
  (swap! app-state assoc :player-hand [5])
  (swap! app-state update :player-scores conj {:value 100}))
