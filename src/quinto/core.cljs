(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.html :refer [render-game]]
            [quinto.mode :as m]))

(defonce app-state (r/atom (m/fresh-game-state)))

(defn ^:export main []
  (render-game app-state))

(defn on-js-reload []
  (render-game app-state))

(comment
  (swap! app-state m/end-game-if-player-hand-empty)

  (identity @app-state)
  )
