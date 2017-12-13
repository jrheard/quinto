(ns quinto.core
  (:require [orchestra-cljs.spec.test :as stest]
            [reagent.core :as r]
            [quinto.ai :as ai]
            [quinto.deck :refer [make-deck draw-tiles]]
            [quinto.html :refer [draw-game]]
            [quinto.grid :as g]))

(defonce app-state
         (r/atom {:grid g/empty-grid
                  :deck (make-deck)
                  :hand []}))

(defn render-game []
  (assert (g/is-grid-valid? (@app-state :grid)))

  (r/render-component [draw-game
                       @app-state
                       (set (g/find-playable-cells (@app-state :grid)))
                       (set (g/find-blocked-cells (@app-state :grid)))]
                      (js/document.getElementById "app")))

(defn ^:export main []
  (stest/instrument)

  (let [[new-deck new-hand] (draw-tiles (@app-state :deck) (@app-state :hand) 5)]
    (swap! app-state assoc :deck new-deck)
    #_(swap! app-state assoc :hand new-hand)
    (swap! app-state assoc :hand [4 5 1]))

  (swap! app-state update-in [:grid] g/make-move [[[6 6] 5]
                                                  [[6 7] 4]
                                                  [[6 8] 1]
                                                  ])

  #_(swap! app-state update-in [:grid] g/make-move [[[6 6] 0]
                                                    [[6 5] 9]
                                                    [[6 4] 1]
                                                    [[6 3] 5]
                                                    [[6 2] 5]])
  #_(swap! app-state update-in [:grid] g/make-move [[[2 6] 5]
                                                    [[3 6] 9]
                                                    [[4 6] 1]
                                                    [[5 6] 5]])
  (render-game)

  #_(ai/pick-move
      (@app-state :grid)
      (@app-state :hand)
      )
  )

(def on-js-reload render-game)

(comment
  (g/is-grid-valid? (@app-state :grid))
  (g/find-playable-cells (@app-state :grid))
  (contains? (set (g/find-playable-cells (@app-state :grid))) [1 1])
  (@app-state :grid)
  (@app-state :hand)
  (count (@app-state :deck))

  (ai/pick-move
    (@app-state :grid)
    (@app-state :hand))

  )


