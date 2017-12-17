(ns quinto.core
  (:require [com.rpl.specter :refer [select ALL srange nthpath multi-path STOP]]
            [orchestra-cljs.spec.test :as stest]
            [reagent.core :as r]
            [quinto.deck :refer [make-deck draw-tiles MAX-HAND-SIZE]]
            [quinto.html :refer [draw-game]]
            [quinto.grid :as g]
            [quinto.specs :refer [GRID-WIDTH GRID-HEIGHT MAX-RUN-LENGTH]]))

(defonce app-state
         (r/atom {:grid g/empty-grid
                  :deck (make-deck)
                  :hand []
                  :mode {:mode/type :default}}))

(defn render-game []
  (assert (g/is-grid-valid? (@app-state :grid)))

  (r/render-component [draw-game app-state]
                      (js/document.getElementById "app")))

(defn enter-assembling-move-mode! [selected-cell]
  ; XXX assert selected-cell is a playable cell
  (swap! app-state assoc :mode
         {:mode/type       :assembling-move
          :selected-cell   selected-cell
          :available-cells []
          :move-so-far     []}))

(defn ^:export main []
  ;(stest/instrument)

  (let [[new-deck new-hand] (draw-tiles (@app-state :deck)
                                        (@app-state :hand)
                                        MAX-HAND-SIZE)]
    (swap! app-state assoc :deck new-deck)
    (swap! app-state assoc :hand new-hand))

  #_(enter-assembling-move-mode! [5 5])

  (swap! app-state update-in [:grid] g/make-move [[[6 4] 7]
                                                  [[6 5] 3]
                                                  [[6 6] 5]
                                                  [[6 7] 4]
                                                  [[6 8] 1]])

  (swap! app-state update-in [:grid] g/make-move [[[5 4] 5]
                                                  [[4 4] 3]
                                                  [[3 4] 2]
                                                  [[2 4] 8]])

  (render-game))

(def on-js-reload render-game)

(comment
  (@app-state :mode)
  )
