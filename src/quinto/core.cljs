(ns quinto.core
  (:require [clojure.spec.test.alpha :as stest]
            [reagent.core :as r]
            [quinto.html :refer [draw-game]]
            [quinto.grid :as g]))

; next up:
; grid valiation fn
; start work on ai

(defonce app-state
         (r/atom {:grid g/empty-grid}))

(defn ^:export main []
  (stest/instrument)
  (swap! app-state update-in [:grid] g/make-move [[[6 6] 0]
                                                  [[6 5] 9]
                                                  [[6 4] 1]
                                                  [[6 3] 5]
                                                  [[6 2] 5]])
  (swap! app-state update-in [:grid] g/make-move [[[2 6] 5]
                                                  [[3 6] 9]
                                                  [[4 6] 1]
                                                  [[5 6] 5]])
  (r/render-component [draw-game
                       @app-state
                       (set (g/find-playable-cells (@app-state :grid)))
                       (set (g/find-blocked-cells (@app-state :grid)))]
                      (js/document.getElementById "app")))

(def on-js-reload main)

(comment
  (find-open-cells (@app-state :grid))
  (contains? (set (find-open-cells (@app-state :grid))) [1 1])
  )
