(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.html :refer [draw-game]]))

(def BOARD-SIZE 11)
(def empty-grid (vec (repeat BOARD-SIZE (vec (repeat BOARD-SIZE nil)))))

(defonce app-state
         (r/atom {:grid empty-grid}))

(defn make-move [grid move]
  (reduce (fn [grid [[x y] value]]
            (assoc-in grid [x y] value))
          grid
          move))

(swap! app-state update-in [:grid] make-move [[[5 5] 0]
                                              [[5 4] 9]
                                              [[5 3] 1]])


(defn ^:export main []
  (r/render-component [draw-game @app-state]
                      (js/document.getElementById "app")))

(defn on-js-reload []
  (main))

(comment

  )
