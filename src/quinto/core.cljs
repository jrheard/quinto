(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.ai :as ai]
            [quinto.html :refer [render-game draw-grid]]
            [quinto.mode :as m]))

(defonce app-state (r/atom (m/fresh-game-state)))

(def grid-1 [[7] [8] [5]])

(def grid-2 [[7] [7] [3]])

(def grid-3 [[5 nil nil]
             [3 nil nil]
             [2 nil nil]])

(def grid-4 [[5 nil nil]
             [3 nil nil]
             [2 3 5]])
(def cell-map-4 {[2 1] #{:speculative}
                 [2 2] #{:speculative}})

(def grid-5 [[5 5 nil]
             [3 7 nil]
             [2 3 5]])
(def cell-map-5 {[0 1] #{:speculative}
                 [1 1] #{:speculative}})

(def grid-6 [[5 nil nil]
             [3 nil nil]
             [2 nil nil]
             [5 nil nil]
             [5 nil nil]
             [0 3 2]])
(def cell-map-6 {[5 0] #{:speculative}
                 [5 1] #{:speculative}
                 [5 2] #{:speculative}})

(def grid-7 [[nil nil nil nil]
             [nil 8 nil nil]
             [nil 7 nil nil]
             [nil 5 nil nil]
             [nil 3 nil nil]
             [nil 2 8 nil]
             [nil nil nil nil]])
(def cell-map-7 {[0 1] #{:blocked}
                 [6 1] #{:blocked}
                 [1 0] #{:playable}
                 [2 0] #{:playable}
                 [3 0] #{:playable}
                 [4 0] #{:playable}
                 [5 0] #{:playable}
                 [1 2] #{:playable}
                 [2 2] #{:playable}
                 [3 2] #{:playable}
                 [4 2] #{:playable}
                 [6 2] #{:playable}
                 [5 3] #{:playable}
                 })

(def cell-class (r/atom "red"))

(defn colorful-cell []
  [:div.cell
   {:class    @cell-class
    :on-click #(if (= @cell-class "red")
                 (reset! cell-class "green")
                 (reset! cell-class "red"))}
   ""])

(defn ^:export main []
  ;(render-game app-state)

  (r/render-component (draw-grid nil grid-1 {})
                      (js/document.getElementById "grid-1"))
  (r/render-component (draw-grid nil grid-2 {})
                      (js/document.getElementById "grid-2"))
  (r/render-component (draw-grid nil grid-3 {})
                      (js/document.getElementById "grid-3"))
  (r/render-component (draw-grid nil grid-4 cell-map-4)
                      (js/document.getElementById "grid-4"))
  (r/render-component (draw-grid nil grid-5 cell-map-5)
                      (js/document.getElementById "grid-5"))
  (r/render-component (draw-grid nil grid-6 cell-map-6)
                      (js/document.getElementById "grid-6"))
  (r/render-component (draw-grid nil grid-7 cell-map-7)
                      (js/document.getElementById "grid-7"))

  (r/render-component [:div.grid [colorful-cell]]
                      (js/document.getElementById "reagent-example"))

  )

(defn on-js-reload []
  (main)

  )

(comment
  (swap! app-state m/maybe-end-game)

  (identity @app-state)

  (@app-state :game-over)

  (ai/pick-move (@app-state :grid) (@app-state :player-hand))
  (swap! app-state assoc :player-hand [5])
  (swap! app-state update :player-scores conj {:value 100}))
