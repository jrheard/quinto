(ns quinto.core
  (:require [reagent.core :as r]
            [quinto.html :refer [draw-game]]
            [quinto.grid :refer [empty-grid make-move find-open-cells]]))

; next up:
; * refactor (html shouldn't import grid)
; * spec
; * make board a little more complicated, with some potential multimoves and some blocked,
;   to verify that blocked checking works like i expect
; * start work on ai?

(defonce app-state
         (r/atom {:grid empty-grid}))

(defn ^:export main []
  (swap! app-state update-in [:grid] make-move [[[5 5] 0]
                                                [[5 4] 9]
                                                [[5 3] 1]
                                                [[5 2] 5]
                                                [[5 1] 5]])


  (r/render-component [draw-game @app-state]
                      (js/document.getElementById "app")))

(defn on-js-reload []
  (main))

(comment
  (find-open-cells (@app-state :grid))
  (contains? (set (find-open-cells (@app-state :grid))) [1 1])

  )
