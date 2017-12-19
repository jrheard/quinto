(ns quinto.mode
  (:require [quinto.grid :as g]))

(defn enter-assembling-move-mode! [app-state selected-cell]
  (assert (contains? (set (g/find-playable-cells (@app-state :grid)))
                     selected-cell))

  (swap! app-state assoc :mode
         {:mode/type       :assembling-move
          :selected-cell   selected-cell
          :available-cells []
          :move-so-far     []}))
