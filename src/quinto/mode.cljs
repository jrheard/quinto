(ns quinto.mode
  (:require [quinto.grid :as g]))

(defn enter-assembling-move-mode [app-state selected-cell]
  (assert (contains? (set (g/find-playable-cells (app-state :grid)))
                     selected-cell))

  (assoc app-state
         :mode
         {:mode/type       :assembling-move
          :selected-cell   selected-cell
          :available-cells []
          :move-so-far     []}))

(defn cancel-mode [app-state]
  (assoc app-state :mode {:mode/type :default}))
