(ns quinto.mode
  (:require [quinto.grid :as g]
            [quinto.utils :refer [remove-item]]))

(defn enter-assembling-move-mode [app-state selected-cell]
  (assert (contains? (set (g/find-playable-cells (app-state :grid)))
                     selected-cell))

  (assoc app-state
         :mode
         {:mode/type       :assembling-move
          :selected-cell   selected-cell
          :available-cells []
          :move-so-far []
          :original-hand   (app-state :hand)
          :original-grid   (app-state :grid)}))

(defn select-tile [app-state value]
  (let [[x y] (get-in app-state [:mode :selected-cell])]
    (-> app-state
        (assoc-in [:grid x y] value)
        (assoc-in [:mode :selected-cell] nil)
        (update-in [:hand] remove-item value)
        (update-in [:mode :mode-so-far] conj [[x y] value])
        ; xxxxx update available-cells
        ; this will be kind of tricky, because you have to handle situations where
        ; the move crosses one or more filled cells
        )))

(defn cancel-mode [app-state]
  (cond-> app-state
    (contains? (app-state :mode) :original-grid) (assoc :grid (get-in app-state [:mode :original-grid]))
    (contains? (app-state :mode) :original-hand) (assoc :hand (get-in app-state [:mode :original-hand]))
    true (assoc :mode {:mode/type :default})))
