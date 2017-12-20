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
          :move-so-far     []
          :original-hand   (app-state :hand)
          :original-grid   (app-state :grid)}))

(defn select-cell [app-state cell]
  (assert (nil? (get-in app-state [:mode :selected-cell])))

  (-> app-state
      (assoc-in [:mode :selected-cell] cell)
      (assoc-in [:mode :available-cells] [])))

(defn select-tile [app-state value]
  (assert (some? (get-in app-state [:mode :selected-cell])))

  (let [[x y] (get-in app-state [:mode :selected-cell])]
    (as-> app-state $
      (assoc-in $ [:grid x y] value)
      (assoc-in $ [:mode :selected-cell] nil)
      (update-in $ [:hand] remove-item value)
      (update-in $ [:mode :move-so-far] conj [[x y] value])
      (assoc-in $ [:mode :available-cells]
                (g/find-next-open-cells-for-move
                  ($ :grid)
                  (get-in $ [:mode :move-so-far]))))))

(defn cancel-mode [app-state]
  (cond-> app-state
    (contains? (app-state :mode) :original-grid) (assoc :grid (get-in app-state [:mode :original-grid]))
    (contains? (app-state :mode) :original-hand) (assoc :hand (get-in app-state [:mode :original-hand]))
    true (assoc :mode {:mode/type :default})))

(defn go-back [app-state]
  (assert (not= (get-in app-state [:mode :mode/type])
                :default))

  (cond
    (and
      (some? (get-in app-state [:mode :selected-cell]))
      (= (count (get-in app-state [:mode :move-so-far]))
         0))
    (cancel-mode app-state)

    (some? (get-in app-state [:mode :selected-cell]))
    (as-> app-state $
      (assoc-in $ [:mode :selected-cell] nil)
      (assoc-in $ [:mode :available-cells]
                (g/find-next-open-cells-for-move
                  ($ :grid)
                  (get-in $ [:mode :move-so-far]))))

    (seq (get-in app-state [:mode :move-so-far]))
    (let [[[x y] value] (last (get-in app-state [:mode :move-so-far]))]

      (-> app-state
          (assoc-in [:grid x y] nil)
          (update-in [:mode :move-so-far] pop)
          (update-in [:hand] conj value)
          (assoc-in [:mode :available-cells] [])
          (assoc-in [:mode :selected-cell] [x y])))))
