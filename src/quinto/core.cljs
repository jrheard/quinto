(ns quinto.core
    (:require [reagent.core :as r])
    )


(defonce app-state
         (atom {:grid []}))


(defn draw-game [state]
  [:div "butt"]
  )

(defn ^:export main []
  (r/render-component [draw-game @app-state]
                      (js/document.getElementById "app")))


(defn on-js-reload []
  (main))
