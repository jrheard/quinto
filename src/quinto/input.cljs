(ns quinto.input
  (:require [cljs.core.async :refer [chan <! put!]]
            [quinto.mode :as m])
  (:require-macros [cljs.core.async :refer [go-loop]]))

(defn handle-game-events
  "Kicks off a go-loop that processes incoming events on `game-event-chan`
  and transitions the `state` atom based on those events."
  [state game-event-chan]
  (go-loop []
    (let [event (<! game-event-chan)]
      ;(js/console.log event)
      (condp = (event :event/type)
        :select-cell (if (= (get-in @state [:mode :mode/type]) :default)
                       (swap! state m/enter-assembling-move-mode (event :cell))
                       (swap! state m/select-cell (event :cell)))
        :select-tile (when (m/can-select-a-tile? @state)
                       (swap! state m/select-tile (event :value)))
        :confirm-move (when (m/can-confirm-move? @state)
                        (swap! state m/confirm-move))
        :go-back (when (m/can-go-back? @state)
                   (swap! state m/go-back))
        :cancel-mode (swap! state m/cancel-mode)
        :view-move (swap! state m/view-historical-move (event :grid) (event :move) (event :optimal-move))
        :stop-viewing-move (swap! state m/stop-viewing-historical-move)
        nil))
    (recur)))

(def ESCAPE-KEY-CODE 27)
(def LEFT-ARROW-KEY-CODE 37)
(def ENTER-KEY-CODE 13)
(def ZERO-KEY-CODE 48)
(def NUMBER-KEY-CODES {49 1
                       50 2
                       51 3
                       52 4
                       53 5})

(defn make-key-handler
  "Returns a function that, when given a event, send the corresponding game event to `game-event-chan`."
  [state game-event-chan]
  (fn key-handler [event]
    (let [key-code (.-keyCode event)

          game-event (condp contains? key-code
                       #{ESCAPE-KEY-CODE} {:event/type :cancel-mode}

                       #{LEFT-ARROW-KEY-CODE} (when (m/can-go-back? @state)
                                                {:event/type :go-back})

                       #{ENTER-KEY-CODE} (when (m/can-confirm-move? @state)
                                           {:event/type :confirm-move})

                       #{ZERO-KEY-CODE} (let [textarea (js/document.createElement "textarea")]
                                          (set! (.-value textarea)
                                                (str "My Quinto game's state is: " (pr-str @state)))
                                          (.appendChild js/document.body textarea)
                                          (.select textarea)
                                          (js/document.execCommand "copy")
                                          (.removeChild js/document.body textarea))

                       NUMBER-KEY-CODES (when (m/can-select-a-tile? @state)
                                          (let [hand (@state :player-hand)
                                                hand-index (dec (NUMBER-KEY-CODES key-code))]
                                            (when (< hand-index (count hand))
                                              {:event/type :select-tile
                                               :value      (nth hand hand-index)})))
                       nil)]

      (when game-event
        (put! game-event-chan game-event)))))
