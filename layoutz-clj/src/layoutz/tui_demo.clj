(ns layoutz.tui-demo
  (:require [layoutz.core :refer :all]))

(def app
  {:init
   (fn []
     [{:count 0 :frame 0} (cmd-none)])

   :update
   (fn [msg state]
     (case msg
       :inc   [(update state :count inc) (cmd-none)]
       :dec   [(update state :count dec) (cmd-none)]
       :reset [(assoc state :count 0) (cmd-none)]
       :tick  [(update state :frame inc) (cmd-none)]
       :quit  [state (cmd-exit)]
       [state (cmd-none)]))

   :subscriptions
   (fn [state]
     (sub-batch
      (sub-key-press
       (fn [key]
         (case (:type key)
           :up    :inc
           :down  :dec
           :char  (case (:char key)
                    \k :inc
                    \j :dec
                    \r :reset
                    \q :quit
                    nil)
           nil)))
      (sub-every-ms 80 :tick)))

   :view
   (fn [{:keys [count frame]}]
     (let [bar-val (/ (+ (Math/sin (/ frame 8.0)) 1.0) 2.0)]
       (layout
        [(center (-> "Counter Demo" style-bold color-bright-cyan))
         (-> (hr' :char "=" :width 36) color-bright-black)
         br
         (row [(let [card (-> (status-card "Count" (str count)) border-double)]
                 (if (neg? count) (color-red card) (color-bright-green card)))
               (-> (status-card "Frame" (str frame))
                   border-round
                   color-bright-blue)])
         br
         (inline-bar "Pulse" bar-val)
         br
         (-> (spinner "Running" frame :dots) color-bright-yellow)
         br
         (sparkline (mapv (fn [i]
                            (+ 4.0 (* 4.0 (Math/sin (/ (+ frame i) 5.0)))))
                          (range 20)))
         br
         (-> (kv [["up/k" "increment"]
                  ["down/j" "decrement"]
                  ["r" "reset"]
                  ["q" "quit"]
                  ["Ctrl-Q" "force quit"]])
             color-bright-black)])))})

(defn -main [& _]
  (run-app app {:alignment :center}))
