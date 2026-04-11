(ns layoutz.inline-demo
  (:require [layoutz.core :refer :all]))

;; Inline loading demo — progress bars that render in-place without
;; clearing the screen. Prior output stays visible.
;;
;; Run with: make inline-demo

(defn progress-bar [label speed]
  {:init
   (fn [] [{:progress 0.0 :done-ticks 0} (cmd-none)])

   :update
   (fn [msg state]
     (case msg
       :tick (cond
               (> (:done-ticks state) 20)
               [state (cmd-exit)]

               (>= (:progress state) 1.0)
               [(update state :done-ticks inc) (cmd-none)]

               :else
               [(update state :progress #(min 1.0 (+ % speed))) (cmd-none)])
       [state (cmd-none)]))

   :subscriptions
   (fn [_] (sub-every-ms 16 :tick))

   :view
   (fn [{:keys [progress]}]
     (let [w       40
           filled  (int (* progress w))
           bar     (map (fn [i]
                          (if (< i filled)
                            (let [ratio (/ (double i) w)
                                  r     (+ (int (* ratio 180.0)) 50)
                                  g     (+ (int (* (- 1.0 ratio) 200.0)) 55)]
                              (-> "█" (color-rgb r g 255)))
                            (-> "░" color-bright-black)))
                        (range w))
           pct     (str (int (* progress 100.0)) "%")]
       (layout [(tight-row bar)
                (-> (str label " " pct) color-bright-cyan)])))})

(defn -main [& _]
  (println "hello from a normal process")
  (Thread/sleep 800)
  (println "doing some work...")
  (Thread/sleep 700)
  (println "now watch this:")
  (Thread/sleep 800)
  (println)
  (run-inline (progress-bar "Fetching deps..." 0.018))
  (run-inline (progress-bar "Building..." 0.010))
  (run-inline (progress-bar "Linking..." 0.025))
  (println)
  (println "back to normal output"))
