(ns layoutz.demo
  (:require [layoutz.core :refer :all]))

(def t
  (-> (table
       ["Name" "Role" "Status"]
       [["Alice" "Engineer" "Online"]
        ["Eve"   "QA"       "Away"]
        [(ul [(li "Gegard"
                  :c [(li "Mousasi"
                          :c [(li "was a BAD man")])])])
         "Fighter"
         "Nasty"]])
      border-round))

(def gradient
  (map (fn [i]
         (let [r (if (< i 128) (* i 2) 255)
               g (if (< i 128) 255 (* (- 255 i) 2))
               b (if (> i 128) (* (- i 128) 2) 0)]
           (-> "█" (color-rgb r g b))))
       (range 0 265 12)))

(def d
  (layout
   [(center
     (row
      [(underline-colored (-> "Layoutz" style-bold)
                          "^" bright-magenta)
       "... A Small Demo (ちいさい)"]))
    (row
     [(-> (status-card "Users" "1.2K") color-bright-blue)
      (-> (status-card "API" "UP") border-double color-bright-green)
      (-> (status-card "Errors" "0") border-round color-bright-red)])
    br
    (-> (section "Data" [(chart [["Scala" 85.0] ["OCaml" 62.0]
                                 ["Haskell" 70.0] ["Clojure" 95.0]])])
        color-cyan)
    br
    t
    br
    (tight-row gradient)
    br
    (kv [["Language" "Clojure"]
         ["Version"  "0.1.0"]
         ["Status"   "Alpha"]])
    br
    (sparkline [1.0 3.0 7.0 5.0 9.0 2.0 6.0 4.0 8.0 3.0])
    br
    (-> (box "Trees!"
             [(tree (node "root"
                          :c [(node "src"
                                    :c [(node "core.clj")
                                        (node "demo.clj")])
                              (node "deps.edn")]))])
        border-round
        color-bright-yellow)
    br
    (-> (section "Plots" []) color-bright-cyan)
    br
    (row [(plot-line 25 8
                     [(series (mapv (fn [x] [x (Math/sin x)]) (range 0 7 0.3))
                              "sin" bright-cyan)
                      (series (mapv (fn [x] [x (Math/cos x)]) (range 0 7 0.3))
                              "cos" bright-magenta)])
          (plot-pie 12 6
                    [(slice 45 "Scala" bright-cyan)
                     (slice 30 "Clj" bright-magenta)
                     (slice 15 "OCaml" bright-yellow)
                     (slice 10 "Hs" bright-green)])])
    br
    (row [(plot-bar 24 8
                    [(bar-item 85 "Go" bright-cyan)
                     (bar-item 95 "Rust" bright-magenta)
                     (bar-item 72 "Zig" bright-yellow)])
          (plot-stacked-bar 24 8
                            [(stacked-bar-group "Q1"
                                                [(bar-item 30 "Rev" bright-cyan)
                                                 (bar-item 20 "Cost" bright-red)])
                             (stacked-bar-group "Q2"
                                                [(bar-item 45 "Rev" bright-cyan)
                                                 (bar-item 25 "Cost" bright-red)])])])
    br
    (plot-heatmap
     (heatmap-data [[1.0 3.0 7.0 9.0 5.0]
                    [4.0 8.0 2.0 6.0 3.0]
                    [7.0 1.0 5.0 8.0 4.0]]
                   ["Mon" "Tue" "Wed"]
                   ["Q1" "Q2" "Q3" "Q4" "Q5"]))]))

(defn -main [& _]
  (print-elem d))
