(ns layoutz.demo (:require [layoutz.core :refer :all]))

;; Define layouts
(def t
  (-> (table ["Name" "Role" "Status"]
             [["Alice" "Engineer" "Online"]
              ["Eve"   "QA"       "Away"]
              [(ul [(li "Gegard"
                        :c [(li "Mousasi"
                                :c [(li "was a BAD man")])])])
               "Fighter" "Nasty"]])
      border-round))

;; Color gradient
(def gradient
  (map (fn [i]
         (let [r (if (< i 128) (* i 2) 255)
               g (if (< i 128) 255 (* (- 255 i) 2))
               b (if (> i 128) (* (- i 128) 2) 0)]
           (-> "█" (color-rgb r g b))))
       (range 0 256 12)))

;; Nest, compose, combine
(def d
  (layout
   [(center (row [(underline-colored (-> "Layoutz" style-bold) "^" bright-magenta)
                  "... A Small Demo (ちいさい)"]))
    (row [(-> (status-card "Users" "1.2K") color-bright-blue)
          (-> (status-card "API"   "UP")   border-double color-bright-green)
          (-> (status-card "CPU"   "23%")  border-thick  color-bright-yellow)
          t
          (section "Pugilists"
                   [(kv [["Kazushi"    "Sakuraba"]
                         ["Jet 李連杰" "Li"]
                         ["Rory"       "MacDonald"]])
                    (tight-row gradient)])])
    (row [(layout
           [(-> (box "Wrapped" [(wrap 20 "Where there is a will ... Water x Necessaries")])
                color-bright-magenta style-reverse style-bold)
            (ol [(li "Arcole")
                 (li "Austerlitz")
                 (li "Wagram" :c [(li "Iéna" :c [(li "Бородино")])])])])
          (margin "[Clojure!]"
                  (layout
                   [(-> (box "Deploy Status"
                             [(inline-bar "Build"  1.0)
                              (inline-bar "Test"   0.8)
                              (inline-bar "Deploy" 0.3)])
                        color-green)
                    (-> (tree (node "📁 Project"
                                    :c [(node "src"
                                              :c [(node "main.clj")
                                                  (node "test.clj")])]))
                        color-cyan)]))])]))

;; Get pretty strings w/ render, or just print-elem
(defn -main [& _] (print-elem d))
