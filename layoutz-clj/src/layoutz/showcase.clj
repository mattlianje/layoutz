(ns layoutz.showcase
  (:require [layoutz.core :refer :all]
            [clojure.string :as str]))

;; Constants -------------------------------------------------------------------

(def total-scenes 7)

(def scene-names
  ["Physics Game" "Text Input & Lists" "Borders & Styles"
   "Tables" "Charts & Plots" "Bar Charts & Sparklines"
   "Selections & Heatmap"])

(def services
  [["API Gateway" "LIVE" "12ms" "99.99%"]
   ["Database"    "LIVE" "3ms"  "99.95%"]
   ["Cache"       "WARN" "1ms"  "98.50%"]
   ["Queue"       "LIVE" "8ms"  "99.90%"]
   ["Auth"        "LIVE" "5ms"  "99.97%"]
   ["CDN"         "LIVE" "2ms"  "99.99%"]])

(def scene-width 75)

;; Helpers ---------------------------------------------------------------------

(defn- toggle-in [x xs]
  (if (some #{x} xs) (filterv #(not= % x) xs) (conj xs x)))

(defn- bounce [y vy]
  (cond
    (<= y 0)                          [0.0 (* (Math/abs vy) 0.82)]
    (> y 12)                          [12.0 (- (* (Math/abs vy) 0.5))]
    (and (< (Math/abs vy) 0.05)
         (< y 0.1))                   [0.0 0.0]
    :else                             [y vy]))

;; Initial state ---------------------------------------------------------------

(def initial-state
  {:scene 0 :tick 0 :text-value "" :items []
   :adding-item false :add-tick 0 :selected [] :cursor 0
   :line-offset 0 :table-row 0 :table-selected []
   :bar-mode 0 :ball-y 10.0 :ball-vy 0.0 :gravity 5
   :ball-trail (vec (repeat 80 10.0))})

;; Update ----------------------------------------------------------------------

(defn- update-tick [s]
  (let [;; Text input adding animation
        s1 (if (:adding-item s)
             (if (>= (:add-tick s) 8)
               (-> s
                   (assoc :adding-item false :add-tick 0 :text-value "")
                   (update :items conj (:text-value s)))
               (update s :add-tick inc))
             s)
        ;; Ball physics
        g     (* (double (:gravity s1)) 0.08)
        new-vy (- (:ball-vy s1) g)
        raw-y  (+ (:ball-y s1) (* new-vy 0.3))
        [ny vy] (bounce raw-y new-vy)
        trail (conj (subvec (:ball-trail s1)
                            (max 0 (- (count (:ball-trail s1)) 79)))
                    ny)]
    (assoc s1 :tick (inc (:tick s1)) :ball-y ny :ball-vy vy :ball-trail trail)))

(defn- app-update [msg s]
  (case msg
    :next-scene [(update s :scene #(mod (inc %) total-scenes)) (cmd-none)]
    :prev-scene [(update s :scene #(mod (+ % (dec total-scenes)) total-scenes)) (cmd-none)]
    :tick       [(update-tick s) (cmd-none)]

    :backspace  [(if (:adding-item s) s
                     (update s :text-value #(if (empty? %) "" (subs % 0 (dec (count %))))))
                 (cmd-none)]

    :submit-item [(if (and (not (:adding-item s)) (seq (:text-value s)))
                    (assoc s :adding-item true :add-tick 0)
                    s)
                  (cmd-none)]

    :toggle-select [(cond
                      (= (:scene s) 3) (update s :table-selected #(toggle-in (:table-row s) %))
                      (= (:scene s) 6) (update s :selected #(toggle-in (:cursor s) %))
                      :else s)
                    (cmd-none)]

    :cursor-up [(cond
                  (= (:scene s) 3) (update s :table-row #(mod (+ % (dec (count services))) (count services)))
                  (= (:scene s) 6) (update s :cursor #(mod (+ % 6) 7))
                  :else s)
                (cmd-none)]

    :cursor-down [(cond
                    (= (:scene s) 3) (update s :table-row #(mod (inc %) (count services)))
                    (= (:scene s) 6) (update s :cursor #(mod (inc %) 7))
                    :else s)
                  (cmd-none)]

    :adjust-up [(if (= (:scene s) 0)
                  (update s :gravity #(min (inc %) 15))
                  (update s :line-offset #(min (inc %) 10)))
                (cmd-none)]

    :adjust-down [(if (= (:scene s) 0)
                    (update s :gravity #(max (dec %) 1))
                    (update s :line-offset #(max (dec %) -10)))
                  (cmd-none)]

    :toggle-bar-mode [(update s :bar-mode #(mod (inc %) 2)) (cmd-none)]

    :kick-ball [(assoc s :ball-vy 5.0) (cmd-none)]

    ;; Default: check for :go-scene or :type-char
    (cond
      (and (vector? msg) (= (first msg) :go-scene))
      (let [n (second msg)]
        [(if (and (>= n 0) (< n total-scenes)) (assoc s :scene n) s) (cmd-none)])

      (and (vector? msg) (= (first msg) :type-char))
      [(if (:adding-item s) s
           (update s :text-value str (second msg)))
       (cmd-none)]

      :else [s (cmd-none)])))

;; Subscriptions ---------------------------------------------------------------

(defn- app-subscriptions [s]
  (sub-batch
   (sub-every-ms 80 :tick)
   (sub-key-press
    (fn [{:keys [type char]}]
      (case type
        :right     :next-scene
        :left      :prev-scene
        :up        :cursor-up
        :down      :cursor-down
        :backspace :backspace
        :enter     (when (= (:scene s) 1) :submit-item)
        :tab       (when (= (:scene s) 5) :toggle-bar-mode)
        :char      (cond
                     (= char \+)                                          :adjust-up
                     (= char \-)                                          :adjust-down
                     (= char \space) (cond
                                       (= (:scene s) 0) :kick-ball
                                       (= (:scene s) 1) [:type-char char]
                                       (#{3 6} (:scene s)) :toggle-select
                                       :else nil)
                     (and (<= (int \1) (int char) (int \7)))
                     [:go-scene (- (int char) (int \1))]
                     (and (= (:scene s) 1)
                          (Character/isLetterOrDigit char))
                     [:type-char char]
                     :else nil)
        nil)))))

;; View ------------------------------------------------------------------------

(defn- render-header [s]
  (let [scene-dots (str/join " " (map #(if (= % (:scene s)) "●" "○") (range total-scenes)))
        prefix     " ─── "
        title-str  "layoutz"
        suffix     (str (inc (:scene s)) " / " total-scenes)
        dash-count (max 3 (- scene-width (count prefix) (count title-str) (count suffix) 2))
        dashes     (apply str (repeat dash-count \─))]
    (layout
     [br
      (tight-row
       [(-> prefix color-bright-black)
        (-> title-str style-bold color-bright-cyan)
        (-> (str " " dashes " ") color-bright-black)
        (-> suffix color-bright-black)])
      br
      (-> (str " " (nth scene-names (:scene s))) style-bold color-bright-yellow)
      (str " " scene-dots)])))

(defn- render-footer [s]
  (let [hints (case (int (:scene s))
                0 "  </> scenes  Space kick  +/- gravity  Ctrl-Q quit"
                1 "  </> scenes  type + Enter to add  Ctrl-Q quit"
                3 "  </> scenes  ^/v navigate  Space select  Ctrl-Q quit"
                4 "  </> scenes  +/- move threshold  Ctrl-Q quit"
                5 "  </> scenes  Tab cycle chart mode  Ctrl-Q quit"
                6 "  </> scenes  ^/v navigate  Space toggle  Ctrl-Q quit"
                "  </> scenes  Ctrl-Q quit")]
    (-> hints style-dim color-bright-black)))

;; Scene 1: Physics Game -------------------------------------------------------

(defn- scene-physics-game [s]
  (let [trail-points (mapv (fn [i] [(double i) (nth (:ball-trail s) i)])
                           (range (count (:ball-trail s))))
        g-label   (format "g = %.2f" (* (double (:gravity s)) 0.08))
        vel-label (format "vy = %.1f" (:ball-vy s))
        y-label   (format "y = %.1f" (:ball-y s))
        bounds    [[0.0 0.0] [0.0 12.0]]
        energy    (min 1.0 (/ (+ (Math/abs (:ball-vy s)) (:ball-y s)) 15.0))
        bar-w     14
        filled    (int (Math/floor (* energy bar-w)))
        pct       (int (Math/floor (* energy 100)))
        energy-bar (str "Energy "
                        (apply str (repeat filled \█))
                        (apply str (repeat (- bar-w filled) \░))
                        " " pct "%")]
    (row [(layout
           [(-> "Trajectory" color-bright-yellow)
            (plot-line 35 12
                       [(series trail-points "ball" bright-cyan)
                        (series bounds " " bright-black)])])
          (-> (box "Physics"
                   [(left-align 28
                                (render
                                 (layout
                                  [(kv [["gravity" g-label] ["velocity" vel-label] ["height" y-label]])
                                   br
                                   (-> energy-bar color-bright-green)
                                   br
                                   (-> (spinner "Simulating" (quot (:tick s) 3) :dots) color-bright-cyan)
                                   (-> "Press Space to kick ball!" style-bold color-bright-yellow)])))])
              border-round color-bright-magenta)])))

;; Scene 2: Text Input & Lists ------------------------------------------------

(defn- scene-text-input [s]
  (let [input-line
        (if (:adding-item s)
          (row [(-> (spinner "Adding" (:add-tick s) :dots) color-bright-yellow)
                (-> (str "  \"" (:text-value s) "\"") color-bright-yellow)])
          (let [display (if (empty? (:text-value s))
                          (-> "Type something..." color-bright-black)
                          (-> (:text-value s) color-bright-white))]
            (tight-row [(-> "> " color-bright-cyan) display (-> "_" style-blink)])))

        item-colors [bright-green bright-blue bright-magenta bright-yellow bright-cyan]
        item-list
        (if (empty? (:items s))
          (-> "  (no items yet)" color-bright-black)
          (layout
           (map-indexed
            (fn [i item]
              (tight-row
               [(-> (str "  " (inc i) ". ") color-bright-black)
                (fg-color (nth item-colors (mod i (count item-colors))) item)]))
            (:items s))))

        box-w    32
        its      (:items s)
        longest  (if (empty? its) "-" (reduce #(if (>= (count %1) (count %2)) %1 %2) its))
        shortest (if (empty? its) "-" (reduce #(if (<= (count %1) (count %2)) %1 %2) its))
        cnt      (count its)
        ;; Left box content lines: input(1) + br(1) + "Items:"(1) + max(1,cnt) items
        left-lines  (+ 3 (max 1 cnt))
        ;; Right box content lines: stats(3) + br(1) + status(1) = 5
        right-lines 5
        left-pad    (max 0 (- right-lines left-lines))
        right-pad   (max 0 (- left-lines right-lines))]
    (row [(-> (box "Add Items"
                   (vec
                    (concat
                     [(left-align box-w (render input-line))
                      br
                      (left-align box-w
                                  (render (layout [(-> "Items:" style-bold)
                                                   item-list])))]
                     (repeat left-pad ""))))
              border-round color-bright-cyan)
          (-> (box "Stats"
                   (vec
                    (concat
                     [(left-align box-w
                                  (render (layout
                                           [(tight-row ["Total items: " (-> (str cnt) style-bold color-bright-cyan)])
                                            (tight-row ["Longest:     " (-> longest color-bright-magenta)])
                                            (tight-row ["Shortest:    " (-> shortest color-bright-magenta)])])))
                      br
                      (left-align box-w
                                  (render
                                   (if (>= cnt 3)
                                     (-> "Nice collection!" style-bold color-bright-green)
                                     (-> (str "Add " (- 3 cnt) " more...") color-bright-black))))]
                     (repeat right-pad ""))))
              border-round)])))

;; Scene 3: Borders & Styles --------------------------------------------------

(defn- scene-borders-styles [_s]
  (let [mk-box (fn [border-fn name color]
                 (fg-color color (-> (box name [(left-align 8 name)]) border-fn)))]
    (layout
     [(-> "Border Styles" style-bold color-bright-yellow)
      (row [(mk-box identity   "Single" bright-cyan)
            (mk-box border-double "Double" bright-magenta)
            (mk-box border-round  "Round"  bright-green)])
      (row [(mk-box border-thick  "Thick"  bright-yellow)
            (mk-box border-dashed "Dashed" bright-blue)
            (mk-box border-ascii  "Ascii"  bright-white)])
      br
      (-> "Text Styles" style-bold color-bright-yellow)
      (row [(-> (box "Standard"
                     [(-> "Bold" style-bold color-bright-cyan)
                      (-> "Italic" style-italic color-bright-magenta)
                      (-> "Underline" style-underline-s color-bright-green)])
                border-round)
            (-> (box "Extended"
                     [(-> "Dim" style-dim color-bright-yellow)
                      (-> "Strikethrough" style-strikethrough color-bright-red)
                      (-> "Bold+Italic" style-bold style-italic color-bright-white)])
                border-round)])])))

;; Scene 4: Tables -------------------------------------------------------------

(defn- scene-tables [s]
  (let [colored-rows
        (map-indexed
         (fn [idx [name status lat up]]
           (let [is-active (= idx (:table-row s))
                 is-sel    (some #{idx} (:table-selected s))
                 mark      (if is-sel "* " "  ")
                 cells     [(str mark name) status lat up]
                 apply-style (cond
                               (and is-active is-sel) #(-> % style-bold style-reverse color-bright-green)
                               is-active              #(-> % style-bold style-reverse color-bright-cyan)
                               is-sel                 #(-> % color-bright-green)
                               :else                  identity)]
             (mapv apply-style cells)))
         services)
        sel-count (count (:table-selected s))
        sel-info  (if (pos? sel-count)
                    (-> (str sel-count " selected") color-bright-green)
                    (-> "none selected" color-bright-black))]
    (layout
     [(-> (table ["Service" "Status" "Latency" "Uptime"] (vec colored-rows))
          border-round)
      (tight-row
       [(-> (str " Row " (inc (:table-row s)) "/" (count services) "  |  ")
            color-bright-black)
        sel-info])])))

;; Scene 5: Charts & Plots ----------------------------------------------------

(defn- scene-charts-plots [s]
  (let [sin-points  (mapv (fn [i]
                            (let [x (* (double i) 0.08)]
                              [x (* (Math/sin (+ x (* (double (:tick s)) 0.06))) 4)]))
                          (range 101))
        intercept   (* (double (:line-offset s)) 0.5)
        line-points (mapv (fn [i]
                            (let [x (* (double i) 0.08)]
                              [x (+ (* 0.5 x) intercept)]))
                          (range 101))
        sign        (if (>= intercept 0) "+" "-")
        line-label  (format "0.5x %s %.1f" sign (Math/abs intercept))]
    (row [(layout
           [(-> (str "sin(x) & y = " line-label "  [+/- to shift]") color-bright-yellow)
            (plot-line 35 12
                       [(series sin-points "sin(x)" bright-cyan)
                        (series line-points "linear" bright-yellow)])])
          (layout
           [(-> "Revenue Share" color-bright-yellow)
            (plot-pie 30 8
                      [(slice 45 "Product"   bright-cyan)
                       (slice 30 "Services"  bright-magenta)
                       (slice 15 "Licensing" bright-yellow)
                       (slice 10 "Other"     bright-green)])])])))

;; Scene 6: Bar Charts & Sparklines -------------------------------------------

(defn- scene-bar-sparklines [s]
  (let [spark-data (mapv (fn [i]
                           (+ (* (Math/sin (* (+ i (:tick s)) 0.3)) 10) 15))
                         (range 30))
        mode-name  (if (zero? (:bar-mode s)) "Vertical Bars" "Stacked Bars")
        chart-elem (if (zero? (:bar-mode s))
                     (plot-bar 30 8
                               [(bar-item 85  "Mon" bright-cyan)
                                (bar-item 120 "Tue" bright-green)
                                (bar-item 95  "Wed" bright-magenta)
                                (bar-item 110 "Thu" bright-yellow)
                                (bar-item 75  "Fri" bright-blue)])
                     (plot-stacked-bar 30 8
                                       [(stacked-bar-group "Q1"
                                                           [(bar-item 50 "Online" bright-cyan)
                                                            (bar-item 35 "Retail" bright-green)
                                                            (bar-item 15 "Other"  bright-magenta)])
                                        (stacked-bar-group "Q2"
                                                           [(bar-item 70 "Online" bright-cyan)
                                                            (bar-item 30 "Retail" bright-green)
                                                            (bar-item 20 "Other"  bright-magenta)])
                                        (stacked-bar-group "Q3"
                                                           [(bar-item 45 "Online" bright-cyan)
                                                            (bar-item 55 "Retail" bright-green)
                                                            (bar-item 10 "Other"  bright-magenta)])
                                        (stacked-bar-group "Q4"
                                                           [(bar-item 60 "Online" bright-cyan)
                                                            (bar-item 40 "Retail" bright-green)
                                                            (bar-item 25 "Other"  bright-magenta)])]))]
    (row [(layout
           [(-> "Live Signal" color-bright-yellow)
            (-> (sparkline spark-data) color-bright-cyan)])
          (layout
           [(-> (str mode-name "  [Tab to cycle]") color-bright-yellow)
            chart-elem])])))

;; Scene 7: Selections & Heatmap -----------------------------------------------

(defn- scene-selections-heatmap [s]
  (let [days  ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]
        hours ["6am" "9am" "12pm" "3pm" "6pm" "9pm"]
        selector-lines
        (map-indexed
         (fn [idx day]
           (let [is-sel (some #{idx} (:selected s))
                 is-cur (= (:cursor s) idx)
                 check  (if is-sel "[x]" "[ ]")
                 arrow  (if is-cur "> " "  ")
                 label  (str arrow check " " day)
                 apply-style (cond
                               (and is-cur is-sel) #(-> % style-bold color-bright-green)
                               is-cur              #(-> % style-bold color-bright-cyan)
                               is-sel              #(-> % color-bright-green)
                               :else               identity)]
             (apply-style label)))
         days)
        sel-count (count (:selected s))
        base-data [[10 45 80 75 50 15]
                   [12 50 85 70 55 20]
                   [8 40 90 80 60 25]
                   [15 55 75 65 45 18]
                   [10 48 70 60 35 30]
                   [5 15 25 30 40 55]
                   [3 10 20 25 35 45]]
        heat-data (if (empty? (:selected s))
                    (mapv #(mapv double %) base-data)
                    (vec (map-indexed
                          (fn [idx r]
                            (if (some #{idx} (:selected s))
                              (mapv double r)
                              (mapv #(* (double %) 0.15) r)))
                          base-data)))]
    (row [(-> (box "Schedule"
                   [(layout (vec selector-lines))
                    br
                    (fg-color (if (pos? sel-count) bright-green bright-black)
                              (str sel-count " of " (count days) " active"))])
              border-round color-bright-cyan)
          (-> (box "Weekly Activity"
                   [(plot-heatmap 5
                                  (heatmap-data heat-data days hours))])
              border-round)])))

;; View dispatch ---------------------------------------------------------------

(defn- app-view [s]
  (let [header  (render-header s)
        content (case (int (:scene s))
                  0 (scene-physics-game s)
                  1 (scene-text-input s)
                  2 (scene-borders-styles s)
                  3 (scene-tables s)
                  4 (scene-charts-plots s)
                  5 (scene-bar-sparklines s)
                  6 (scene-selections-heatmap s)
                  "Unknown scene")
        footer  (render-footer s)]
    (left-align scene-width
                (render (layout [header br content br footer])))))

;; App -------------------------------------------------------------------------

(def showcase-app
  {:init          (fn [] [initial-state (cmd-none)])
   :update        app-update
   :subscriptions app-subscriptions
   :view          app-view})

(defn -main [& _]
  (run-app showcase-app {:alignment :center}))
