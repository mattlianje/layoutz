;; +==========================================================================+
;; |                               layoutz                                    |
;; |               Friendly, expressive print-layout DSL                      |
;; |                                                                          |
;; | Copyright 2026 Matthieu Court (matthieu.court@protonmail.com)            |
;; | Apache License 2.0                                                       |
;; +==========================================================================+

(ns layoutz.core
  (:require [clojure.string :as str]))

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defn strip-ansi
  "Strip ANSI escape codes from a string for accurate width calculation."
  [s]
  (str/replace s #"\033\[[0-9;]*m" ""))

(defn- wide-codepoint?
  "Check if a Unicode codepoint is double-width in terminals (emojis, CJK, fullwidth)."
  [cp]
  (or (<= 0x1100 cp 0x115F)
      (<= 0x2E80 cp 0x9FFF)
      (<= 0xAC00 cp 0xD7AF)
      (<= 0xF900 cp 0xFAFF)
      (<= 0xFE30 cp 0xFE4F)
      (<= 0xFF01 cp 0xFF60)
      (<= 0xFFE0 cp 0xFFE6)
      (<= 0x1F000 cp 0x1FFFF)
      (<= 0x20000 cp 0x2FA1F)
      (<= 0x2600 cp 0x27BF)))

(defn display-width
  "Calculate display width of a string, accounting for double-width characters."
  [s]
  (let [cps (.codePoints ^String s)]
    (reduce (fn [acc cp]
              (+ acc (if (wide-codepoint? cp) 2 1)))
            0
            (iterator-seq (.iterator cps)))))

(defn visible-length
  "Calculate visible length of a string (ignoring ANSI codes, accounting for double-width chars)."
  [s]
  (display-width (strip-ansi s)))

(defn- lines [s]
  (if (= s "") [""]
      (str/split s #"\n" -1)))

(defn- unlines [ls]
  (str/join "\n" ls))

(defn- pad-right [target-width s]
  (let [len (visible-length s)
        pad (- target-width len)]
    (if (<= pad 0) s
        (str s (apply str (repeat pad \space))))))

(defn- pad-left [target-width s]
  (let [len (visible-length s)
        pad (- target-width len)]
    (if (<= pad 0) s
        (str (apply str (repeat pad \space)) s))))

(defn- center-string [target-width s]
  (let [len (visible-length s)]
    (if (>= len target-width) s
        (let [total-pad (- target-width len)
              left-pad  (quot total-pad 2)
              right-pad (- total-pad left-pad)]
          (str (apply str (repeat left-pad \space))
               s
               (apply str (repeat right-pad \space)))))))

(defn- repeat-str [n s]
  (apply str (repeat n s)))

;; ============================================================================
;; Element Protocol
;; ============================================================================

(defprotocol Element
  (render [this] "Render element to string")
  (elem-width [this] "Get element width")
  (elem-height [this] "Get element height"))

(defprotocol Borderable
  (with-border [this border] "Set border style")
  (get-border [this] "Get border style"))

(extend-type String
  Element
  (render [this] this)
  (elem-width [this]
    (reduce max 0 (map visible-length (lines this))))
  (elem-height [this]
    (count (lines this))))

;; ============================================================================
;; Border Styles
;; ============================================================================

(def border-chars
  {:normal        {:tl "┌" :tr "┐" :bl "└" :br "┘" :h "─" :v "│" :lt "├" :rt "┤" :cross "┼" :tt "┬" :bt "┴"}
   :double        {:tl "╔" :tr "╗" :bl "╚" :br "╝" :h "═" :v "║" :lt "╠" :rt "╣" :cross "╬" :tt "╦" :bt "╩"}
   :thick         {:tl "┏" :tr "┓" :bl "┗" :br "┛" :h "━" :v "┃" :lt "┣" :rt "┫" :cross "╋" :tt "┳" :bt "┻"}
   :round         {:tl "╭" :tr "╮" :bl "╰" :br "╯" :h "─" :v "│" :lt "├" :rt "┤" :cross "┼" :tt "┬" :bt "┴"}
   :ascii         {:tl "+" :tr "+" :bl "+" :br "+" :h "-" :v "|" :lt "+" :rt "+" :cross "+" :tt "+" :bt "+"}
   :block         {:tl "█" :tr "█" :bl "█" :br "█" :h "█" :v "█" :lt "█" :rt "█" :cross "█" :tt "█" :bt "█"}
   :dashed        {:tl "┌" :tr "┐" :bl "└" :br "┘" :h "╌" :v "╎" :lt "├" :rt "┤" :cross "┼" :tt "┬" :bt "┴"}
   :dotted        {:tl "┌" :tr "┐" :bl "└" :br "┘" :h "┈" :v "┊" :lt "├" :rt "┤" :cross "┼" :tt "┬" :bt "┴"}
   :inner-half-block {:tl "▗" :tr "▖" :bl "▝" :br "▘" :h "▄" :v "▐" :v-right "▌"
                      :lt "▐" :rt "▌" :cross "▄" :tt "▄" :bt "▀"
                      :h-bottom "▀"}
   :outer-half-block {:tl "▛" :tr "▜" :bl "▙" :br "▟" :h "▀" :v "▌" :v-right "▐"
                      :lt "▌" :rt "▐" :cross "▀" :tt "▀" :bt "▄"
                      :h-bottom "▄"}
   :markdown      {:tl "|" :tr "|" :bl "|" :br "|" :h "-" :v "|" :lt "|" :rt "|" :cross "|" :tt "|" :bt "|"}
   :none          {:tl " " :tr " " :bl " " :br " " :h " " :v " " :lt " " :rt " " :cross " " :tt " " :bt " "}})

(defn- get-chars [border]
  (if (keyword? border)
    (let [c (get border-chars border (get border-chars :normal))]
      {:tl (:tl c) :tr (:tr c) :bl (:bl c) :br (:br c)
       :h-top    (or (:h c) " ")
       :h-bottom (or (:h-bottom c) (:h c) " ")
       :v-left   (or (:v c) " ")
       :v-right  (or (:v-right c) (:v c) " ")
       :left-tee  (:lt c)
       :right-tee (:rt c)
       :cross     (:cross c)
       :top-tee   (:tt c)
       :bottom-tee (:bt c)})
    ;; Custom border: {:corner "x" :h "-" :v "|"}
    (let [{:keys [corner h v]} border]
      {:tl corner :tr corner :bl corner :br corner
       :h-top h :h-bottom h :v-left v :v-right v
       :left-tee corner :right-tee corner :cross corner
       :top-tee corner :bottom-tee corner})))

;; ============================================================================
;; Color & Style
;; ============================================================================

(defn- fg-code [color]
  (cond
    (nil? color)    nil
    (integer? color) (str color)
    (vector? color)
    (case (first color)
      :c256 (str "38;5;" (second color))
      :rgb  (str "38;2;" (nth color 1) ";" (nth color 2) ";" (nth color 3))
      nil)
    :else nil))

(defn- bg-code [color]
  (cond
    (nil? color)    nil
    (integer? color) (str (+ color 10))
    (vector? color)
    (case (first color)
      :c256 (str "48;5;" (second color))
      :rgb  (str "48;2;" (nth color 1) ";" (nth color 2) ";" (nth color 3))
      nil)
    :else nil))

;; Named color values (raw ANSI base codes, for use as fg values)
(def black         30)
(def red           31)
(def green         32)
(def yellow        33)
(def blue          34)
(def magenta       35)
(def cyan          36)
(def white         37)
(def bright-black  90)
(def bright-red    91)
(def bright-green  92)
(def bright-yellow 93)
(def bright-blue   94)
(def bright-magenta 95)
(def bright-cyan   96)
(def bright-white  97)

(defn rgb [r g b] [:rgb r g b])
(defn c256 [n]    [:c256 n])

;; Style codes (raw ANSI codes, for use with with-style)
(def ^:private code-bold          "1")
(def ^:private code-dim           "2")
(def ^:private code-italic        "3")
(def ^:private code-underline     "4")
(def ^:private code-blink         "5")
(def ^:private code-reverse       "7")
(def ^:private code-hidden        "8")
(def ^:private code-strikethrough "9")

;; ============================================================================
;; Built-in Elements
;; ============================================================================

;; --- Text ---
(defrecord TextElement [text]
  Element
  (render [_] text)
  (elem-width [_]
    (reduce max 0 (map visible-length (lines text))))
  (elem-height [_]
    (count (lines text))))

;; --- LineBreak ---
(defrecord LineBreakElement [n]
  Element
  (render [_] (apply str (repeat (dec n) "\n")))
  (elem-width [_] 0)
  (elem-height [_] n))

;; --- Space ---
(defrecord SpaceElement [n]
  Element
  (render [_] (apply str (repeat n \space)))
  (elem-width [_] n)
  (elem-height [_] 1))

;; --- Empty ---
(defrecord EmptyElement []
  Element
  (render [_] "")
  (elem-width [_] 0)
  (elem-height [_] 0))

;; --- HorizontalRule ---
(defrecord HorizontalRuleElement [ch rule-width]
  Element
  (render [_]
    (let [char-len (visible-length ch)]
      (if (zero? char-len)
        (apply str (repeat rule-width \space))
        (repeat-str (quot rule-width char-len) ch))))
  (elem-width [_] rule-width)
  (elem-height [_] 1))

;; --- VerticalRule ---
(defrecord VerticalRuleElement [ch rule-height]
  Element
  (render [_] (str/join "\n" (repeat rule-height ch)))
  (elem-width [_] (visible-length ch))
  (elem-height [_] rule-height))

;; --- Centered ---
(defrecord CenteredElement [inner target-width]
  Element
  (render [_]
    (unlines (map #(center-string target-width %) (lines (render inner)))))
  (elem-width [_] target-width)
  (elem-height [_] (elem-height inner)))

;; --- AutoCenter marker ---
(defrecord AutoCenterElement [inner]
  Element
  (render [_] (render inner))
  (elem-width [_] (elem-width inner))
  (elem-height [_] (elem-height inner)))

;; --- LeftAligned ---
(defrecord LeftAlignedElement [inner target-width]
  Element
  (render [_]
    (unlines (map #(pad-right target-width %) (lines (render inner)))))
  (elem-width [_] target-width)
  (elem-height [_] (elem-height inner)))

;; --- RightAligned ---
(defrecord RightAlignedElement [inner target-width]
  Element
  (render [_]
    (unlines (map #(pad-left target-width %) (lines (render inner)))))
  (elem-width [_] target-width)
  (elem-height [_] (elem-height inner)))

;; --- Truncated ---
(defrecord TruncatedElement [inner max-width ellipsis]
  Element
  (render [_]
    (let [truncate-line (fn [line]
                          (let [len (visible-length line)]
                            (if (<= len max-width) line
                                (let [elen (visible-length ellipsis)
                                      target (- max-width elen)]
                                  (if (<= target 0)
                                    (subs ellipsis 0 (min max-width (count ellipsis)))
                                    (str (subs (strip-ansi line) 0 target) ellipsis))))))]
      (unlines (map truncate-line (lines (render inner))))))
  (elem-width [_] (min max-width (elem-width inner)))
  (elem-height [_] (elem-height inner)))

;; --- Wrapped ---
(defrecord WrappedElement [inner max-width]
  Element
  (render [_]
    (let [wrap-line (fn [line]
                      (let [words (str/split line #" ")]
                        (loop [result [] current "" current-len 0 words words]
                          (if (empty? words)
                            (conj result current)
                            (let [word (first words)
                                  wlen (visible-length word)]
                              (if (= current "")
                                (recur result word wlen (rest words))
                                (if (<= (+ current-len 1 wlen) max-width)
                                  (recur result (str current " " word) (+ current-len 1 wlen) (rest words))
                                  (recur (conj result current) word wlen (rest words)))))))))]
      (unlines (mapcat wrap-line (lines (render inner))))))
  (elem-width [_] max-width)
  (elem-height [_]
    (count (lines (render (->WrappedElement inner max-width))))))

;; --- Justified ---
(defrecord JustifiedElement [inner target-width justify-last?]
  Element
  (render [_]
    (let [justify-line
          (fn [line]
            (let [words (filterv #(pos? (count %)) (str/split line #" "))]
              (case (count words)
                0 (apply str (repeat target-width \space))
                1 (pad-right target-width (first words))
                (let [total-word-len (reduce + (map visible-length words))
                      gaps           (dec (count words))
                      total-space    (- target-width total-word-len)]
                  (if (or (<= total-space 0) (<= gaps 0))
                    (str/join " " words)
                    (let [base-space (quot total-space gaps)
                          extra      (mod total-space gaps)]
                      (str/join ""
                                (map-indexed
                                 (fn [i w]
                                   (if (= i (dec (count words))) w
                                       (str w (apply str (repeat (+ base-space (if (< i extra) 1 0)) \space)))))
                                 words))))))))
          ls (lines (render inner))
          n  (count ls)]
      (unlines
       (map-indexed
        (fn [i line]
          (if (and (= i (dec n)) (not justify-last?))
            (pad-right target-width line)
            (justify-line line)))
        ls))))
  (elem-width [_] target-width)
  (elem-height [_] (elem-height inner)))

;; --- Padded ---
(defrecord PaddedElement [inner padding]
  Element
  (render [_]
    (let [content (render inner)
          content-lines (lines content)
          max-w   (reduce max 0 (map visible-length content-lines))
          h-pad   (apply str (repeat padding \space))
          total-w (+ max-w (* padding 2))
          v-pad   (apply str (repeat total-w \space))
          padded  (map #(str h-pad (pad-right max-w %) h-pad) content-lines)
          v-lines (repeat padding v-pad)]
      (unlines (concat v-lines padded v-lines))))
  (elem-width [_] (+ (elem-width inner) (* padding 2)))
  (elem-height [_] (+ (elem-height inner) (* padding 2))))

;; --- Margin ---
(defrecord MarginElement [inner prefix color]
  Element
  (render [_]
    (let [styled-prefix (if color
                          (str "\033[" (fg-code color) "m" prefix "\033[0m")
                          prefix)]
      (unlines (map #(str styled-prefix " " %) (lines (render inner))))))
  (elem-width [_] (+ (visible-length prefix) 1 (elem-width inner)))
  (elem-height [_] (elem-height inner)))

;; --- Underline ---
(defrecord UnderlineElement [inner ch color]
  Element
  (render [_]
    (let [content (render inner)
          w (elem-width inner)
          underline-str (repeat-str w ch)
          styled (if color
                   (str "\033[" (fg-code color) "m" underline-str "\033[0m")
                   underline-str)]
      (str content "\n" styled)))
  (elem-width [_] (elem-width inner))
  (elem-height [_] (inc (elem-height inner))))

;; --- Styled ---
(defrecord StyledElement [inner fg-color bg-color styles]
  Element
  (render [_]
    (let [codes (filterv some?
                         (concat
                          [(fg-code fg-color)]
                          [(bg-code bg-color)]
                          styles))
          content (render inner)]
      (if (empty? codes) content
          (let [start-code (str "\033[" (str/join ";" codes) "m")
                reset      "\033[0m"
                reapply (fn [line]
                          (str/replace line reset (str reset start-code)))]
            (unlines
             (map #(str start-code (reapply %) reset)
                  (lines content)))))))
  (elem-width [_] (elem-width inner))
  (elem-height [_] (elem-height inner)))

;; --- Bordered wrapper ---
(defrecord BorderedElement [inner border]
  Element
  (render [_]
    (let [content-str (render inner)
          content-lines (lines content-str)
          max-w (reduce max 0 (map visible-length content-lines))
          c (get-chars border)
          top (str (:tl c) (repeat-str (+ max-w 2) (:h-top c)) (:tr c))
          bot (str (:bl c) (repeat-str (+ max-w 2) (:h-bottom c)) (:br c))
          body (map #(str (:v-left c) " " (pad-right max-w %) " " (:v-right c))
                    content-lines)]
      (unlines (concat [top] body [bot]))))
  (elem-width [_] (+ (elem-width inner) 4))
  (elem-height [_] (+ (elem-height inner) 2))
  Borderable
  (with-border [this b] (assoc this :border b))
  (get-border [_] border))

;; --- Columns ---
(defrecord ColumnsElement [elements spacing]
  Element
  (render [_]
    (if (empty? elements) ""
        (let [rendered     (map render elements)
              elem-lines   (map lines rendered)
              max-height   (reduce max 0 (map count elem-lines))
              widths       (map (fn [ls] (reduce max 0 (map visible-length ls))) elem-lines)
              sep          (apply str (repeat spacing \space))
              padded       (map (fn [[w ls]]
                                  (let [plines (mapv #(pad-right w %) ls)
                                        missing (- max-height (count ls))]
                                    (into plines (repeat missing (apply str (repeat w \space))))))
                                (map vector widths elem-lines))
              padded-arr   (vec padded)]
          (unlines
           (for [row (range max-height)]
             (str/join sep (map #(nth % row) padded-arr)))))))
  (elem-width [_]
    (let [widths (map elem-width elements)
          total  (reduce + 0 widths)
          gaps   (* spacing (max 0 (dec (count elements))))]
      (+ total gaps)))
  (elem-height [_]
    (reduce max 0 (map elem-height elements))))

;; --- KeyValue ---
(defrecord KeyValueElement [pairs]
  Element
  (render [_]
    (if (empty? pairs) ""
        (let [max-key-len (reduce max 0 (map #(visible-length (first %)) pairs))
              align-pos   (+ max-key-len 2)]
          (unlines
           (map (fn [[k v]]
                  (let [kc (str k ":")
                        spaces (max 1 (- align-pos (visible-length kc)))]
                    (str kc (apply str (repeat spaces \space)) v)))
                pairs)))))
  (elem-width [_]
    (if (empty? pairs) 0
        (let [max-key (reduce max 0 (map #(visible-length (first %)) pairs))
              max-val (reduce max 0 (map #(visible-length (second %)) pairs))]
          (+ max-key 2 max-val))))
  (elem-height [_] (count pairs)))

;; --- InlineBar ---
(def ^:private bar-width 20)

(defrecord InlineBarElement [label progress]
  Element
  (render [_]
    (let [p      (max 0.0 (min 1.0 progress))
          filled (int (* p bar-width))
          empty  (- bar-width filled)
          bar    (str (repeat-str filled "█") (repeat-str empty "─"))
          pct    (int (* p 100.0))]
      (format "%s [%s] %d%%" label bar pct)))
  (elem-width [_] (+ (visible-length label) 2 bar-width 2 4))
  (elem-height [_] 1))

;; --- Chart (horizontal bar) ---
(def ^:private chart-label-cap 15)
(def ^:private chart-bar-width 40)
(def ^:private chart-value-col-width 10)

(defrecord ChartElement [data]
  Element
  (render [_]
    (if (empty? data) "No data"
        (let [max-value     (reduce max 0.0 (map second data))
              max-label-w   (min chart-label-cap
                                 (reduce max 0 (map #(count (first %)) data)))]
          (unlines
           (map (fn [[label value]]
                  (let [truncated (if (> (count label) max-label-w)
                                    (str (subs label 0 (- max-label-w 3)) "...")
                                    label)
                        padded    (pad-right max-label-w truncated)
                        pct       (if (zero? max-value) 0.0 (/ value max-value))
                        bar-len   (int (* pct chart-bar-width))
                        bar       (str (repeat-str bar-len "█")
                                       (repeat-str (- chart-bar-width bar-len) "─"))
                        value-str (if (= value (Math/floor value))
                                    (str (int value))
                                    (format "%.1f" value))]
                    (str padded " │" bar "│ " value-str)))
                data)))))
  (elem-width [_] (+ chart-label-cap 3 chart-bar-width chart-value-col-width))
  (elem-height [_] (max 1 (count data))))

;; --- Sparkline ---
(def ^:private block-chars [" " "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"])

(defrecord SparklineElement [values]
  Element
  (render [_]
    (if (empty? values) ""
        (let [mn  (reduce min values)
              mx  (reduce max values)
              rng (- mx mn)
              idx (fn [v]
                    (if (zero? rng) 4
                        (max 1 (min 8 (Math/round (double (* (/ (- v mn) rng) 8)))))))]
          (apply str (map #(nth block-chars (idx %)) values)))))
  (elem-width [_] (count values))
  (elem-height [_] 1))

;; --- Plot helpers ---

(defn- braille-dot [row col]
  (case [row col]
    [0 0] 0x01  [1 0] 0x02  [2 0] 0x04  [3 0] 0x40
    [0 1] 0x08  [1 1] 0x10  [2 1] 0x20  [3 1] 0x80
    0))

(def ^:private default-palette
  [bright-cyan bright-magenta bright-yellow bright-green bright-red bright-blue])

(defn- pick-color [idx color]
  (if (nil? color)
    (nth default-palette (mod idx (count default-palette)))
    color))

(defn- format-axis-num [v]
  (if (== v (Math/round (double v)))
    (str (long (Math/round (double v))))
    (format "%.1f" (double v))))

(defn- wrap-ansi-color [color s]
  (if-let [code (fg-code color)]
    (str "\033[" code "m" s "\033[0m")
    s))

(defn- bg-color-256 [n]
  (str "\033[48;5;" n "m"))

(def ^:private ansi-reset "\033[0m")

;; --- Line Plot (Braille) ---

(defrecord LinePlotElement [series w h]
  Element
  (render [_]
    (if (or (empty? series)
            (every? #(empty? (:points %)) series))
      "No data"
      (let [all-pts  (mapcat :points series)
            xs       (map first all-pts)
            ys       (map second all-pts)
            x-min    (reduce min xs) x-max (reduce max xs)
            y-min    (reduce min ys) y-max (reduce max ys)
            x-rng    (if (== x-max x-min) 1.0 (- x-max x-min))
            y-rng    (if (== y-max y-min) 1.0 (- y-max y-min))
            px-w     (* w 2) px-h (* h 4)
            to-pixel (fn [[x y]]
                       [(max 0 (min (dec px-w) (Math/round (double (* (/ (- x x-min) x-rng) (dec px-w))))))
                        (max 0 (min (dec px-h) (Math/round (double (* (/ (- y-max y) y-rng) (dec px-h))))))])
            empty-grid (vec (repeat h (vec (repeat w [0 -1]))))
            plot-series (fn [grid s-idx s]
                          (reduce (fn [g pt]
                                    (let [[px py] (to-pixel pt)
                                          cx (quot px 2) cy (quot py 4)
                                          dx (rem px 2) dy (rem py 4)
                                          bit (braille-dot dy dx)]
                                      (update-in g [cy cx]
                                                 (fn [[b si]]
                                                   [(bit-or b bit)
                                                    (if (neg? si) s-idx si)]))))
                                  grid (:points s)))
            grid (reduce-kv (fn [g i s] (plot-series g i s))
                            empty-grid (vec series))
            y-ticks  (mapv #(- y-max (* y-rng (/ (double %) (max 1 (dec h)))))
                           (range h))
            y-labels (mapv format-axis-num y-ticks)
            y-label-w (reduce max 0 (map visible-length y-labels))
            grid-lines (mapv (fn [row-idx]
                               (str (pad-left y-label-w (nth y-labels row-idx))
                                    " │"
                                    (apply str
                                           (map (fn [[bits si]]
                                                  (let [ch (if (zero? bits) " "
                                                               (str (char (+ 0x2800 bits))))]
                                                    (if (and (not (zero? bits)) (>= si 0))
                                                      (wrap-ansi-color
                                                       (pick-color si (:color (nth series si)))
                                                       ch)
                                                      ch)))
                                                (nth grid row-idx)))))
                             (range h))
            x-axis   (str (apply str (repeat (+ y-label-w 1) \space))
                          "└" (apply str (repeat w \─)))
            x-min-l  (format-axis-num x-min)
            x-max-l  (format-axis-num x-max)
            x-labels (str (apply str (repeat (+ y-label-w 2) \space))  ;; after └
                          x-min-l
                          (apply str (repeat (max 1 (- w (count x-min-l) (count x-max-l))) \space))
                          x-max-l)
            legend   (if (<= (count series) 1) []
                         ["" (str/join "  "
                                       (map-indexed
                                        (fn [i s]
                                          (str (wrap-ansi-color (pick-color i (:color s)) "●")
                                               " " (:label s)))
                                        series))])]
        (str/join "\n" (concat grid-lines [x-axis x-labels] legend)))))
  (elem-width [_] (+ 8 w))
  (elem-height [_] (+ h 2 (if (> (count series) 1) 2 0))))

;; --- Pie Chart (Braille) ---

(defrecord PieElement [slices w h]
  Element
  (render [_]
    (if (empty? slices) "No data"
        (let [total    (reduce + (map :value slices))
              cum-angs (reductions + 0 (map #(* (/ (:value %) total) 2 Math/PI) slices))
              cx-f     (double w)
              cy-f     (/ (* (double h) 4) 2.0)
              radius   (* (min cx-f (* cy-f 0.9)) 1.0)
              find-slice (fn [ang]
                           (loop [i 0 angs (rest cum-angs)]
                             (if (empty? angs) (max 0 (dec i))
                                 (if (< ang (first angs)) i
                                     (recur (inc i) (rest angs))))))
              render-cell (fn [gcx gcy]
                            (let [sub-px (for [dy (range 4) dx (range 2)]
                                           (let [dpx (+ (* gcx 2) dx)
                                                 dpy (+ (* gcy 4) dy)
                                                 rel-x (- (double dpx) cx-f)
                                                 rel-y (* (- (double dpy) cy-f) 2.0)
                                                 dist  (Math/sqrt (+ (* rel-x rel-x) (* rel-y rel-y)))
                                                 ang   (Math/atan2 rel-y rel-x)
                                                 n-ang (if (neg? ang) (+ ang (* 2 Math/PI)) ang)]
                                             [dy dx dist n-ang]))
                                  inside (filter (fn [[_ _ dist _]] (<= dist radius)) sub-px)
                                  bits   (reduce (fn [acc [dy dx _ _]]
                                                   (bit-or acc (braille-dot dy dx)))
                                                 0 inside)
                                  dom-si (if (empty? inside) -1
                                             (find-slice (nth (first inside) 3)))
                                  ch     (if (zero? bits) " " (str (char (+ 0x2800 bits))))
                                  color  (when (and (>= dom-si 0) (< dom-si (count slices)))
                                           (pick-color dom-si (:color (nth slices dom-si))))]
                              (if (zero? bits) " "
                                  (wrap-ansi-color color ch))))
              grid-lines (mapv (fn [gcy]
                                 (apply str (map #(render-cell % gcy) (range w))))
                               (range h))
              legend (mapv (fn [i]
                             (let [sl (nth slices i)
                                   c  (pick-color i (:color sl))
                                   pct (format "%.0f" (double (* (/ (:value sl) total) 100)))]
                               (str "  " (wrap-ansi-color c "●") " " (:label sl) " (" pct "%)")))
                           (range (count slices)))]
          (str/join "\n" (concat grid-lines [""] legend)))))
  (elem-width [_] (max w (reduce max 0 (map #(+ 6 (count (:label %))) slices))))
  (elem-height [_] (+ h 1 (count slices))))

;; --- Bar Chart (Vertical) ---

(defrecord BarChartElement [items w h]
  Element
  (render [_]
    (if (empty? items) "No data"
        (let [max-val   (reduce max (map :value items))
              n-bars    (count items)
              bar-w     (max 1 (quot (- w (dec n-bars)) n-bars))
              total-sub (* h 8)
              bar-hts   (mapv #(Math/round (double (* (/ (:value %) max-val) total-sub))) items)
              y-ticks   (mapv #(* max-val (/ (double (- (dec h) %)) (max 1 (dec h))))
                              (range h))
              y-labels  (mapv format-axis-num y-ticks)
              y-label-w (reduce max 0 (map visible-length y-labels))
              grid-lines (mapv
                          (fn [row-idx]
                            (let [r (- (dec h) row-idx)
                                  bar-cells
                                  (str/join " "
                                            (map-indexed
                                             (fn [i item]
                                               (let [bh      (nth bar-hts i)
                                                     filled  (min 8 (max 0 (- bh (* r 8))))
                                                     color   (pick-color i (:color item))
                                                     bar-str (apply str (repeat bar-w (nth block-chars filled)))]
                                                 (if (pos? filled)
                                                   (wrap-ansi-color color bar-str)
                                                   bar-str)))
                                             items))]
                              (str (pad-left y-label-w (nth y-labels row-idx))
                                   " │" bar-cells)))
                          (range h))
              x-axis-w  (+ (* n-bars bar-w) (dec n-bars))
              x-axis    (str (apply str (repeat (+ y-label-w 1) \space))
                             "└" (apply str (repeat x-axis-w \─)))
              bar-labels (str (apply str (repeat (+ y-label-w 2) \space))
                              (str/join " " (map #(subs (str (:label %) (apply str (repeat bar-w \space)))
                                                        0 bar-w)
                                                 items)))]
          (str/join "\n" (concat grid-lines [x-axis bar-labels])))))
  (elem-width [_] w)
  (elem-height [_] (+ h 2)))

;; --- Stacked Bar Chart ---

(defrecord StackedBarElement [groups w h]
  Element
  (render [_]
    (if (empty? groups) "No data"
        (let [max-total (reduce max (map (fn [g] (reduce + (map :value (:segments g)))) groups))
              n-groups  (count groups)
              bar-w     (max 1 (quot (- w (dec n-groups)) n-groups))
              total-sub (* h 8)
              group-bounds
              (mapv (fn [g]
                      (let [segs    (:segments g)
                            sub-hts (mapv #(Math/round (double (* (/ (:value %) max-total) total-sub))) segs)
                            cum-hts (reductions + 0 sub-hts)
                            bottoms (butlast cum-hts)
                            tops    (rest cum-hts)]
                        (mapv vector segs bottoms tops)))
                    groups)
              all-labels (distinct (mapcat (fn [g] (map :label (:segments g))) groups))
              label-idx  (into {} (map-indexed (fn [i l] [l i]) all-labels))
              y-ticks    (mapv #(* max-total (/ (double (- (dec h) %)) (max 1 (dec h))))
                               (range h))
              y-labels   (mapv format-axis-num y-ticks)
              y-label-w  (reduce max 0 (map count y-labels))
              grid-lines
              (mapv
               (fn [row-idx]
                 (let [r       (- (dec h) row-idx)
                       sub-bot (* r 8)
                       sub-top (+ sub-bot 8)
                       bar-cells
                       (str/join " "
                                 (map (fn [bounds]
                                        (let [overlapping (filter (fn [[_ bot top]]
                                                                    (and (> top sub-bot) (< bot sub-top)))
                                                                  bounds)
                                              top-seg (when (seq overlapping)
                                                        (reduce (fn [a b] (if (> (nth b 2) (nth a 2)) b a))
                                                                overlapping))]
                                          (if (nil? top-seg)
                                            (apply str (repeat bar-w \space))
                                            (let [[item _ top] top-seg
                                                  filled (min 8 (max 0 (- top sub-bot)))
                                                  color  (if (:color item)
                                                           (:color item)
                                                           (pick-color (get label-idx (:label item) 0) nil))
                                                  bar-str (apply str (repeat bar-w (nth block-chars filled)))]
                                              (if (pos? filled)
                                                (wrap-ansi-color color bar-str)
                                                bar-str)))))
                                      group-bounds))]
                   (str (pad-left y-label-w (nth y-labels row-idx))
                        " │" bar-cells)))
               (range h))
              x-axis-w   (+ (* n-groups bar-w) (dec n-groups))
              x-axis     (str (apply str (repeat (+ y-label-w 1) \space))
                              "└" (apply str (repeat x-axis-w \─)))
              grp-labels (str (apply str (repeat (+ y-label-w 2) \space))
                              (str/join " " (map #(subs (str (:group-label %)
                                                             (apply str (repeat bar-w \space)))
                                                        0 bar-w)
                                                 groups)))
              label-colors (into {} (for [g groups
                                          seg (:segments g)
                                          :when (:color seg)]
                                      [(:label seg) (:color seg)]))
              legend-items (map (fn [nm]
                                  (let [i (get label-idx nm 0)
                                        c (or (get label-colors nm)
                                              (nth default-palette (mod i (count default-palette))))]
                                    (str (wrap-ansi-color c "█") " " nm)))
                                all-labels)
              legend (if (<= (count all-labels) 1) []
                         ["" (str/join "  " legend-items)])]
          (str/join "\n" (concat grid-lines [x-axis grp-labels] legend)))))
  (elem-width [_] w)
  (elem-height [_] (+ h 2 (if (> (count (distinct (mapcat (fn [g] (map :label (:segments g))) groups))) 1)
                            2 0))))

;; --- Heatmap ---

(defrecord HeatmapElement [grid-data row-labels col-labels cell-w]
  Element
  (render [_]
    (if (empty? grid-data) "No data"
        (let [all-vals  (flatten grid-data)
              mn        (reduce min all-vals)
              mx        (reduce max all-vals)
              rng       (if (== mx mn) 1.0 (- mx mn))
              normalize (fn [v] (/ (- v mn) rng))
              to-color  (fn [t]
                          (cond
                            (<= t 0.0)  21
                            (>= t 1.0)  196
                            (< t 0.25)  (Math/round (+ 21.0 (* (/ t 0.25) 30.0)))
                            (< t 0.5)   (Math/round (+ 51.0 (* (/ (- t 0.25) 0.25) -5.0)))
                            (< t 0.75)  (Math/round (+ 46.0 (* (/ (- t 0.5) 0.25) 180.0)))
                            :else        (Math/round (+ 226.0 (* (/ (- t 0.75) 0.25) -30.0)))))
              row-lbl-w (reduce max 0 (map count row-labels))
              header    (str (apply str (repeat (inc row-lbl-w) \space))
                             (str/join " " (map #(pad-right cell-w (subs % 0 (min (count %) cell-w)))
                                                col-labels)))
              data-rows (map-indexed
                         (fn [i row-vals]
                           (str (pad-right row-lbl-w
                                           (subs (nth row-labels i "")
                                                 0 (min (count (nth row-labels i "")) row-lbl-w)))
                                " "
                                (str/join " "
                                          (map (fn [v]
                                                 (let [n    (normalize v)
                                                       c    (to-color n)
                                                       vs   (format-axis-num v)]
                                                   (str (bg-color-256 c)
                                                        (pad-right cell-w (subs vs 0 (min (count vs) cell-w)))
                                                        ansi-reset)))
                                               row-vals))))
                         grid-data)
              legend-cs  (mapv #(to-color (/ (double %) 10.0)) (range 11))
              legend-bar (apply str (map #(str (bg-color-256 %) " " ansi-reset) legend-cs))
              legend     (str (apply str (repeat (inc row-lbl-w) \space))
                              (format-axis-num mn) " " legend-bar " " (format-axis-num mx))]
          (str/join "\n" (concat [header] data-rows ["" legend])))))
  (elem-width [_]
    (let [row-lbl-w (reduce max 0 (map count row-labels))]
      (+ row-lbl-w 1 (* (count col-labels) (inc cell-w)))))
  (elem-height [_] (+ 1 (count grid-data) 2)))

;; --- Spinner ---
(def spinner-frames
  {:dots    ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
   :line    ["|" "/" "-" "\\"]
   :clock   ["🕐" "🕑" "🕒" "🕓" "🕔" "🕕" "🕖" "🕗" "🕘" "🕙" "🕚" "🕛"]
   :bounce  ["⠁" "⠂" "⠄" "⠂"]
   :earth   ["🌍" "🌎" "🌏"]
   :moon    ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"]
   :grow    ["▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"]
   :arrow   ["←" "↖" "↑" "↗" "→" "↘" "↓" "↙"]})

(defrecord SpinnerElement [label frame style]
  Element
  (render [_]
    (let [frames (get spinner-frames style)
          spin   (nth frames (mod frame (count frames)))]
      (if (= label "") spin (str spin " " label))))
  (elem-width [_]
    (let [frames (get spinner-frames style)
          sw     (visible-length (first frames))]
      (if (= label "") sw (+ sw 1 (visible-length label)))))
  (elem-height [_] 1))

;; --- Section ---
(defrecord SectionElement [title glyph flanking content]
  Element
  (render [_]
    (let [header (str (repeat-str flanking glyph)
                      " " title " "
                      (repeat-str flanking glyph))
          body   (str/join "\n" (map render content))]
      (str header "\n" body)))
  (elem-width [_]
    (let [header-w (+ (* flanking 2) 2 (visible-length title))
          content-w (reduce max 0 (map elem-width content))]
      (max header-w content-w)))
  (elem-height [_]
    (+ 1 (reduce + (map elem-height content)))))

;; --- VStack ---
(defrecord VStackElement [elements]
  Element
  (render [_]
    (let [layout-width (reduce max 0 (map elem-width elements))
          resolve (fn [e]
                    (if (instance? AutoCenterElement e)
                      (let [content (render (:inner e))
                            ls (lines content)]
                        (unlines (map #(center-string layout-width %) ls)))
                      (render e)))]
      (str/join "\n" (map resolve elements))))
  (elem-width [_]
    (reduce max 0 (map elem-width elements)))
  (elem-height [_]
    (reduce + (map elem-height elements))))

;; --- HStack ---
(defrecord HStackElement [elements tight?]
  Element
  (render [_]
    (if (empty? elements) ""
        (let [rendered     (map render elements)
              elem-lines   (map lines rendered)
              max-height   (reduce max 0 (map count elem-lines))
              widths       (map (fn [ls] (reduce max 0 (map visible-length ls))) elem-lines)
              separator    (if tight? "" " ")
              padded       (map (fn [[w ls]]
                                  (let [plines (mapv #(pad-right w %) ls)
                                        missing (- max-height (count ls))]
                                    (into plines (repeat missing (apply str (repeat w \space))))))
                                (map vector widths elem-lines))
              padded-arr   (vec padded)]
          (unlines
           (for [row (range max-height)]
             (str/join separator (map #(nth % row) padded-arr)))))))
  (elem-width [_]
    (let [widths (map elem-width elements)
          total  (reduce + 0 widths)
          gaps   (if tight? 0 (max 0 (dec (count elements))))]
      (+ total gaps)))
  (elem-height [_]
    (reduce max 0 (map elem-height elements))))

;; --- Box ---
(defrecord BoxElement [title elements border]
  Element
  (render [_]
    (let [content (str/join "\n" (map render elements))
          content-lines (if (= content "") [""] (lines content))
          content-w   (reduce max 0 (map visible-length content-lines))
          title-w     (if (= title "") 0 (+ (visible-length title) 2))
          inner-w     (max content-w title-w)
          total-w     (+ inner-w 4)
          c           (get-chars border)
          top-border  (if (= title "")
                        (str (:tl c) (repeat-str (- total-w 2) (:h-top c)) (:tr c))
                        (let [tp      (- total-w (visible-length title) 2)
                              left-p  (quot tp 2)
                              right-p (- tp left-p)]
                          (str (:tl c)
                               (repeat-str left-p (:h-top c))
                               title
                               (repeat-str right-p (:h-top c))
                               (:tr c))))
          bot-border  (str (:bl c) (repeat-str (- total-w 2) (:h-bottom c)) (:br c))
          body        (map #(str (:v-left c) " " (pad-right inner-w %) " " (:v-right c))
                           content-lines)]
      (unlines (concat [top-border] body [bot-border]))))
  (elem-width [_]
    (let [content (str/join "\n" (map render elements))
          content-lines (if (= content "") [""] (lines content))
          content-w   (reduce max 0 (map visible-length content-lines))
          title-w     (if (= title "") 0 (+ (visible-length title) 2))]
      (+ (max content-w title-w) 4)))
  (elem-height [_]
    (let [content (str/join "\n" (map render elements))
          content-lines (if (= content "") [""] (lines content))]
      (+ (count content-lines) 2)))
  Borderable
  (with-border [this b] (assoc this :border b))
  (get-border [_] border))

;; --- Banner ---
(defrecord BannerElement [content border]
  Element
  (render [_]
    (let [content-str (render content)
          content-lines (lines content-str)
          max-w      (reduce max 0 (map visible-length content-lines))
          inner-w    (+ max-w 4)
          c          (get-chars border)
          top        (str (:tl c) (repeat-str inner-w (:h-top c)) (:tr c))
          bot        (str (:bl c) (repeat-str inner-w (:h-bottom c)) (:br c))
          empty-line (str (:v-left c) (apply str (repeat inner-w \space)) (:v-right c))
          body       (map #(str (:v-left c) "  " (pad-right max-w %) "  " (:v-right c))
                          content-lines)]
      (unlines (concat [top empty-line] body [empty-line bot]))))
  (elem-width [_] (+ (elem-width content) 8))
  (elem-height [_] (+ (elem-height content) 4))
  Borderable
  (with-border [this b] (assoc this :border b))
  (get-border [_] border))

;; --- Table ---
(defrecord TableElement [headers rows border]
  Element
  (render [_]
    (let [num-cols      (count headers)
          normalize-row (fn [row]
                          (let [len (count row)]
                            (if (>= len num-cols)
                              (take num-cols row)
                              (concat row (repeat (- num-cols len) (->TextElement ""))))))
          header-strs   (map render headers)
          rendered-rows (map (fn [row] (map render (normalize-row row))) rows)
          col-widths    (let [hw (mapv visible-length header-strs)]
                          (reduce (fn [widths row]
                                    (mapv (fn [w cell]
                                            (max w (reduce max 0 (map visible-length (lines cell)))))
                                          widths row))
                                  hw rendered-rows))
          c             (get-chars border)
          make-border   (fn [left h conn right]
                          (let [parts (map #(repeat-str % h) col-widths)]
                            (str left h (str/join (str h conn h) parts) h right)))
          top-border    (make-border (:tl c) (:h-top c) (:top-tee c) (:tr c))
          sep-border    (make-border (:left-tee c) (:h-top c) (:cross c) (:right-tee c))
          bot-border    (make-border (:bl c) (:h-bottom c) (:bottom-tee c) (:br c))
          make-row      (fn [cells]
                          (let [padded (map pad-right col-widths cells)]
                            (str (:v-left c) " "
                                 (str/join (str " " (:v-left c) " ") padded)
                                 " " (:v-right c))))
          header-row    (make-row header-strs)
          data-rows     (mapcat
                         (fn [row]
                           (let [cell-lines (map lines row)
                                 max-h      (reduce max 1 (map count cell-lines))
                                 padded-cells (map (fn [w cls]
                                                     (let [plines (mapv #(pad-right w %) cls)
                                                           missing (- max-h (count cls))]
                                                       (into plines (repeat missing (apply str (repeat w \space))))))
                                                   col-widths cell-lines)]
                             (for [r (range max-h)]
                               (let [row-cells (map #(nth % r) padded-cells)]
                                 (str (:v-left c) " "
                                      (str/join (str " " (:v-left c) " ") row-cells)
                                      " " (:v-right c))))))
                         rendered-rows)]
      (unlines (concat [top-border header-row sep-border] data-rows [bot-border]))))
  (elem-width [_]
    (let [num-cols      (count headers)
          normalize-row (fn [row]
                          (let [len (count row)]
                            (if (>= len num-cols)
                              (take num-cols row)
                              (concat row (repeat (- num-cols len) (->TextElement ""))))))
          header-strs   (map render headers)
          rendered-rows (map (fn [row] (map render (normalize-row row))) rows)
          col-widths    (let [hw (mapv visible-length header-strs)]
                          (reduce (fn [widths row]
                                    (mapv (fn [w cell]
                                            (max w (reduce max 0 (map visible-length (lines cell)))))
                                          widths row))
                                  hw rendered-rows))
          total         (reduce + col-widths)]
      (+ total (* (count col-widths) 3) 1)))
  (elem-height [_]
    (+ 3 (reduce + (map (fn [row]
                          (reduce max 1 (map elem-height row)))
                        rows))))
  Borderable
  (with-border [this b] (assoc this :border b))
  (get-border [_] border))

;; --- UList ---
(defrecord UListElement [items bullet]
  Element
  (render [_]
    (letfn [(render-items [items indent]
              (let [bullet-w (inc (visible-length bullet))]
                (str/join "\n"
                          (map (fn [{:keys [content children]}]
                                 (let [rendered    (render content)
                                       item-lines  (lines rendered)
                                       prefix      (apply str (repeat indent \space))
                                       first-line  (str prefix bullet " " (first item-lines))
                                       cont-indent (apply str (repeat (+ indent bullet-w) \space))
                                       rest-lines  (map #(str cont-indent %) (rest item-lines))
                                       item-str    (unlines (cons first-line rest-lines))]
                                   (if (empty? children) item-str
                                       (str item-str "\n"
                                            (render-items children (+ indent 2))))))
                               items))))]
      (render-items items 0)))
  (elem-width [_]
    (let [bw (inc (visible-length bullet))]
      (+ bw (reduce max 0 (map #(elem-width (:content %)) items)))))
  (elem-height [_]
    (reduce + (map #(+ (elem-height (:content %)) (count (:children %))) items))))

;; --- OList ---
(defrecord OListElement [items start]
  Element
  (render [_]
    (letfn [(to-letter [n]
              (loop [acc "" n n]
                (if (neg? n) acc
                    (recur (str (char (+ 97 (mod n 26))) acc) (dec (quot n 26))))))
            (to-roman [n]
              (let [numerals [[1000 "m"] [900 "cm"] [500 "d"] [400 "cd"]
                              [100 "c"] [90 "xc"] [50 "l"] [40 "xl"]
                              [10 "x"] [9 "ix"] [5 "v"] [4 "iv"] [1 "i"]]]
                (loop [acc "" n n nums numerals]
                  (if (or (empty? nums) (<= n 0)) acc
                      (let [[v s] (first nums)]
                        (if (>= n v)
                          (recur (str acc s) (- n v) nums)
                          (recur acc n (rest nums))))))))
            (format-num [level idx]
              (let [n (+ start idx)]
                (case (mod level 3)
                  0 (str n)
                  1 (to-letter (dec n))
                  2 (to-roman n))))
            (render-items [items prefix-indent level st]
              (let [sample-nums (map-indexed (fn [i _] (format-num level i)) items)
                    num-w       (reduce max 0 (map count sample-nums))]
                (str/join "\n"
                          (map-indexed
                           (fn [idx {:keys [content children]}]
                             (let [num-str    (format-num level idx)
                                   padded-num (str (apply str (repeat (- num-w (count num-str)) \space)) num-str)
                                   prefix     (str padded-num ". ")
                                   prefix-w   (+ num-w 2)
                                   rendered   (render content)
                                   item-lines (lines rendered)
                                   indent-str (apply str (repeat prefix-indent \space))
                                   first-line (str indent-str prefix (first item-lines))
                                   cont-indent (apply str (repeat (+ prefix-indent prefix-w) \space))
                                   rest-lines (map #(str cont-indent %) (rest item-lines))
                                   item-str   (unlines (cons first-line rest-lines))]
                               (if (empty? children) item-str
                                   (str item-str "\n"
                                        (render-items children (+ prefix-indent prefix-w) (inc level) 1)))))
                           items))))]
      (render-items items 0 0 start)))
  (elem-width [_]
    (let [num-w (+ (count (str (+ start (dec (count items))))) 2)]
      (+ num-w (reduce max 0 (map #(elem-width (:content %)) items)))))
  (elem-height [_]
    (reduce + (map #(+ (elem-height (:content %)) (count (:children %))) items))))

;; --- Tree ---
(defrecord TreeElement [root]
  Element
  (render [_]
    (letfn [(render-node [prefix is-last node]
              (let [connector   (if is-last "└── " "├── ")
                    rendered    (render (:label node))
                    label-lines (lines rendered)
                    first-line  (str prefix connector (first label-lines))
                    child-prefix (str prefix (if is-last "    " "│   "))
                    rest-lines  (map #(str child-prefix %) (rest label-lines))
                    num-children (count (:branches node))
                    children    (str/join "\n"
                                          (map-indexed
                                           (fn [i child]
                                             (render-node child-prefix (= i (dec num-children)) child))
                                           (:branches node)))]
                (str/join "\n" (filterv #(not= % "")
                                        (concat [first-line] rest-lines [children])))))]
      (let [rendered    (render (:label root))
            label-lines (lines rendered)
            root-line   (first label-lines)
            rest-lines  (rest label-lines)
            num-children (count (:branches root))
            children    (str/join "\n"
                                  (map-indexed
                                   (fn [i child]
                                     (render-node "" (= i (dec num-children)) child))
                                   (:branches root)))]
        (str/join "\n" (filterv #(not= % "")
                                (concat [root-line] rest-lines [children]))))))
  (elem-width [_]
    (letfn [(node-width [node]
              (let [label-w  (elem-width (:label node))
                    child-w  (reduce max 0 (map #(+ 4 (node-width %)) (:branches node)))]
                (max label-w child-w)))]
      (node-width root)))
  (elem-height [_]
    (letfn [(node-height [node]
              (+ 1 (reduce + (map node-height (:branches node)))))]
      (node-height root))))

;; ============================================================================
;; Smart Constructors (DSL)
;; ============================================================================

(defn s
  [text]
  (->TextElement text))

(defn text
  [t]
  (->TextElement t))

(def br
  "Line break."
  (->LineBreakElement 1))

(defn br'
  "Multiple line breaks."
  [n]
  (->LineBreakElement n))

(def space
  "Single space."
  (->SpaceElement 1))

(defn space'
  "Multiple spaces."
  [n]
  (->SpaceElement n))

(def empty-elem
  "Empty element."
  (->EmptyElement))

(def hr
  "Horizontal rule (default: 50 wide)."
  (->HorizontalRuleElement "─" 50))

(defn hr'
  "Horizontal rule with options."
  [& {:keys [char width] :or {char "─" width 50}}]
  (->HorizontalRuleElement char width))

(defn vr
  "Vertical rule."
  [& {:keys [char height] :or {char "│" height 10}}]
  (->VerticalRuleElement char height))

(defn center
  "Center element - auto-centers in layout, or specify width."
  ([inner]       (->AutoCenterElement inner))
  ([width inner] (->CenteredElement inner width)))

(defn left-align
  "Left-align element within width."
  [width inner]
  (->LeftAlignedElement inner width))

(defn right-align
  "Right-align element within width."
  [width inner]
  (->RightAlignedElement inner width))

(defn pad
  "Add padding around element."
  [padding inner]
  (->PaddedElement inner padding))

(defn margin
  "Add prefix margin to element."
  [prefix inner]
  (->MarginElement inner prefix nil))

(defn margin-color
  "Add colored prefix margin to element."
  [prefix color inner]
  (->MarginElement inner prefix color))

(defn truncate
  "Truncate element to max width."
  [max-width inner & {:keys [ellipsis] :or {ellipsis "..."}}]
  (->TruncatedElement inner max-width ellipsis))

(defn wrap
  "Word wrap element to max width."
  [max-width inner]
  (->WrappedElement inner max-width))

(defn justify
  "Justify text to width."
  [width inner]
  (->JustifiedElement inner width false))

(defn justify-all
  "Justify all lines including last."
  [width inner]
  (->JustifiedElement inner width true))

(defn underline-elem
  "Underline element."
  ([inner]    (->UnderlineElement inner "─" nil))
  ([ch inner] (->UnderlineElement inner ch nil)))

(defn underline-colored
  "Colored underline."
  [inner ch color]
  (->UnderlineElement inner ch color))

(defn columns
  "Multi-column layout."
  ([elements]         (->ColumnsElement elements 2))
  ([spacing elements] (->ColumnsElement elements spacing)))

(defn status-card
  "Status card - bordered box with label and content inside."
  [label content]
  (->BoxElement "" [label content] :normal))

(defn banner
  "Banner with decorative border."
  [content]
  (->BannerElement content :double))

(defn kv
  "Key-value pairs. Accepts a seq of [k v] pairs or a map."
  [pairs]
  (->KeyValueElement (if (map? pairs) (vec pairs) pairs)))

(defn inline-bar
  "Inline progress bar."
  [label progress]
  (->InlineBarElement label progress))

(defn chart
  "Horizontal bar chart."
  [data]
  (->ChartElement data))

(defn spinner
  "Spinner - animated loading indicator.
   (spinner label frame style) or (spinner label style frame)"
  [label a b]
  (if (keyword? a)
    (->SpinnerElement label b a)
    (->SpinnerElement label a b)))

(defn sparkline
  "Sparkline - inline spark chart from values."
  [values]
  (->SparklineElement values))

(defn series
  "Create a data series for line plots.
   points is a seq of [x y] pairs."
  ([points label]       {:points points :label label :color nil})
  ([points label color] {:points points :label label :color color}))

(defn plot-line
  "Braille line plot. series is a seq of (series ...) values."
  [w h series-data]
  (->LinePlotElement series-data w h))

(defn slice
  "Create a pie slice."
  ([value label]       {:value value :label label :color nil})
  ([value label color] {:value value :label label :color color}))

(defn plot-pie
  "Braille pie chart."
  [w h slices]
  (->PieElement slices w h))

(defn bar-item
  "Create a bar item."
  ([value label]       {:value value :label label :color nil})
  ([value label color] {:value value :label label :color color}))

(defn plot-bar
  "Vertical bar chart."
  [w h items]
  (->BarChartElement items w h))

(defn stacked-bar-group
  "Create a stacked bar group."
  [group-label segments]
  {:group-label group-label :segments segments})

(defn plot-stacked-bar
  "Stacked vertical bar chart."
  [w h groups]
  (->StackedBarElement groups w h))

(defn heatmap-data
  "Create heatmap data."
  [grid row-labels col-labels]
  {:grid grid :row-labels row-labels :col-labels col-labels})

(defn plot-heatmap
  "Heatmap with default cell width (6)."
  ([data]
   (->HeatmapElement (:grid data) (:row-labels data) (:col-labels data) 6))
  ([cell-w data]
   (->HeatmapElement (:grid data) (:row-labels data) (:col-labels data) cell-w)))

(defn section
  "Section with title and content. Content can be a vector or a single element."
  [title content & {:keys [glyph flanking] :or {glyph "=" flanking 3}}]
  (->SectionElement title glyph flanking (if (sequential? content) content [content])))

(defn layout
  "Vertical layout."
  [elements]
  (->VStackElement elements))

(defn row
  "Horizontal row."
  ([elements]       (->HStackElement elements false))
  ([tight? elements] (->HStackElement elements tight?)))

(defn tight-row
  "Tight row - horizontal arrangement without gaps."
  [elements]
  (->HStackElement elements true))

(defn box
  "Bordered box. Elements can be a vector or a single element."
  [title elements]
  (->BoxElement title (if (sequential? elements) elements [elements]) :normal))

(defn table
  "Table with element headers and rows."
  [headers rows]
  (->TableElement headers rows :normal))

(defn li
  "List item - optionally with nested children via :c."
  ([item]   {:content item :children []})
  ([item & {:keys [c] :or {c []}}] {:content item :children c}))

(defn- normalize-li [item]
  (if (and (map? item) (contains? item :content))
    item
    {:content item :children []}))

(defn ul
  "Unordered list. Items can be li maps or plain strings/elements (auto-wrapped)."
  ([items]        (->UListElement (mapv normalize-li items) "•"))
  ([bullet items] (->UListElement (mapv normalize-li items) bullet)))

(defn ol
  "Ordered list. Items can be li maps or plain strings/elements (auto-wrapped)."
  ([items]       (->OListElement (mapv normalize-li items) 1))
  ([start items] (->OListElement (mapv normalize-li items) start)))

(defn node
  "Tree node constructor."
  ([label]   {:label label :branches []})
  ([label & {:keys [c] :or {c []}}] {:label label :branches c}))

(defn tree
  "Tree element."
  [root]
  (->TreeElement root))

;; ============================================================================
;; Border Functions (pipe-friendly, element last)
;; ============================================================================

(defn set-border [border elem]
  (cond
    (satisfies? Borderable elem) (with-border elem border)
    (instance? BorderedElement elem) (assoc elem :border border)
    :else (->BorderedElement elem border)))

(defn border-normal [e]           (set-border :normal e))
(defn border-double [e]           (set-border :double e))
(defn border-thick [e]            (set-border :thick e))
(defn border-round [e]            (set-border :round e))
(defn border-ascii [e]            (set-border :ascii e))
(defn border-block [e]            (set-border :block e))
(defn border-dashed [e]           (set-border :dashed e))
(defn border-dotted [e]           (set-border :dotted e))
(defn border-inner-half-block [e] (set-border :inner-half-block e))
(defn border-outer-half-block [e] (set-border :outer-half-block e))
(defn border-markdown [e]         (set-border :markdown e))
(defn border-custom
  "Custom border. Returns a 1-arg fn, or applies directly when threaded."
  ([corner h v]
   (fn [e] (set-border {:corner corner :h h :v v} e)))
  ([e corner h v]
   (set-border {:corner corner :h h :v v} e)))

(defn border-none [e]
  (cond
    (satisfies? Borderable e) (with-border e :none)
    (instance? BorderedElement e) (:inner e)
    :else e))

;; ============================================================================
;; Styling Functions (pipe-friendly, element last)
;; ============================================================================

(defn with-style
  "Apply fg, bg, and/or styles to an element."
  [inner & {:keys [fg bg style] :or {fg nil bg nil style []}}]
  (->StyledElement inner fg bg style))

(defn fg-color [color e] (->StyledElement e color nil []))
(defn bg-color [color e] (->StyledElement e nil color []))

;; Foreground color functions
(defn color-black [e]          (fg-color black e))
(defn color-red [e]            (fg-color red e))
(defn color-green [e]          (fg-color green e))
(defn color-yellow [e]         (fg-color yellow e))
(defn color-blue [e]           (fg-color blue e))
(defn color-magenta [e]        (fg-color magenta e))
(defn color-cyan [e]           (fg-color cyan e))
(defn color-white [e]          (fg-color white e))
(defn color-bright-black [e]   (fg-color bright-black e))
(defn color-bright-red [e]     (fg-color bright-red e))
(defn color-bright-green [e]   (fg-color bright-green e))
(defn color-bright-yellow [e]  (fg-color bright-yellow e))
(defn color-bright-blue [e]    (fg-color bright-blue e))
(defn color-bright-magenta [e] (fg-color bright-magenta e))
(defn color-bright-cyan [e]    (fg-color bright-cyan e))
(defn color-bright-white [e]   (fg-color bright-white e))
(defn color-rgb [e r g b]     (fg-color (rgb r g b) e))
(defn color-256 [e n]          (fg-color (c256 n) e))

;; Background color functions
(defn bg-black [e]          (bg-color black e))
(defn bg-red [e]            (bg-color red e))
(defn bg-green [e]          (bg-color green e))
(defn bg-yellow [e]         (bg-color yellow e))
(defn bg-blue [e]           (bg-color blue e))
(defn bg-magenta [e]        (bg-color magenta e))
(defn bg-cyan [e]           (bg-color cyan e))
(defn bg-white [e]          (bg-color white e))
(defn bg-bright-black [e]   (bg-color bright-black e))
(defn bg-bright-red [e]     (bg-color bright-red e))
(defn bg-bright-green [e]   (bg-color bright-green e))
(defn bg-bright-yellow [e]  (bg-color bright-yellow e))
(defn bg-bright-blue [e]    (bg-color bright-blue e))
(defn bg-bright-magenta [e] (bg-color bright-magenta e))
(defn bg-bright-cyan [e]    (bg-color bright-cyan e))
(defn bg-bright-white [e]   (bg-color bright-white e))
(defn bg-rgb [e r g b]     (bg-color (rgb r g b) e))
(defn bg-256 [e n]          (bg-color (c256 n) e))

;; Style functions
(defn style-bold [e]          (->StyledElement e nil nil [code-bold]))
(defn style-dim [e]           (->StyledElement e nil nil [code-dim]))
(defn style-italic [e]        (->StyledElement e nil nil [code-italic]))
(defn style-underline-s [e]   (->StyledElement e nil nil [code-underline]))
(defn style-blink [e]         (->StyledElement e nil nil [code-blink]))
(defn style-reverse [e]       (->StyledElement e nil nil [code-reverse]))
(defn style-hidden [e]        (->StyledElement e nil nil [code-hidden]))
(defn style-strikethrough [e] (->StyledElement e nil nil [code-strikethrough]))

;; ============================================================================
;; Render / Print
;; ============================================================================

(defn print-elem
  "Print element to stdout."
  [element]
  (println (render element)))

;; ============================================================================
;; TUI Runtime - Elm Architecture for interactive terminal applications
;; ============================================================================

;; --- Key types ---

(defn key-char [c]      {:type :char :char c})
(defn key-ctrl [c]      {:type :ctrl :char c})
(def key-enter           {:type :enter})
(def key-backspace       {:type :backspace})
(def key-tab             {:type :tab})
(def key-escape          {:type :escape})
(def key-delete          {:type :delete})
(def key-up              {:type :up})
(def key-down            {:type :down})
(def key-left            {:type :left})
(def key-right           {:type :right})
(def key-home            {:type :home})
(def key-end             {:type :end})
(def key-page-up         {:type :page-up})
(def key-page-down       {:type :page-down})

;; --- Commands ---

(defn cmd-none [] {:cmd :none})
(defn cmd-exit [] {:cmd :exit})
(defn cmd-batch [& cmds] {:cmd :batch :cmds cmds})
(defn cmd-task [f] {:cmd :task :f f})
(defn cmd-after-ms [ms msg] {:cmd :after-ms :ms ms :msg msg})

;; --- Subscriptions ---

(defn sub-none [] {:sub :none})
(defn sub-key-press [handler] {:sub :key-press :handler handler})
(defn sub-every-ms [ms msg] {:sub :every-ms :ms ms :msg msg})
(defn sub-batch [& subs] {:sub :batch :subs subs})

;; --- Terminal (stty-based, zero deps) ---

(defn- stty [args]
  (try
    (let [proc (.exec (Runtime/getRuntime)
                      (into-array String ["sh" "-c" (str "stty " args " < /dev/tty")]))]
      (.waitFor proc)
      (slurp (.getInputStream proc)))
    (catch Exception _ "")))

(defn- terminal-width []
  (try
    (let [size (str/trim (stty "size"))
          parts (str/split size #"\s+")]
      (if (= 2 (count parts))
        (Integer/parseInt (second parts))
        80))
    (catch Exception _ 80)))

(defn- esc [& codes]
  (doseq [c codes]
    (.print System/out (str "\033[" c)))
  (.flush System/out))

(defn- enter-raw-mode []
  (let [original (str/trim (stty "-g"))]
    (stty "-echo -icanon -ixon min 1")
    original))

(defn- exit-raw-mode [original]
  (when (seq original)
    (stty original)))

(defn- clear-screen []     (esc "2J" "H"))
(defn- clear-scrollback [] (esc "3J" "2J" "H"))
(defn- hide-cursor []      (esc "?25l"))
(defn- show-cursor []      (esc "?25h"))

;; --- Key parsing ---

(defn- read-byte []
  (.read System/in))

(defn- available? []
  (pos? (.available System/in)))

(defn- read-byte-nonblocking []
  (Thread/sleep 5)
  (when (available?)
    (.read System/in)))

(defn- consume-tilde []
  (Thread/sleep 5)
  (when (available?)
    (.read System/in)))

(defn- parse-escape []
  (try
    (if-let [b (read-byte-nonblocking)]
      (if (= b (int \[))
        (if-let [b2 (read-byte-nonblocking)]
          (case (char b2)
            \A key-up
            \B key-down
            \C key-right
            \D key-left
            \H key-home
            \F key-end
            \5 (do (consume-tilde) key-page-up)
            \6 (do (consume-tilde) key-page-down)
            \3 (do (consume-tilde) key-delete)
            key-escape)
          key-escape)
        key-escape)
      key-escape)
    (catch Exception _ key-escape)))

(defn- parse-key [input]
  (cond
    (or (= input 10) (= input 13)) key-enter
    (= input 9)                     key-tab
    (= input 27)                    (parse-escape)
    (or (= input 8) (= input 127)) key-backspace
    (<= 32 input 126)               (key-char (char input))
    (<= 1 input 26)                 (key-ctrl (char (+ input 64)))
    :else                            {:type :unknown :code input}))

;; --- App options ---

(def default-app-options
  {:alignment    :left
   :quit-key     (key-ctrl \Q)
   :clear-on-start true
   :clear-on-exit  true
   :render-interval-ms 33})

;; --- Command processing ---

(defn- process-command [cmd update-state-fn]
  (case (:cmd cmd)
    :none  nil
    :exit  :exit
    :batch (some #(process-command % update-state-fn) (:cmds cmd))
    :task  (future
             (when-let [msg ((:f cmd))]
               (update-state-fn msg)))
    :after-ms (future
                (Thread/sleep (:ms cmd))
                (update-state-fn (:msg cmd)))
    nil))

;; --- Subscription helpers ---

(defn- flatten-subs [sub]
  (case (:sub sub)
    :none  []
    :batch (mapcat flatten-subs (:subs sub))
    [sub]))

(defn- get-key-handler [subs]
  (some #(when (= :key-press (:sub %)) (:handler %))
        subs))

(defn- get-tick-subs [subs]
  (filter #(= :every-ms (:sub %)) subs))

;; --- Input helper for text fields ---

(defn input-handle
  "Handle a key for a text field. Returns new value or nil if unhandled.
   field-id: which field (0, 1, ...)
   active-field: currently active field
   current-value: current text"
  [key field-id active-field current-value]
  (when (= field-id active-field)
    (case (:type key)
      :char (let [c (:char key)]
              (when (or (Character/isLetterOrDigit c)
                        (Character/isWhitespace c)
                        (str/includes? "!@#$%^&*()_+-=[]{}|;':,.<>?/\\\"" (str c)))
                (str current-value c)))
      :backspace (when (seq current-value)
                   (subs current-value 0 (dec (count current-value))))
      nil)))

;; --- The runtime ---

(defn run-app
  "Run an Elm-architecture TUI app.

   app is a map with:
     :init           - fn [] -> [state cmd]
     :update         - fn [msg state] -> [state cmd]
     :subscriptions  - fn [state] -> sub
     :view           - fn [state] -> element

   opts (optional) merges with default-app-options."
  ([app] (run-app app {}))
  ([app opts]
   (let [opts          (merge default-app-options opts)
         state-ref     (atom nil)
         should-run    (atom true)
         original-stty (atom nil)
         state-lock    (Object.)

         update-fn-ref (atom nil)

         update-state! (fn [msg]
                         (locking state-lock
                           (let [[new-state cmd] ((:update app) msg @state-ref)]
                             (reset! state-ref new-state)
                             (when (= :exit (process-command cmd @update-fn-ref))
                               (reset! should-run false)))))

         _ (reset! update-fn-ref update-state!)

         ;; Initialize
         _ (do (reset! original-stty (enter-raw-mode))
               (when (:clear-on-start opts) (clear-screen))
               (hide-cursor)
               (let [[init-state init-cmd] ((:init app))]
                 (reset! state-ref init-state)
                 (when (= :exit (process-command init-cmd update-state!))
                   (reset! should-run false))))

         term-w        (terminal-width)

         ;; Render thread
         render-thread
         (Thread.
          (fn []
            (let [last-rendered (atom "")
                  last-line-count (atom 0)]
              (while @should-run
                (try
                  (let [rendered (render ((:view app) @state-ref))]
                    (when (not= rendered @last-rendered)
                      (let [rendered-lines (str/split rendered #"\n" -1)
                            current-count  (count rendered-lines)
                            max-line-w     (reduce max 0 (map visible-length rendered-lines))
                            block-pad      (case (:alignment opts)
                                             :left   0
                                             :center (max 0 (quot (- term-w max-line-w) 2))
                                             :right  (max 0 (- term-w max-line-w))
                                             0)
                            padding        (apply str (repeat block-pad \space))
                            aligned        (if (pos? block-pad)
                                             (map #(str padding %) rendered-lines)
                                             rendered-lines)
                            move-up        (if (:clear-on-start opts)
                                             "\033[H"
                                             (if (pos? @last-line-count)
                                               (str "\033[" @last-line-count "A\r")
                                               ""))
                            extra          (- @last-line-count current-count)
                            sb             (StringBuilder.)]
                        (.append sb move-up)
                        (doseq [line aligned]
                          (.append sb line)
                          (.append sb "\033[K\n"))
                        (when (pos? extra)
                          (dotimes [_ extra]
                            (.append sb "\033[K\n")))
                        (.print System/out (.toString sb))
                        (.flush System/out)
                        (reset! last-rendered rendered)
                        (reset! last-line-count current-count))))
                  (catch Exception _ nil))
                (Thread/sleep (:render-interval-ms opts))))))

         ;; Tick thread
         tick-thread
         (Thread.
          (fn []
            (let [last-tick-times (atom {})]
              (while @should-run
                (try
                  (let [now  (System/currentTimeMillis)
                        subs (flatten-subs ((:subscriptions app) @state-ref))
                        ticks (get-tick-subs subs)]
                    (doseq [{:keys [ms msg]} ticks]
                      (let [last-time (get @last-tick-times ms 0)]
                        (when (>= (- now last-time) ms)
                          (swap! last-tick-times assoc ms now)
                          (update-state! msg)))))
                  (catch Exception _ nil))
                (Thread/sleep 10)))))

         ;; Input thread
         input-thread
         (Thread.
          (fn []
            (while @should-run
              (try
                (let [input (read-byte)
                      key   (parse-key input)]
                  (if (= key (:quit-key opts))
                    (reset! should-run false)
                    (let [subs    (flatten-subs ((:subscriptions app) @state-ref))
                          handler (get-key-handler subs)]
                      (when handler
                        (when-let [msg (handler key)]
                          (update-state! msg))))))
                (catch Exception _
                  (Thread/sleep 10))))))]

     ;; Start threads
     (.setDaemon render-thread true)
     (.setDaemon tick-thread true)
     (.setDaemon input-thread true)
     (.start render-thread)
     (.start tick-thread)
     (.start input-thread)

     ;; Wait for exit
     (try
       (while @should-run
         (Thread/sleep 50))
       (catch InterruptedException _ nil)
       (finally
         (reset! should-run false)
         (when (:clear-on-exit opts) (clear-scrollback))
         (show-cursor)
         (.print System/out "\n")
         (.flush System/out)
         (exit-raw-mode @original-stty)))

     ;; Return final state
     @state-ref)))

(defn run-inline
  "Run app inline - no alt screen, no clearing. Renders in-place."
  ([app] (run-inline app {}))
  ([app opts]
   (run-app app (merge opts {:clear-on-start false :clear-on-exit false}))))
