(ns layoutz.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [layoutz.core :as l]))

;; ============================================================================
;; Text
;; ============================================================================

(deftest test-text-rendering
  (is (= "Hello World" (l/render "Hello World"))))

(deftest test-text-shorthand
  (is (= "Hello" (l/render (l/s "Hello")))))

(deftest test-string-as-element
  (is (= 5 (l/elem-width "Hello")))
  (is (= 1 (l/elem-height "Hello")))
  (is (= 2 (l/elem-height "Line 1\nLine 2"))))

(deftest test-text-width
  (is (= 5 (l/elem-width (l/text "Hello")))))

(deftest test-text-height
  (is (= 1 (l/elem-height (l/text "Hello")))))

(deftest test-multiline-height
  (is (= 2 (l/elem-height "Line 1\nLine 2"))))

;; ============================================================================
;; Line Break / Space / Empty
;; ============================================================================

(deftest test-line-break
  (is (= "" (l/render l/br))))

(deftest test-multiple-line-breaks
  (is (= "\n\n" (l/render (l/br' 3)))))

(deftest test-space
  (is (= " " (l/render l/space))))

(deftest test-multiple-spaces
  (is (= "   " (l/render (l/space' 3)))))

(deftest test-empty
  (is (= "" (l/render l/empty-elem)))
  (is (= 0 (l/elem-width l/empty-elem)))
  (is (= 0 (l/elem-height l/empty-elem))))

;; ============================================================================
;; Horizontal / Vertical Rules
;; ============================================================================

(deftest test-hr-default-width
  (is (= 50 (l/elem-width l/hr))))

(deftest test-hr-custom-width
  (is (= 10 (l/elem-width (l/hr' :width 10)))))

(deftest test-hr-custom-char
  (is (= "==========" (l/render (l/hr' :char "=" :width 10)))))

(deftest test-vr
  (let [v (l/vr :height 3)]
    (is (= 3 (l/elem-height v)))
    (is (= "│\n│\n│" (l/render v)))))

;; ============================================================================
;; Layout Composition
;; ============================================================================

(deftest test-layout-composition
  (is (= "Line 1\nLine 2"
         (l/render (l/layout ["Line 1" "Line 2"])))))

(deftest test-layout-width
  (is (= 6 (l/elem-width (l/layout ["Line 1" "Line 2"])))))

(deftest test-row-layout
  (is (= "A B" (l/render (l/row ["A" "B"])))))

(deftest test-tight-row
  (is (= "AB" (l/render (l/tight-row ["A" "B"])))))

;; ============================================================================
;; Box
;; ============================================================================

(deftest test-box-height
  (is (= 3 (l/elem-height (l/box "T" ["X"])))))

(deftest test-box-renders
  (let [output (l/render (l/box "Title" ["Content"]))]
    (is (pos? (count output)))))

(deftest test-box-contains-border-chars
  (let [output (l/render (l/box "" ["X"]))]
    (is (str/includes? output "┌"))
    (is (str/includes? output "┘"))
    (is (str/includes? output "│"))
    (is (str/includes? output "─"))))

(deftest test-double-border
  (let [output (l/render (l/border-double (l/box "" ["X"])))]
    (is (str/includes? output "╔"))
    (is (str/includes? output "╝"))))

(deftest test-round-border
  (let [output (l/render (l/border-round (l/box "" ["X"])))]
    (is (str/includes? output "╭"))
    (is (str/includes? output "╯"))))

(deftest test-ascii-border
  (let [output (l/render (l/border-ascii (l/box "" ["X"])))]
    (is (str/includes? output "+"))
    (is (str/includes? output "-"))
    (is (str/includes? output "|"))))

(deftest test-block-border
  (let [output (l/render (l/border-block (l/box "" ["X"])))]
    (is (str/includes? output "█"))))

(deftest test-dashed-border
  (let [output (l/render (l/border-dashed (l/box "" ["X"])))]
    (is (str/includes? output "╌"))))

(deftest test-dotted-border
  (let [output (l/render (l/border-dotted (l/box "" ["X"])))]
    (is (str/includes? output "┈"))))

(deftest test-markdown-border
  (let [output (l/render (l/border-markdown (l/box "" ["X"])))]
    (is (str/includes? output "|"))
    (is (str/includes? output "-"))))

(deftest test-inner-half-block-border
  (let [output (l/render (l/border-inner-half-block (l/box "" ["X"])))]
    (is (str/includes? output "▗"))
    (is (str/includes? output "▘"))))

(deftest test-outer-half-block-border
  (let [output (l/render (l/border-outer-half-block (l/box "" ["X"])))]
    (is (str/includes? output "▛"))
    (is (str/includes? output "▟"))))

;; ============================================================================
;; Table
;; ============================================================================

(deftest test-table-rendering
  (let [t (l/table ["Name" "Role" "Status"]
                   [["Alice" "Engineer" "Online"]
                    ["Bob"   "Designer" "Offline"]
                    ["Eve"   "QA"       "Away"]])]
    (is (= "┌───────┬──────────┬─────────┐\n│ Name  │ Role     │ Status  │\n├───────┼──────────┼─────────┤\n│ Alice │ Engineer │ Online  │\n│ Bob   │ Designer │ Offline │\n│ Eve   │ QA       │ Away    │\n└───────┴──────────┴─────────┘"
           (l/render t)))))

(deftest test-table-border-swap
  (let [t (l/border-double
           (l/table ["A" "B"]
                    [["1" "2"]]))]
    (is (str/includes? (l/render t) "╔"))
    (is (str/includes? (l/render t) "║"))))

;; ============================================================================
;; Banner
;; ============================================================================

(deftest test-banner
  (let [output (l/render (l/banner "Hello"))]
    (is (str/includes? output "╔"))
    (is (str/includes? output "Hello"))
    (is (str/includes? output "╝"))))

;; ============================================================================
;; Key-Value
;; ============================================================================

(deftest test-kv-rendering
  (let [output (l/render (l/kv [["user" "alice"] ["role" "admin"] ["status" "active"]]))]
    (is (= "user:   alice\nrole:   admin\nstatus: active" output))))

;; ============================================================================
;; Alignment
;; ============================================================================

(deftest test-center-with-width
  (let [output (l/render (l/center 10 "Hi"))]
    (is (= 10 (l/visible-length output)))
    (is (str/includes? output "Hi"))))

(deftest test-left-align
  (let [output (l/render (l/left-align 10 "Hi"))]
    (is (= "Hi        " output))))

(deftest test-right-align
  (let [output (l/render (l/right-align 10 "Hi"))]
    (is (= "        Hi" output))))

;; ============================================================================
;; Truncate / Wrap / Justify
;; ============================================================================

(deftest test-truncate
  (let [output (l/render (l/truncate 8 "Hello World"))]
    (is (= "Hello..." output))))

(deftest test-wrap
  (let [output (l/render (l/wrap 10 "one two three four five"))]
    (is (every? #(<= (l/visible-length %) 10)
                (str/split output #"\n")))))

(deftest test-justify
  (let [output (l/render (l/justify 20 "hello world test"))]
    (is (= 20 (l/visible-length output)))))

;; ============================================================================
;; Padding / Margin / Underline
;; ============================================================================

(deftest test-padding
  (let [output (l/render (l/pad 1 "X"))]
    (is (= 3 (l/visible-length (first (str/split output #"\n")))))))

(deftest test-margin
  (let [output (l/render (l/margin ">" "Hello"))]
    (is (= "> Hello" output))))

(deftest test-underline
  (let [output (l/render (l/underline-elem "Hi"))]
    (is (str/includes? output "Hi"))
    (is (str/includes? output "─"))))

;; ============================================================================
;; Columns
;; ============================================================================

(deftest test-columns
  (let [output (l/render (l/columns ["A" "B" "C"]))]
    (is (str/includes? output "A"))
    (is (str/includes? output "B"))
    (is (str/includes? output "C"))
    (is (= 1 (count (str/split output #"\n"))))))

;; ============================================================================
;; Chart / Sparkline / InlineBar / Spinner
;; ============================================================================

(deftest test-chart
  (let [output (l/render (l/chart [["Scala" 85.0] ["Clojure" 95.0]]))]
    (is (str/includes? output "Scala"))
    (is (str/includes? output "Clojure"))
    (is (str/includes? output "█"))))

(deftest test-sparkline
  (let [output (l/render (l/sparkline [1.0 5.0 9.0 3.0]))]
    (is (= 4 (count output)))))

(deftest test-inline-bar
  (let [output (l/render (l/inline-bar "Loading" 0.5))]
    (is (str/includes? output "Loading"))
    (is (str/includes? output "50%"))))

(deftest test-spinner
  (let [output (l/render (l/spinner "Loading" 0 :dots))]
    (is (str/includes? output "Loading"))
    (is (str/includes? output "⠋"))))

;; ============================================================================
;; Lists (ordered / unordered)
;; ============================================================================

(deftest test-unordered-list
  (let [output (l/render (l/ul [(l/li "One") (l/li "Two")]))]
    (is (str/includes? output "•"))
    (is (str/includes? output "One"))
    (is (str/includes? output "Two"))))

(deftest test-ordered-list
  (let [output (l/render (l/ol [(l/li "First") (l/li "Second")]))]
    (is (str/includes? output "1."))
    (is (str/includes? output "2."))
    (is (str/includes? output "First"))))

(deftest test-nested-list
  (let [output (l/render (l/ul [(l/li "Parent"
                                      :c [(l/li "Child")])]))]
    (is (str/includes? output "Parent"))
    (is (str/includes? output "Child"))))

;; ============================================================================
;; Tree
;; ============================================================================

(deftest test-tree
  (let [output (l/render (l/tree (l/node "root"
                                         :c [(l/node "a")
                                             (l/node "b")])))]
    (is (str/includes? output "root"))
    (is (str/includes? output "├──"))
    (is (str/includes? output "└──"))
    (is (str/includes? output "a"))
    (is (str/includes? output "b"))))

;; ============================================================================
;; Section
;; ============================================================================

(deftest test-section
  (let [output (l/render (l/section "Title" ["Body"]))]
    (is (str/includes? output "=== Title ==="))
    (is (str/includes? output "Body"))))

;; ============================================================================
;; Styling
;; ============================================================================

(deftest test-color-wraps-ansi
  (let [output (l/render (l/color-red "Hi"))]
    (is (str/includes? output "\033["))
    (is (str/includes? output "Hi"))
    (is (str/includes? output "\033[0m"))))

(deftest test-bold-wraps-ansi
  (let [output (l/render (l/style-bold "Hi"))]
    (is (str/includes? output "\033[1m"))))

(deftest test-strip-ansi
  (is (= "Hello" (l/strip-ansi "\033[31mHello\033[0m"))))

(deftest test-visible-length-ignores-ansi
  (is (= 5 (l/visible-length "\033[31mHello\033[0m"))))

(deftest test-rgb-color
  (let [output (l/render (l/color-rgb "R" 255 0 0))]
    (is (str/includes? output "38;2;255;0;0"))))

(deftest test-256-color
  (let [output (l/render (l/color-256 "X" 42))]
    (is (str/includes? output "38;5;42"))))

(deftest test-bg-color
  (let [output (l/render (l/bg-red "X"))]
    (is (str/includes? output "\033["))
    (is (str/includes? output "41"))))

;; ============================================================================
;; Nested / Complex Composition
;; ============================================================================

(deftest test-nested-layout
  (let [nested (l/layout
                ["Header"
                 (l/box "Box" ["Inside" (l/row ["A" "B"])])
                 "Footer"])]
    (is (pos? (count (l/render nested))))
    (is (> (count (str/split (l/render nested) #"\n")) 3))))

(deftest test-autocenter-in-layout
  (let [output (l/render (l/layout ["Wide Content!!"
                                    (l/center "Hi")]))]
    (is (str/includes? output "Hi"))
    (is (str/includes? output "Wide Content!!"))))

(deftest test-pipe-style-composition
  (let [output (l/render (-> "Hello"
                             l/style-bold
                             l/color-red
                             l/border-round))]
    (is (str/includes? output "Hello"))
    (is (str/includes? output "╭"))
    (is (str/includes? output "\033["))))

(deftest test-status-card
  (let [output (l/render (l/status-card "Users" "1.2K"))]
    (is (str/includes? output "Users"))
    (is (str/includes? output "1.2K"))
    (is (str/includes? output "┌"))))

;; ============================================================================
;; Display Width (CJK / Emoji)
;; ============================================================================

(deftest test-display-width-ascii
  (is (= 5 (l/display-width "Hello"))))

(deftest test-display-width-cjk
  (is (= 6 (l/display-width "日本語"))))

(deftest test-display-width-mixed
  (is (= 9 (l/display-width "Hi 日本語"))))

;; ============================================================================
;; Extended Plots
;; ============================================================================

(deftest test-plot-line
  (let [s1 (l/series [[0 0] [1 1] [2 4] [3 9]] "squares")
        output (l/render (l/plot-line 20 8 [s1]))]
    (is (str/includes? output "│"))
    (is (str/includes? output "─"))
    (is (pos? (count output)))))

(deftest test-plot-line-multi-series
  (let [s1 (l/series [[0 0] [1 1] [2 2]] "linear" l/bright-cyan)
        s2 (l/series [[0 0] [1 1] [2 4]] "quad" l/bright-red)
        output (l/render (l/plot-line 20 8 [s1 s2]))]
    (is (str/includes? output "●"))
    (is (str/includes? output "linear"))
    (is (str/includes? output "quad"))))

(deftest test-plot-pie
  (let [output (l/render (l/plot-pie 15 8
                                     [(l/slice 50 "A") (l/slice 30 "B") (l/slice 20 "C")]))]
    (is (str/includes? output "A"))
    (is (str/includes? output "B"))
    (is (str/includes? output "%"))))

(deftest test-plot-bar
  (let [output (l/render (l/plot-bar 30 8
                                     [(l/bar-item 80 "Go") (l/bar-item 95 "Rust") (l/bar-item 60 "Zig")]))]
    (is (str/includes? output "│"))
    (is (str/includes? output "─"))
    (is (str/includes? output "Go"))))

(deftest test-plot-stacked-bar
  (let [output (l/render (l/plot-stacked-bar 30 8
                                             [(l/stacked-bar-group "Q1"
                                                                   [(l/bar-item 30 "Sales") (l/bar-item 20 "Costs")])
                                              (l/stacked-bar-group "Q2"
                                                                   [(l/bar-item 40 "Sales") (l/bar-item 25 "Costs")])]))]
    (is (str/includes? output "Q1"))
    (is (str/includes? output "Q2"))
    (is (str/includes? output "█"))))

(deftest test-heatmap
  (let [data (l/heatmap-data [[1.0 5.0 9.0]
                              [3.0 7.0 2.0]]
                             ["row1" "row2"]
                             ["A" "B" "C"])
        output (l/render (l/plot-heatmap data))]
    (is (str/includes? output "row1"))
    (is (str/includes? output "A"))
    (is (str/includes? output "\033[48;5;"))))

;; ============================================================================
;; Runner
;; ============================================================================

(defn -main [& _]
  (run-tests 'layoutz.core-test))
