//> using scala 3.3.7
//> using file ../../layoutz/src/Layoutz.scala
import layoutz._

val sin = (0 to 60).map(i => (i.toDouble, math.sin(i * 0.15) * 5))
val cos = (0 to 60).map(i => (i.toDouble, math.cos(i * 0.15) * 5))

val charts = layout(
  row(
    box("plot")(plot(width = 52, height = 10)(
      Series(sin, "sin(x)").color(Color.BrightCyan),
      Series(cos, "cos(x)").color(Color.BrightMagenta)
    )).border(Border.Round),
    box("pie")(pie(width = 32, height = 8)(
      Slice(50, "Liquor"), Slice(20, "Protein"), Slice(10, "Water"), Slice(20, "Fun")
    )).border(Border.Round)
  ),
  row(
    box("bar")(bar(width = 28, height = 9)(
      Bar(100, "Mon").color(Color.BrightBlue), Bar(120, "Tue").color(Color.BrightMagenta),
      Bar(110, "Wed").color(Color.BrightCyan), Bar(85, "Thu").color(Color.BrightYellow)
    )).border(Border.Round),
    box("stackedBar")(stackedBar(width = 28, height = 7)(
      StackedBar(Seq(Bar(30, "Q1"), Bar(20, "Q2"), Bar(25, "Q3")), "2023"),
      StackedBar(Seq(Bar(35, "Q1"), Bar(25, "Q2"), Bar(30, "Q3")), "2024"),
      StackedBar(Seq(Bar(40, "Q1"), Bar(30, "Q2"), Bar(35, "Q3")), "2025")
    )).border(Border.Round),
    box("boxPlot")(boxPlot(height = 9)(
      BoxData("A", min = 10, q1 = 25, median = 50, q3 = 75, max = 90).color(Color.BrightCyan),
      BoxData("B", min = 20, q1 = 40, median = 55, q3 = 70, max = 85).color(Color.BrightMagenta),
      BoxData("C", min = 5, q1 = 30, median = 45, q3 = 60, max = 95).color(Color.BrightYellow)
    )).border(Border.Round)
  ),
  row(
    box("heatmap")(Heatmap(
      HeatmapData(
        rows = Seq(
          Seq(12.0, 15.0, 22.0, 28.0, 30.0, 25.0),
          Seq(14.0, 18.0, 25.0, 32.0, 35.0, 28.0),
          Seq(10.0, 13.0, 20.0, 26.0, 28.0, 22.0),
          Seq(16.0, 20.0, 27.0, 31.0, 30.0, 33.0)
        ),
        rowLabels = Seq("Mon", "Tue", "Wed", "Thu"),
        colLabels = Seq("6am", "9am", "12pm", "3pm", "6pm", "9pm")
      ),
      cellWidth = 5
    )).border(Border.Round),
    layout(
      box("chart")(chart(("Web", 10.0), ("Mobile", 20.0), ("API", 15.0))).border(Border.Round),
      box("sparkline")(sparkline(Seq(1, 4, 2, 8, 5, 7, 3, 6, 9, 4, 6, 8, 5, 9)).color(Color.BrightGreen)).border(Border.Round)
    )
  )
)
@main def run = println(charts.render)
