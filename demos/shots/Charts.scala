//> using scala 3.3.7
//> using file ../../layoutz/src/Layoutz.scala
import layoutz._

val sin = (0 to 60).map(i => (i.toDouble, math.sin(i * 0.15) * 5))
val cos = (0 to 60).map(i => (i.toDouble, math.cos(i * 0.15) * 5))

val d = layout(
  row(
    box("Signals")(plot(width = 44, height = 9)(
      Series(sin, "sin(x)").color(Color.BrightCyan),
      Series(cos, "cos(x)").color(Color.BrightMagenta)
    )).border(Border.Round),
    box("Diet")(pie(width = 33, height = 7)(
      Slice(50, "Liquor"), Slice(20, "Protein"), Slice(10, "Water"), Slice(20, "Fun")
    )).border(Border.Round)
  ),
  row(
    box("Commits")(bar(width = 47, height = 9)(
      Bar(100, "Mon").color(Color.BrightBlue), Bar(120, "Tue").color(Color.BrightMagenta),
      Bar(110, "Wed").color(Color.BrightCyan), Bar(85, "Thu").color(Color.BrightYellow),
      Bar(115, "Fri").color(Color.BrightGreen)
    )).border(Border.Round),
    box("Load")(Heatmap(
      HeatmapData(
        rows = Seq(
          Seq(12.0, 15.0, 22.0, 28.0, 30.0, 25.0, 18.0),
          Seq(14.0, 18.0, 25.0, 32.0, 35.0, 28.0, 20.0),
          Seq(10.0, 13.0, 20.0, 26.0, 28.0, 22.0, 15.0),
          Seq(11.0, 16.0, 24.0, 30.0, 33.0, 27.0, 19.0),
          Seq(13.0, 17.0, 23.0, 29.0, 34.0, 31.0, 26.0),
          Seq(16.0, 20.0, 27.0, 31.0, 30.0, 33.0, 29.0),
          Seq(15.0, 19.0, 21.0, 24.0, 22.0, 20.0, 17.0)
        ),
        rowLabels = Seq("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
        colLabels = Seq("6am", "9am", "12pm", "3pm", "6pm", "9pm", "12am")
      ),
      cellWidth = 5
    )).border(Border.Round)
  )
)
@main def run = println(d.render)
