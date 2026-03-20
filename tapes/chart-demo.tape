# layoutz Charts & Plots Demo (Animated)
# Records a GIF showing live-updating pie, bar, braille plot, sparkline, and heatmap

Output ../docs/pix/chart-demo.gif
Set FontSize 28
Set Width 1500
Set Height 1200
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 30

Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 45s
Type ":load examples/ChartDemo.scala"
Enter
Sleep 15s
Type "ChartDemo.run()"
Enter
Sleep 2s
Show

# Capture several seconds of live animation
Sleep 8s

# Quit
Hide
Ctrl+Q
