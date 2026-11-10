# layoutz Tables & Borders Demo (Animated)
# Records a GIF showing tables, border styles, status cards, and trees

Output ../docs/pix/table-demo.gif
Set FontSize 28
Set Width 1500
Set Height 1000
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 30

Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 45s
Type ":load examples/TableBorderDemo.scala"
Enter
Sleep 15s
Type "TableBorderDemo.run()"
Enter
Sleep 2s
Show

Sleep 8s

Hide
Ctrl+Q
