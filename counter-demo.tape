# layoutz Counter Demo
# Records a GIF of the simple CounterApp

Output pix/counter-demo.gif
Set FontSize 28
Set Width 1100
Set Height 550
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 60

# Start mill console and load the counter app (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/CounterApp.scala"
Enter
Sleep 5s
Type "CounterApp.run"
Enter
Sleep 3s
Show

Sleep 800ms
Type "+"
Sleep 300ms
Type "+"
Sleep 300ms
Type "+"
Sleep 300ms
Type "+"
Sleep 300ms
Type "+"
Sleep 500ms

# Go down past zero into negatives
Type "-"
Sleep 250ms
Type "-"
Sleep 250ms
Type "-"
Sleep 250ms
Type "-"
Sleep 250ms
Type "-"
Sleep 250ms
Type "-"
Sleep 250ms
Type "-"
Sleep 250ms
Type "-"
Sleep 500ms

# Back up a bit
Type "+"
Sleep 300ms
Type "+"
Sleep 300ms
Type "+"
Sleep 1s

Hide
Ctrl+Q
