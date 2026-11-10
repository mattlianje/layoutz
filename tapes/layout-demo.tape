# layoutz Layout Composition Demo (Animated)
# Records a GIF of the layout building blocks with animated progress bars and gradient

Output ../docs/pix/layout-demo.gif
Set FontSize 28
Set Width 1800
Set Height 1100
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 30

Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 45s
Type ":load examples/LayoutShowcase.scala"
Enter
Sleep 15s
Type "LayoutShowcase.run()"
Enter
Sleep 2s
Show

Sleep 8s

Hide
Ctrl+Q
