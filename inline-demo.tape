# layoutz Inline Demo
# Shows a LayoutzApp animating inline without clearing the screen

Output pix/inline-demo.gif
Set FontSize 28
Set Width 1100
Set Height 550
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 60

# Start mill console, load definitions, and run (all hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/InlineDemo.scala"
Enter
Sleep 5s
Type "InlineDemo.run()"
Enter
Sleep 300ms
Show

# Lines print one by one, then the gradient bar animates, then done
Sleep 8s

Hide
