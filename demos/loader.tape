# layoutz Loader Demo
# Wraps any iterable/iterator with a live progress display — one line per style

Output demos/loader.gif
Set FontSize 26
Set Width 1100
Set Height 800
Set Theme "Catppuccin Mocha"
Set Padding 10
Set Framerate 60

# Start mill console and load the demo (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/LoaderExample.scala"
Enter
Sleep 8s
Type "LoaderExample.main(Array())"
Enter
# let main clear the REPL banner while still hidden
Sleep 800ms
Show

# Let every style fill, then the streaming spinner run to completion
Sleep 12s

Hide
