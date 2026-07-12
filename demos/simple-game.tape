# layoutz Simple Game Demo
# A tiny grid game driven by WASD / arrow keys

Output demos/simple-game.gif
Set FontSize 24
Set Width 1200
Set Height 750
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 60

# Start mill console and load the example (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/SimpleGame.scala"
Enter
Sleep 10s
Type "SimpleGame.run"
Enter
Sleep 3s
Show

# Walk around the board
Sleep 1.5s
Right
Sleep 400ms
Right
Sleep 400ms
Down
Sleep 400ms
Down
Sleep 400ms
Right
Sleep 400ms
Up
Sleep 400ms
Left
Sleep 400ms
Left
Sleep 400ms
Down
Sleep 1.5s

# Restart the game
Type "r"
Sleep 2.5s

Hide
Ctrl+Q
