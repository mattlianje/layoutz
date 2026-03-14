# layoutz Inline Loading Demo (Haskell)
# Shows progress bars animating in-place without clearing the screen
# The user prompt stays visible above the bars

Output pix/inline-loading-demo.gif
Set FontSize 28
Set Width 1100
Set Height 450
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 60

# Build the demo (hidden)
Hide
Type "cd layoutz-hs && cabal build inline-loading-demo"
Enter
Sleep 30s
Type "clear"
Enter
Sleep 500ms
Show

# User types the command — prompt stays visible throughout
Type "cabal run inline-loading-demo"
Sleep 500ms
Enter

# Watch the three loading bars animate through
Sleep 12s

Hide
