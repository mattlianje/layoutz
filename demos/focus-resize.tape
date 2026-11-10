# layoutz (Haskell) Focus Demo, with the terminal resized mid-recording.
#
# VHS fixes the terminal size at the top of the tape, so the app runs under
# demos/focus-resize.py, which gives it its own pty: Alt+n slides that pty's
# width in, Alt+w slides it back out. Each step is a real SIGWINCH, so what
# you see is the runtime reflowing the layout live.
#
# Run from the repo root: vhs demos/focus-resize.tape

Output demos/focus-resize.gif
Set FontSize 20
Set Width 1600
Set Height 900
Set Theme "Catppuccin Mocha"
Set TypingSpeed 0
Set Padding 10
Set Framerate 30

Hide
Type `(cd layoutz-hs && cabal -v0 build focus-demo) && python3 demos/focus-resize.py "$(cd layoutz-hs && cabal list-bin focus-demo)"`
Enter
Wait+Screen@10m /focus demo/
Sleep 500ms
Show

# Pick a fighter, hop into their fight log and scroll up
Sleep 1s
Down@350ms 2
Sleep 500ms
Tab
Sleep 400ms
Up@40ms 16
Sleep 500ms

# Drag the right edge in about halfway
Alt+n
Sleep 1.4s

# Still usable while narrow
Up@40ms 12
Sleep 600ms

# Back out
Alt+w
Sleep 1.4s

# Put everything back where it started
Down@40ms 28
Sleep 400ms
Tab
Sleep 400ms
Up@350ms 2
Sleep 1.5s

Hide
Escape
