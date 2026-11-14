# layoutz (Haskell) napkin demo: one sidebar, one plot, resized live.
# Reuses demos/focus-resize.py for the real-SIGWINCH width slide (Alt+n / Alt+w).
#
# Run from the repo root: vhs demos/napkin-resize.tape

Output demos/napkin-resize.gif
Output demos/napkin-resize.mp4
Set FontSize 22
Set Width 1200
Set Height 720
Set Theme "Catppuccin Mocha"
Set TypingSpeed 0
Set Padding 10
Set Framerate 30

Hide
Type `(cd layoutz-hs && cabal -v0 build napkin-demo) && NARROW=46 python3 demos/focus-resize.py "$(cd layoutz-hs && cabal list-bin napkin-demo)"`
Enter
Wait+Screen@10m /waves/
Sleep 500ms
Show

# Flick through a few waves, back to sin, then squeeze and release.
# Starts and ends on sin at full width, so it loops seamlessly.
Sleep 600ms
Down
Sleep 700ms
Down
Sleep 700ms
Down
Sleep 700ms
Down
Sleep 700ms
Up@120ms 4
Sleep 800ms
Alt+n
Sleep 1.6s
Alt+w
Sleep 1.5s

Hide
Escape
