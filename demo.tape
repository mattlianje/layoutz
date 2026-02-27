# layoutz Showcase Demo
# Records a GIF navigating all 7 scenes of the showcase TUI

Output pix/showcase-demo.gif
Set FontSize 18
Set Width 1000
Set Height 640
Set Theme "Catppuccin Mocha"
Set TypingSpeed 80ms
Set Padding 10
Set Framerate 60

# Start mill console and load the showcase (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/ShowcaseApp.scala"
Enter
Sleep 10s
Type "ShowcaseApp.run(alignment = Alignment.Center)"
Enter
Sleep 3s
Show

# ── Scene 1/7: Bouncing Ball ──
Sleep 4s
# Two quick kicks in succession
Space
Sleep 1.5s
Space
Sleep 3s

# ── Scene 2/7: Text Input & Lists ──
Right
Sleep 1s
Type "Austerlitz"
Sleep 400ms
Enter
Sleep 1.5s
Type "Arcole"
Sleep 400ms
Enter
Sleep 2s

# ── Scene 3/7: Borders & Styles ──
Right
Sleep 1.5s

# ── Scene 4/7: Tables ──
Right
Sleep 1s
Down
Sleep 200ms
Down
Sleep 200ms
Down
Sleep 300ms
# Select Queue
Space
Sleep 600ms
Down
Sleep 300ms
# Select Auth
Space
Sleep 600ms
# Move around a bit more
Down
Sleep 300ms
Down
Sleep 300ms
Up
Sleep 300ms
Up
Sleep 300ms
Up
Sleep 300ms
Up
Sleep 300ms
Down
Sleep 300ms
Down
Sleep 500ms

# ── Scene 5/7: Charts & Plots ──
Right
Sleep 1s
# Shift down fast
Type "-"
Sleep 150ms
Type "-"
Sleep 150ms
Type "-"
Sleep 150ms
Type "-"
Sleep 150ms
Type "-"
Sleep 150ms
Type "-"
Sleep 400ms
# Shift back up fast
Type "+"
Sleep 150ms
Type "+"
Sleep 150ms
Type "+"
Sleep 150ms
Type "+"
Sleep 150ms
Type "+"
Sleep 150ms
Type "+"
Sleep 2s

# ── Scene 6/7: Bar Charts & Sparklines ──
Right
Sleep 1.5s
# Toggle chart mode
Tab
Sleep 1.5s

# ── Scene 7/7: Selections & Heatmap ──
Right
Sleep 1s
Down
Sleep 200ms
Down
Sleep 300ms
# Select Wed
Space
Sleep 600ms
Down
Sleep 200ms
Down
Sleep 300ms
# Select Fri
Space
Sleep 1.5s

# Quit
Hide
Ctrl+Q
