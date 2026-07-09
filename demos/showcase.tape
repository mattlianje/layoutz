# layoutz Showcase Demo
# Records a GIF navigating all 8 scenes of the showcase TUI

Output demos/showcase.gif
Set FontSize 28
Set Width 1700
Set Height 1300
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

# ── Scene 1/8: Ray Marcher ──
Sleep 1.2s

# Tilt toward the viewer
Type "w"
Sleep 60ms
Type "w"
Sleep 60ms
Type "w"
Sleep 400ms

# Quick zoom in and back out on the torus
Type "+"
Sleep 70ms
Type "+"
Sleep 70ms
Type "+"
Sleep 70ms
Type "+"
Sleep 350ms
Type "-"
Sleep 70ms
Type "-"
Sleep 70ms
Type "-"
Sleep 70ms
Type "-"
Sleep 400ms

# Tilt the torus further toward the viewer
Type "w"
Sleep 60ms
Type "w"
Sleep 550ms
# ease the tilt back toward a 3/4 angle
Type "s"
Sleep 60ms
Type "s"
Sleep 400ms

# Morph to the cube
Space
Sleep 2.2s

# Orbit the cube on a 3/4 angle
Type "w"
Sleep 60ms
Type "w"
Sleep 300ms
Type "d"
Sleep 60ms
Type "d"
Sleep 60ms
Type "d"
Sleep 60ms
Type "d"
Sleep 280ms
# drop the camera so it isn't looking down at one face
Type "s"
Sleep 60ms
Type "s"
Sleep 60ms
Type "s"
Sleep 300ms
Type "d"
Sleep 60ms
Type "d"
Sleep 60ms
Type "d"
Sleep 60ms
Type "d"
Sleep 350ms
# Zoom on the cube
Type "+"
Sleep 70ms
Type "+"
Sleep 70ms
Type "+"
Sleep 350ms
Type "-"
Sleep 70ms
Type "-"
Sleep 70ms
Type "-"
Sleep 500ms
# Spin the cube
Type@40ms "dddddddddddddddddddddddddddddddddddddddd"
Sleep 700ms

# ── Scene 2/8: Physics Game ──
Right
Sleep 500ms
# Two quick kicks
Space
Sleep 1.7s
Space
Sleep 3s

# ── Scene 3/8: Text Input & Lists ──
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

# ── Scene 4/8: Borders & Styles ──
Right
Sleep 1.5s

# ── Scene 5/8: Tables ──
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
# Browse around without wrapping
Up
Sleep 250ms
Up
Sleep 250ms
Up
Sleep 250ms
Up
Sleep 250ms
Down
Sleep 250ms
Down
Sleep 250ms
Down
Sleep 500ms

# ── Scene 6/8: Charts & Plots ──
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

# ── Scene 7/8: Bar Charts & Sparklines ──
Right
Sleep 1.5s
# Toggle chart mode
Tab
Sleep 1.5s

# ── Scene 8/8: Selections & Heatmap ──
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
