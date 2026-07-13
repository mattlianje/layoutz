# layoutz Showcase Mini Demo
# Records a GIF of just Scene 1 (the ray marcher) — the torus and cube,
# exactly as the main showcase opens, without moving on to the other scenes.

Output demos/showcase-mini.gif
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

# Quit
Hide
Ctrl+Q
