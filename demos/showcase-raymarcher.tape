# layoutz Showcase — Ray Marcher spotlight
# Jumps straight to the Ray Marcher scene

Output demos/showcase-raymarcher.gif
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
Sleep 1.5s

# Tilt toward the viewer
Type "w"
Sleep 80ms
Type "w"
Sleep 80ms
Type "w"
Sleep 700ms

# Quick zoom in and back out on the torus
Type "+"
Sleep 100ms
Type "+"
Sleep 100ms
Type "+"
Sleep 100ms
Type "+"
Sleep 550ms
Type "-"
Sleep 100ms
Type "-"
Sleep 100ms
Type "-"
Sleep 100ms
Type "-"
Sleep 700ms

# Tilt the torus further toward the viewer
Type "w"
Sleep 80ms
Type "w"
Sleep 900ms
# ease the tilt back toward a 3/4 angle
Type "s"
Sleep 80ms
Type "s"
Sleep 600ms

# Morph to the cube
Space
Sleep 2.4s

# Orbit the cube on a 3/4 angle
Type "w"
Sleep 80ms
Type "w"
Sleep 450ms
Type "d"
Sleep 80ms
Type "d"
Sleep 80ms
Type "d"
Sleep 80ms
Type "d"
Sleep 400ms
# drop the camera so it isn't looking down at one face
Type "s"
Sleep 80ms
Type "s"
Sleep 80ms
Type "s"
Sleep 450ms
Type "d"
Sleep 80ms
Type "d"
Sleep 80ms
Type "d"
Sleep 80ms
Type "d"
Sleep 550ms
# Zoom on the cube
Type "+"
Sleep 100ms
Type "+"
Sleep 100ms
Type "+"
Sleep 500ms
Type "-"
Sleep 100ms
Type "-"
Sleep 100ms
Type "-"
Sleep 700ms
# Spin the cube
Type@45ms "dddddddddddddddddddddddddddddddddddddddd"
Sleep 900ms

Hide
Ctrl+Q
