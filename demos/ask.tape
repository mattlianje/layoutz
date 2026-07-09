# layoutz Ask Demo
# Tours the inline Ask.* widgets — each collapses to a committed line

Output demos/ask.gif
Set FontSize 26
Set Width 1200
Set Height 1100
Set Theme "Catppuccin Mocha"
Set TypingSpeed 55ms
Set Padding 10
Set Framerate 60

# Start mill console and load the demo (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/AskDemo.scala"
Enter
Sleep 8s
Type "AskDemo.main(Array())"
Enter
Sleep 1.5s
Show

# ── Ask.input (DJ name) ──
Sleep 700ms
Type "Thorin"
Sleep 300ms
Enter
Sleep 600ms

# ── Ask.choose (bubble gum flavor) ──
Down
Sleep 130ms
Down
Sleep 200ms
Enter
Sleep 600ms

# ── Ask.chooseMany (toppings, limit 3) ──
Space
Sleep 160ms
Down
Sleep 110ms
Down
Sleep 160ms
Space
Sleep 160ms
Down
Sleep 160ms
Space
Sleep 250ms
Enter
Sleep 600ms

# ── Ask.filter (fuzzy track search) ──
Type "nightc"
Sleep 350ms
Enter
Sleep 600ms

# ── Ask.write (liner notes — real multiline) ──
Type "midnight drive playlist"
Sleep 250ms
Enter
Type "purcell into prokofiev"
Sleep 250ms
Enter
Type "windows down, volume up"
Sleep 350ms
Ctrl+D
Sleep 600ms

# ── Ask.file (file picker) — wander a bit, land on ask.tape ──
Down
Sleep 130ms
Down
Sleep 130ms
Down
Sleep 180ms
Down
Sleep 180ms
Up
Sleep 110ms
Up
Sleep 300ms
Enter
Sleep 700ms

# ── Ask.confirm ──
Right
Sleep 300ms
Left
Sleep 350ms
Enter
Sleep 600ms

# ── Ask.pager (scrollable viewer) — scroll down, then back up ──
Sleep 300ms
Down
Sleep 160ms
Down
Sleep 160ms
Down
Sleep 200ms
Space
Sleep 400ms
Space
Sleep 500ms
Up
Sleep 160ms
Up
Sleep 160ms
Up
Sleep 200ms
PageUp
Sleep 500ms
Type "q"
Sleep 400ms

# ── Ask.spin (blocking task) ──
Sleep 2.2s

Hide
