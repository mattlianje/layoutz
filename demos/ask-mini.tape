# layoutz Ask Mini Demo
# The minimal Ask.* tour — one line per widget, each collapses to a committed answer

Output demos/ask-mini.gif
Set FontSize 26
Set Width 1200
Set Height 1000
Set Theme "Catppuccin Mocha"
Set TypingSpeed 55ms
Set Padding 10
Set Framerate 60

# Start mill console and load the demo (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/AskMini.scala"
Enter
Sleep 8s
Type "AskMini.main(Array())"
Enter
Sleep 1.5s
Show

# ── Ask.input (name) ──
Sleep 700ms
Type "Thorin Oakenshield"
Sleep 300ms
Enter
Sleep 600ms

# ── Ask.confirm (Ship it?) — toggle No/Yes before committing Yes ──
Sleep 300ms
Right
Sleep 350ms
Left
Sleep 350ms
Right
Sleep 350ms
Left
Sleep 450ms
Enter
Sleep 600ms

# ── Ask.choose (realm) ──
Down
Sleep 150ms
Down
Sleep 250ms
Enter
Sleep 600ms

# ── Ask.chooseMany (provisions, limit 3) ──
Space
Sleep 160ms
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

# ── Ask.write (multiline riddle — Enter for newlines, Ctrl+D to submit) ──
Type "Thirty white horses on a red hill,"
Sleep 250ms
Enter
Type "first they champ, then they stamp,"
Sleep 250ms
Enter
Type "then they stand still."
Sleep 350ms
Ctrl+D
Sleep 600ms

# ── Ask.filter (fuzzy search — "lin" narrows to Balin/Dwalin) ──
Type "lin"
Sleep 500ms
Down
Sleep 350ms
Enter
Sleep 600ms

# ── Ask.file (file picker in examples/) — land on a .scala file and select ──
Down
Sleep 150ms
Down
Sleep 150ms
Down
Sleep 250ms
Enter
Sleep 700ms

# ── Ask.pager (scrollable README viewer) — smooth line scroll, then quit ──
Sleep 500ms
Down@85ms 18
Sleep 600ms
Down@85ms 14
Sleep 600ms
Up@85ms 22
Sleep 600ms
Type "q"
Sleep 400ms

# ── Ask.spin (blocking task) ──
Sleep 2.2s

Hide
