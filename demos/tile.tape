
Output demos/tile.gif
Output demos/tile.mp4
Set FontSize 18
Set Width 1600
Set Height 900
Set Theme "Catppuccin Mocha"
Set TypingSpeed 0
Set Padding 10
Set Framerate 30

Hide
Type `cd layoutz-hs && cabal -v0 build tile-demo && $(cabal list-bin tile-demo)`
Enter
Wait+Screen@10m /Try:/
Sleep 500ms
Show

Sleep 500ms
Type@28ms "is there a space leak in the stream loop?"
Sleep 200ms
Enter
Sleep 400ms

Ctrl+N
Sleep 300ms
Type@28ms "typecheck this with -Wall -Wextra"
Enter
Sleep 400ms

Ctrl+N
Sleep 300ms
Type@28ms "can Layout derive Functro"
Backspace@60ms 2
Type@28ms "or? leaves is just toList"
Enter
Sleep 5s

# Find anything: files, signatures, commits, sessions
Ctrl+P
Sleep 700ms
Type@55ms "sel"
Sleep 1.1s
Backspace@40ms 3
Type@55ms "tiledm"
Sleep 700ms
Enter
Sleep 900ms

# Jump straight to a session by name
Ctrl+P
Sleep 400ms
Type@55ms "leak"
Sleep 600ms
Enter
Sleep 400ms
Ctrl+F
Sleep 2.5s
Ctrl+F
Sleep 1.2s

Hide
Ctrl+D
