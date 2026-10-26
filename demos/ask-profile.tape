# layoutz Ask Profile Demo

Output demos/ask-profile.gif
Set FontSize 26
Set Width 1100
Set Height 450
Set Theme "Catppuccin Mocha"
Set TypingSpeed 85ms
Set Padding 10
Set Framerate 60

# Start mill console and load the demo (hidden)
Hide
Type "./mill -i layoutz.jvm[3.3.7].console"
Enter
Sleep 30s
Type ":load examples/AskProfile.scala"
Enter
Sleep 8s
Type "AskProfile.main(Array())"
Enter
Sleep 1.5s
Show

# Ask.input
Sleep 500ms
Type "Bilbo Baggins"
Sleep 300ms
Enter
Sleep 500ms

# Ask.choose
Down@120ms 2
Sleep 200ms
Up
Sleep 150ms
Up
Sleep 200ms
Enter
Sleep 500ms

Sleep 600ms
# Ask.filter
Type "r"
Sleep 350ms
Type "e"
Sleep 600ms
Backspace@120ms 2
Sleep 250ms
Type "b"
Sleep 400ms
Type "a"
Sleep 300ms
Type "g"
Sleep 500ms
Enter
Sleep 500ms

# Ask.write
Type@60ms "Hobbit of the Shire."
Enter
Type@60ms "Fond of second breakfsat"
Sleep 200ms
Backspace@60ms 3
Type@60ms "ast"
Enter
Type@60ms "and the occasional adventure."
Sleep 500ms
Ctrl+D
Sleep 500ms

# Ask.confirm
Right
Sleep 300ms
Left
Sleep 400ms
Enter
Sleep 2s

Hide
