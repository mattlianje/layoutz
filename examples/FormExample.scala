import layoutz._

case class FormState(
    name: String = "",
    mood: Int = 0, // index into moods list
    selectedLetters: Set[Int] = Set.empty, // indices of selected letters
    letterCursor: Int = 0, // current cursor position in multi-choice
    activeField: Int = 0, // 0=name, 1=mood, 2=letters
    submitted: Boolean = false
)

sealed trait FormMsg
case class UpdateName(newValue: String) extends FormMsg
case object NextField extends FormMsg
case object PrevField extends FormMsg
case object FormMoveUp extends FormMsg
case object FormMoveDown extends FormMsg
case object ToggleSelection extends FormMsg
case object Submit extends FormMsg

object FormExample extends LayoutzApp[FormState, FormMsg] {

  private val moods = Seq("great", "okay", "meh", "not great")
  private val letters = ('A' to 'F').map(_.toString).toSeq

  def init = (FormState(), Cmd.none)

  def update(msg: FormMsg, state: FormState) = msg match {
    case UpdateName(newValue) =>
      (state.copy(name = newValue), Cmd.none)

    case NextField if state.activeField < 2 =>
      (state.copy(activeField = state.activeField + 1), Cmd.none)

    case PrevField if state.activeField > 0 =>
      (state.copy(activeField = state.activeField - 1), Cmd.none)

    // Single choice navigation
    case FormMoveUp if state.activeField == 1 =>
      val newMood = if (state.mood > 0) state.mood - 1 else moods.length - 1
      (state.copy(mood = newMood), Cmd.none)

    case FormMoveDown if state.activeField == 1 =>
      val newMood = if (state.mood < moods.length - 1) state.mood + 1 else 0
      (state.copy(mood = newMood), Cmd.none)

    // Multi choice navigation and toggle
    case FormMoveUp if state.activeField == 2 =>
      val newCursor =
        if (state.letterCursor > 0) state.letterCursor - 1
        else letters.length - 1
      (state.copy(letterCursor = newCursor), Cmd.none)

    case FormMoveDown if state.activeField == 2 =>
      val newCursor =
        if (state.letterCursor < letters.length - 1) state.letterCursor + 1
        else 0
      (state.copy(letterCursor = newCursor), Cmd.none)

    case ToggleSelection if state.activeField == 2 =>
      val newSelected =
        if (state.selectedLetters.contains(state.letterCursor)) {
          state.selectedLetters - state.letterCursor
        } else {
          state.selectedLetters + state.letterCursor
        }
      (state.copy(selectedLetters = newSelected), Cmd.none)

    case Submit =>
      (state.copy(submitted = true), Cmd.none)

    case _ => (state, Cmd.none)
  }

  def subscriptions(state: FormState) = Sub.onKeyPress { key =>
    // Space toggles in multi-choice - handle this first!
    (if (state.activeField == 2 && key == CharKey(' ')) Some(ToggleSelection)
     else None)
      // Check if name field handled the key
      .orElse(
        input.handle(key, 0, state.activeField, state.name).map(UpdateName)
      )
      // Otherwise check other keys
      .orElse(key match {
        case TabKey                                 => Some(NextField)
        case CharKey('n') if state.activeField != 0 => Some(NextField)
        case CharKey('p') if state.activeField != 0 => Some(PrevField)
        case ArrowUpKey | CharKey('k')              => Some(FormMoveUp)
        case ArrowDownKey | CharKey('j')            => Some(FormMoveDown)
        case EnterKey                               => Some(Submit)
        case _                                      => None
      })
  }

  def view(state: FormState) =
    if (state.submitted) {
      layout(
        section("✓ Form Submitted")(
          layout(
            s"Name: ${state.name}",
            s"Mood: ${moods(state.mood)}",
            s"Letters: ${state.selectedLetters.toSeq.sorted.map(i => letters(i)).mkString(", ")}",
            br
          )
        )
      )
    } else {
      layout(
        section("📝 User Survey")(
          layout(
            textInput(
              "What's your name?",
              state.name,
              "Type here...",
              active = state.activeField == 0
            ),
            br,
            SingleChoice(
              label = "How was your day?",
              options = moods,
              selected = state.mood,
              active = state.activeField == 1
            ),
            br,
            MultiChoice(
              label = "Favorite letters?",
              options = letters,
              selected = state.selectedLetters,
              cursor = state.letterCursor,
              active = state.activeField == 2
            )
          )
        ),
        br,
        section("Controls")(
          ul(
            "Type to enter name",
            "↑↓ / k/j to navigate options",
            "Space to toggle multi-choice",
            "Tab / n to next field, p for previous",
            "Enter to submit"
          )
        )
      )
    }

}
