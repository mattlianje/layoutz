import layoutz._

/** Simpler form example using input.handler helpers */

case class SimpleFormState(
    name: String = "",
    email: String = "",
    activeField: Int = 0
)

sealed trait SimpleFormMsg
case class UpdateSimpleName(value: String) extends SimpleFormMsg
case class UpdateSimpleEmail(value: String) extends SimpleFormMsg
case object NextSimpleField extends SimpleFormMsg
case object SubmitSimple extends SimpleFormMsg

object SimpleFormExample extends LayoutzApp[SimpleFormState, SimpleFormMsg] {

  def init = (SimpleFormState(), Cmd.none)

  def update(msg: SimpleFormMsg, state: SimpleFormState) = msg match {
    case UpdateSimpleName(value)  => (state.copy(name = value), Cmd.none)
    case UpdateSimpleEmail(value) => (state.copy(email = value), Cmd.none)
    case NextSimpleField =>
      (state.copy(activeField = (state.activeField + 1) % 2), Cmd.none)
    case SubmitSimple =>
      println(s"Submitted: ${state.name}, ${state.email}")
      (state, Cmd.none)
  }

  def subscriptions(state: SimpleFormState) = Sub.onKeyPress { key =>
    // Check if name field handled the key
    input
      .handle(key, 0, state.activeField, state.name)
      .map(UpdateSimpleName)
      // Otherwise check if email field handled it
      .orElse(
        input
          .handle(key, 1, state.activeField, state.email)
          .map(UpdateSimpleEmail)
      )
      // Otherwise check other keys
      .orElse(key match {
        case Key.Tab   => Some(NextSimpleField)
        case Key.Enter => Some(SubmitSimple)
        case _         => None
      })
  }

  def view(state: SimpleFormState) = layout(
    section("📝 Simple Form")(
      layout(
        textInput(
          "Name",
          state.name,
          "Your name...",
          active = state.activeField == 0
        ),
        textInput(
          "Email",
          state.email,
          "your@email.com",
          active = state.activeField == 1
        )
      )
    ),
    br,
    section("Controls")("Tab to switch fields, Enter to submit")
  )

}
