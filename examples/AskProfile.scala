import layoutz._

object AskProfile {
  def main(args: Array[String]): Unit = {
    print("\u001b[2J\u001b[H")

    def gap(): Unit = println()

    val places = Seq("Bag End", "Bree", "Rivendell", "Erebor", "Edoras", "Lake-town", "Minas Tirith", "Hobbiton")

    val name = Ask.input("Name › ", placeholder = "anonymous"); gap()
    val role = Ask.choose("Role", Seq("Burglar", "Wizard", "King under the Mountain")); gap()
    val home = Ask.filter("Home › ", places); gap()
    val bio = Ask.write("Bio"); gap()
    val ok = Ask.confirm("Save profile?")

    print("\u001b[?25l")
    Thread.sleep(3000)
    print("\u001b[?25h")
  }
}
