import layoutz._

object AskExample {
  def main(args: Array[String]): Unit = {
    val name = Ask.input("What's your name? › ", placeholder = "anonymous").getOrElse("anonymous")
    println(s"Hi, $name!")

    val flavor = Ask.choose(
      "Pick a bubble gum flavor",
      Seq("Strawberry", "Banana", "Cherry", "Watermelon", "Mint")
    )
    flavor.foreach(f => println(s"Picked: $f"))

    val toppings = Ask.chooseMany(
      "Pick up to 3 toppings",
      Seq("cheese", "mushroom", "onion", "pepperoni", "olive"),
      limit = 3
    )
    toppings.foreach(t => println(s"Toppings: ${t.mkString(", ")}"))

    val ok = Ask.confirm(s"Order $name a pack of ${flavor.getOrElse("any")} gum?", default = true)
    println(if (ok) "On its way." else "Maybe next time.")

    val story = Ask.write(prompt = "Tell me a story", placeholder = "Once upon a time…")
    story.foreach(s => println(s"You wrote ${s.length} chars."))

    val fruits =
      Seq("Strawberry", "Banana", "Cherry", "Blueberry", "Watermelon", "Mango", "Papaya", "Kiwi")
    val match_ = Ask.filter("Search › ", fruits)
    match_.foreach(m => println(s"Matched: $m"))

    val picked = Ask.file(System.getProperty("user.dir"))
    picked.foreach(p => println(s"Selected file: $p"))

    Ask.pager((1 to 200).map(i => s"This is line number $i of two hundred").mkString("\n"))

    val result = Ask.spin("Crunching numbers…") {
      Thread.sleep(1500)
      42
    }
    println(s"Answer: $result")
  }
}
