import layoutz._

object AskDemo {
  def main(args: Array[String]): Unit = {
    print("[2J[H")

    def gap(): Unit = println()

    val companions = Seq(
      "Bilbo",
      "Balin",
      "Dwalin",
      "Thorin",
      "Gandalf",
      "Kili",
      "Fili",
      "Bombur",
      "Bofur",
      "Gloin",
      "Oin",
      "Dori"
    )

    val realms = Seq("The Shire", "Rivendell", "Mirkwood", "Lake-town", "Erebor")

    Ask.input("Name › ", placeholder = "anonymous")
    gap()

    Ask.choose("Choose a realm", realms)
    gap()

    Ask.chooseMany(
      "Pack provisions (up to 3)",
      Seq("lembas", "pipe-weed", "waybread", "miruvor", "rope", "athelas"),
      limit = 3
    )
    gap()

    Ask.filter("Search a companion › ", companions)
    gap()

    Ask.write(prompt = "Pose a riddle", placeholder = "This thing all things devours…")
    gap()

    Ask.file(start = "demos")
    gap()

    Ask.confirm("Venture on the quest?", default = true)
    gap()

    Ask.pager(
      (0 until 40)
        .map { i =>
          val c = companions(i % companions.length)
          f"${i + 1}%2d.  $c%-12s ${realms(i % realms.length)}"
        }
        .mkString("\n"),
      height = 8
    )
    gap()

    Ask.spin("Awaking Smaug…") {
      Thread.sleep(1500)
      "ready"
    }
  }
}
