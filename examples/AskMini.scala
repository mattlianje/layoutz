import layoutz._

object AskMini {
  def main(args: Array[String]): Unit = {
    print("[2J[H")

    def gap(): Unit = println()

    val longString = {
      val src = scala.io.Source.fromFile("README.md")
      try src.mkString
      finally src.close()
    }

    val name     = Ask.input("Name › ", placeholder = "anonymous"); gap()
    val ok       = Ask.confirm("Venture on the quest?", default = true); gap()
    val realm    = Ask.choose("Choose a realm", Seq("The Shire", "Rivendell", "Mirkwood", "Lake-town", "Erebor")); gap()
    val packs    = Ask.chooseMany("Pack provisions", Seq("lembas", "pipe-weed", "waybread", "miruvor", "rope"), limit = 3); gap()
    val riddle   = Ask.write("Pose a multi-line riddle", placeholder = "This thing all things devours…"); gap()
    val member   = Ask.filter("Search > ", Seq("Bilbo", "Balin", "Dwalin", "Thorin", "Gandalf")); gap()
    val path     = Ask.file(start = "examples"); gap()
    Ask.pager(longString, height = 12); gap()
    val answer   = Ask.spin("Awaking Smaug…") { Thread.sleep(1500); 42 }
  }
}
