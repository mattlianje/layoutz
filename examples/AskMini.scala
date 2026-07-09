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
    val realm    = Ask.choose("Choose a realm", Seq("Shire", "Rivendell", "Mirkwood")); gap()
    val packs    = Ask.chooseMany("Provisions", Seq("lembas", "pipe-weed", "rope"), limit = 3); gap()
    val riddle   = Ask.write("Pose a riddle", placeholder = "All things it devours…"); gap()
    val member   = Ask.filter("Search > ", Seq("Bilbo", "Balin", "Dwalin", "Thorin")); gap()
    val path     = Ask.file(start = "examples"); gap()
    Ask.pager(longString, height = 12); gap()
    val answer   = Ask.spin("Awaking Smaug…") { Thread.sleep(1500); 42 }
  }
}
