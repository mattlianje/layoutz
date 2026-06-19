import layoutz._

object LoaderExample {
  def main(args: Array[String]): Unit = {
    for (n <- loader("Processing", 1 to 50)) {
      Thread.sleep(40)
      val _ = n
    }
    println("Bounded done")

    for (n <- loader(1 to 30)) {
      Thread.sleep(40)
      val _ = n
    }
    println("No-label done")

    val it: Iterator[Int] = Iterator.from(1).take(60)
    for (n <- loader.stream("Streaming", it)) {
      Thread.sleep(30)
      val _ = n
    }
    println("Streaming done")
  }
}
