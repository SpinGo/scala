// Tests to assert that references to threads are not strongly held when scala-reflection is used inside of them.
object Test {
  import scala.ref.WeakReference

  def nudgeGc() = {
    val ref = new WeakReference(new Object)
    while(ref.get.nonEmpty)
      Array.ofDim[Byte](16 * 1024 * 1024)
  }

  def anyCollectable(refs: Seq[WeakReference[_]], triesRemaining: Int = 8): Boolean = {
    nudgeGc()
    Thread.sleep(25) // give the JVM some time to finish the cycle
    val retained = refs flatMap (_.get)

    if (retained.length < refs.length) // we collected one!
      true
    else if (triesRemaining > 0)
      anyCollectable(refs, triesRemaining - 1)
    else
      false
  }

  def main(args: Array[String]): Unit = {
    val threads: Seq[WeakReference[Thread]] = for (i <- (1 to 8)) yield {
      val t = new Thread {
        val data = Array.ofDim[Byte](1 * 1024 * 1024)
        override def run(): Unit = {
          import reflect.runtime.universe._
          typeOf[List[String]] <:< typeOf[Seq[_]]
        }
      }
      t.start()
      t.join()
      WeakReference(t)
    }

    assert(anyCollectable(threads), s"All thread objects are being retained; expected: they should be available for garbage collection.")
  }
}
