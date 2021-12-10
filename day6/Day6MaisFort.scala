import scala.util.chaining.scalaUtilChainingOps

val input = io.Source.fromFile(args(0)).getLines().take(1).mkString

val counter: Array[Long] = Array.fill(9)(0L)

(input.split(',').map(_.toInt)).foreach(i => counter(i) += 1)

(1 to 256)
  .foldLeft(counter)((f, _) =>
    f(0).pipe(births => f.drop(1).appended(births).tap(_(6) += births))
  )
  .sum