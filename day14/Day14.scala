import scala.io.Source.stdin
import scala.collection.mutable.Map as MutMap

@main def main(idx: Int) =
  val ls = stdin.getLines().to(LazyList)
  val rules = ls.drop(2).map{_.split(" -> ")}.map{
                  case Array(k, v) => ((k(0), k(1)), v(0))}.toMap

  def countMut[A](l: Seq[A]) =
    l.groupMapReduce(identity)(_ => 1L)(_ + _).to(MutMap).withDefaultValue(0L)

  val cs = countMut(ls(0))
  var ps = countMut(ls(0).sliding(2).map{s => (s(0), s(1))}.to(LazyList))

  val top = if (idx == 1) 10 else 40
  for { _ <- 1 to top } do
    val newPs = ps.empty
    ps.foreach{case (p@(p1, p2), v) =>
                 val mid = rules(p)
                 cs(mid) += v
                 newPs((p1, mid)) += v
                 newPs((mid, p2)) += v
              }
    ps = newPs

  println(cs.values.max - cs.values.min)

