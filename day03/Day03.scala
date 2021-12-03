import scala.io.Source.stdin

type Matrix = IndexedSeq[IndexedSeq[Int]]

def mostCommon(l: Matrix) =
  l.transpose.map{_.groupBy(identity).toVector.reverse.maxBy{_._2.length}._1}
def leastCommon(l: Seq[Int]) = l.map{1 - _}
def toDec(l: Seq[Int]) = java.lang.Long.parseLong(l.mkString, 2)

def go(l: Matrix, criteria: Matrix => Seq[Int], index: Int = 0): Seq[Int] =
  if l.length > 1 && index < l(0).length then
    val bits = criteria(l)
    go(l.filter{_(index) == bits(index)}, criteria, index + 1)
  else
    l(0)

@main def main(idx: Int) =
  val xs = stdin.getLines().map{_.map{_.toString.toInt}}.toVector

  if idx == 1 then
    val t = mostCommon(xs)
    val gamma = toDec(t)
    val epsilon = toDec(leastCommon(t))
    println(gamma * epsilon)
  else
    val oxygen = toDec(go(xs, mostCommon))
    val co2 = toDec(go(xs, mostCommon.andThen(leastCommon)))
    println(oxygen * co2)

