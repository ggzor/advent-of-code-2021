import scala.io.Source.stdin
import scala.annotation.tailrec

@main def main(idx: Int) =
  val xs = stdin.getLines().map{_.map{_.asDigit}.toArray}.toArray
  val allPositions = (0 until 10).flatMap{y => (0 until 10).map{x => (y, x)}}

  @tailrec
  def go(s: Set[(Int, Int)]): Int =
    val next = allPositions.filter{case t@(y, x) => !s.contains(t) && xs(y)(x) > 9}
    if !next.isEmpty then
      for { (y, x) <- next; dy <- -1 to 1; dx <- -1 to 1
            (ny, nx) = (y + dy, x + dx)
            if 0 <= ny && ny < 10 && 0 <= nx && nx < 10
          } do xs(ny)(nx) += 1
      go(s ++ next)
    else s.size
  def step() =
    xs.foreach{_.mapInPlace{_ + 1}}
    val result = go(Set())
    xs.foreach{_.mapInPlace{x => if (x > 9) then 0 else x }}
    result

  if idx == 1 then
    println((1 to 100).map{_ => step()}.sum)
  else
    println(LazyList.from(1).dropWhile(_ => step() != 100).head)

