import scala.io.Source.stdin
import scala.collection.mutable.{ArrayDeque, Set}

type Point = (Int, Int)

@main def main(idx: Int) =
  val nums = stdin.getLines().map{_.map{_.asDigit}.toIndexedSeq}.toIndexedSeq
  val (h, w) = (nums.length, nums.head.length)
  val getAt = (t: Point) => nums(t._1)(t._2)

  def neighbors(y: Int, x: Int) =
    for { dy <- -1 to 1; dx <- -1 to 1
          if !(dy == 0 && dx == 0)
          p@(ny, nx) = (y + dy, x + dx)
          if 0 <= ny && ny < h && 0 <= nx && nx < w
    } yield p

  val lowest =
    for { y <- 0 until h; x <- 0 until w
          value = nums(y)(x)
          if neighbors(y, x).map(getAt).forall{_ > value}
    } yield (y, x)

  if idx == 1 then {
    println(lowest.map(getAt).map{_ + 1}.sum)
  } else {
    def basinSizeFrom(t: Point) =
      val seen = Set[Point]()
      val bfs = ArrayDeque[Point](t)
      while !bfs.isEmpty do
        val p@(y, x) = bfs.removeHead(false)
        val value = nums(y)(x)

        for { n@(ny, nx) <- neighbors(y, x)
              if ny == y || nx == x
              nv = nums(ny)(nx)
              if !seen.contains(n)
              if nv != 9 && nv > value
        } bfs.addOne(n)

        seen += p
      seen.size

    println(lowest.map(basinSizeFrom).sorted.takeRight(3).product)
  }

