import scala.io.Source.stdin
import scala.annotation.tailrec

val PERMUTATIONS = Array(
    1, 2, 3,
    -3, 2, 1,
    -1, 2, -3,
    3, 2, -1,
    -1, -2, 3,
    -3, -2, -1,
    1, -2, -3,
    3, -2, 1,
    -2, 1, 3,
    -2, -3, 1,
    -2, -1, -3,
    -2, 3, -1,
    2, -1, 3,
    2, -3, -1,
    2, 1, -3,
    2, 3, 1,
    1, -3, 2,
    -3, -1, 2,
    -1, 3, 2,
    3, 1, 2,
    1, 3, -2,
    -3, 1, -2,
    -1, -3, -2,
    3, -1, -2,
    ).sliding(3).toList

type Vec3 = Array[Int]
type Scanner = Array[Vec3]

extension (v1: Vec3)
  def componentWise(v2: Vec3, f: (Int, Int) => Int): Vec3 =
    v1.lazyZip(v2).map{case (a, b) => f(a, b)}.toArray
  def -(v2: Vec3): Vec3 = v1.componentWise(v2, _ - _)

def find_overlap(s1: Scanner, s2: Scanner): Option[(Scanner, Vec3)] =
  {for { p <- PERMUTATIONS.iterator
        permuted = s2.map{v => p.map{i => v(i.abs - 1) * i.sign}}
        v1 <- s1.iterator
        v2 <- permuted.iterator
        diff = v2 - v1
        result = permuted.map{_ - diff}
        count = (for { a <- s1.iterator; b <- result; if a.sameElements(b) } yield 1).sum
        if count >= 12
  } yield (result, diff)
  }.nextOption

def relativize(s: IndexedSeq[Scanner]): Seq[(Scanner, Vec3)] =
  @tailrec
  def go(distances: Map[Int, (Scanner, Vec3)], next: List[Int]
                           , remaining: Set[Int]): Map[Int, (Scanner, Vec3)] =
    if next.isEmpty then
      distances
    else
      val cur :: rest = next
      val newOnes = remaining.toList.flatMap(
                      b => find_overlap(distances(cur)._1, s(b)).map{r => (b, r)})
      go(distances ++ newOnes
        , newOnes.map{_._1} ++ rest
        , remaining -- newOnes.map{_._1})

  go(Map(0 -> (s(0), Array(0, 0, 0))), List(0), Set(1 until s.length: _*)).values.toList

@main def main(idx: Int) =
  val xs = stdin.mkString.split("\n\n")
                .map{_.split('\n').drop(1)
                  .map{a => a.split(',').map{_.toInt}}}.toIndexedSeq
  val result = relativize(xs)

  println {
    if idx == 1 then
      result.flatMap{_._1.map{_.toList}}.toSet.size
    else
      val diffs = result.map{_._2}
      ( for { d1 <- diffs
              ds <- diffs.tails.to(LazyList).drop(1).dropRight(1)
              d2 <- ds
            } yield (d1 - d2).map{_.abs}.sum
      ).max
  }

