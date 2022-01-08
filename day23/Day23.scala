import scala.io.Source.stdin
import scala.collection.mutable.{Set as MutSet, PriorityQueue}
import scala.util.control.Breaks._

type Board = Map[(Int, Int), Char]

val moveCosts = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
val targets = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
val targetCols = targets.values.toSet

def oneSide(h: Int, w: Int, b: Board, v: Char, xIter: Iterable[Int]) =
  xIter.takeWhile{xi => b.getOrElse((1, xi), '.') == '.'}
       .flatMap{xi =>
         if (xi == targets(v)) then
          val sideRoom = (2 to h - 2).map{yi => (yi, xi)}
          val (start, end) = sideRoom.span{p => b.getOrElse(p, '.') == '.'}

          if end.forall{p => b.getOrElse(p, '.') == v}
          then start.lastOption.iterator
          else Iterator.empty
         else if (!targetCols.contains(xi))
           Iterator((1, xi))
         else
           Iterator.empty
       }

def distance(src: (Int, Int), to: (Int, Int)) =
  val ((y1, x1), (y2, x2)) = (src, to)
  (x1 - x2).abs + (y1 - 1) + (y2 - 1)

def nextMovements(h: Int, w: Int, b: Board) =
  def targetsFor(y: Int, x: Int, v: Char) =
    val left = oneSide(h, w, b, v, x - 1 to 1 by -1)
    val right = oneSide(h, w, b, v, x + 1 to w - 2)
    val bothSides = left ++ right

    if y >= 2
       && (x != targets(v)
           || (y + 1 to h - 2).exists{yi => b.getOrElse((yi, x), '.') != v})
       && (y - 1 to 2 by -1).forall{yi => b.getOrElse((yi, x), '.') == '.'}
    then
      bothSides
    else if y == 1 then
      bothSides.filter{case (y, x) => y != 1}
    else
      Iterator.empty

  for { (src@(y, x), v) <- b.iterator // Implicit Map comprehensions bite hard
        to <- targetsFor(y, x, v)
  } yield (distance(src, to) * moveCosts(v), (src, to))

@main def main(idx: Int) =
  val xs = { for { (r, y) <- stdin.getLines().zipWithIndex
                   (v, x) <- r.zipWithIndex
                   if "ABCD".contains(v)
                 } yield ((y, x), v)
           }.toMap

  val h = if (idx == 1) 5 else 7
  val w = 13

  var best: Option[Int] = None

  val q = new PriorityQueue(using Ordering[Int].on[(Int, Board)](x => x._1).reverse)
  q.addOne((0, xs))

  val seen = MutSet[Board]()

  breakable {
    while !q.isEmpty do
      val (dist, board) = q.dequeue

      if !seen.contains(board) then
        seen.add(board)

        if board.forall{case ((y, x), v) => y >= 2 && x == targets(v)} then
          best = Some(dist)
          break

        for { (cost, (src, to)) <- nextMovements(h, w, board)
              nb = (board - src) + (to -> board(src))
              if !seen.contains(nb)
            } do q.addOne((dist + cost, nb))
  }

  println(best.getOrElse(0))

