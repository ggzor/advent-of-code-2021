import scala.io.Source.stdin
import scala.collection.mutable.{Map as MutMap, Set as MutSet, PriorityQueue}

type Point = (Int, Int)

@main def main(idx: Int) =
  var xs = stdin.getLines().zipWithIndex.flatMap{
             case (l, y) => l.map{_.asDigit}.zipWithIndex.map{
               case (v, x) => ((y, x), v) }}.toMap
  def boundBy(f: Point => Int) = xs.keys.map(f).max + 1
  var h = boundBy(_._1)
  var w = boundBy(_._2)

  if (idx == 2) then
    xs = xs.flatMap{
      case ((y, x), v) =>
         for { sy <- 0 until 5; sx <- 0 until 5
               tv = v + sy + sx
               nv = if (tv <= 9) tv else tv - 9
         } yield ((y + sy * h, x + sx * w), nv)
      }
    h *= 5
    w *= 5

  val dist = xs.view.mapValues{_ => Int.MaxValue}.to(MutMap)
  val seen = MutSet[Point]()
  val q = new PriorityQueue(using Ordering[(Int, Point)].reverse)

  dist((0, 0)) = 0
  q.enqueue((0, (0, 0)))

  val neighbors = List((-1, 0), (0, -1), (0, 1), (1, 0))
  while !q.isEmpty do
    val (len, (y, x)) = q.dequeue()
    for { (dy, dx) <- neighbors
          np@(ny, nx) = (y + dy, x + dx)
          if 0 <= ny && ny < h && 0 <= nx && nx < w
    } do
      val old = dist(np)
      val new_ = len + xs(np)
      if new_ < old then
        dist(np) = new_
        q.enqueue((new_, np))

  println(dist((h - 1, w - 1)))

