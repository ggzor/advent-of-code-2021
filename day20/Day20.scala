import scala.io.Source.stdin

@main def main(idx: Int) =
  val lines = stdin.getLines().map{_.map{_ == '#'}}
  val algo = lines.next
  val img = lines.drop(1).zipWithIndex.flatMap{
    case (r, y) => r.zipWithIndex.map{ case (v, x) => ((y, x), v) }}.toMap

  def binToInt(bs: Iterable[Boolean]) = bs.foldLeft(0){(a,b) => a * 2 + (if (b) 1 else 0)}
  val top = if (idx == 1) 2 else 50
  val result = (1 to top).foldLeft(img){(img, i) =>
    val empty = if (i % 2 == 0) algo(0) else algo(binToInt((1 to 9).map(_ => algo(0))))
    val minY = img.keys.map{_._1}.min
    val maxY = img.keys.map{_._1}.max
    val minX = img.keys.map{_._2}.min
    val maxX = img.keys.map{_._2}.max

    (for { y <- minY - 1 to maxY + 1; x <- minX - 1 to maxX + 1
           value = algo(binToInt(for { dy <- -1 to 1; dx <- -1 to 1 }
                                   yield img.getOrElse((y + dy, x + dx), empty)))
         } yield ((y, x), value)
    ).toMap
  }
  print(result.values.count(identity))

