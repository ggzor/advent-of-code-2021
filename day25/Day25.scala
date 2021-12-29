import scala.io.Source.stdin

@main def main(idx: Int) =
  var xs = ( for { (r, y) <- stdin.getLines().zipWithIndex
                   (v, x) <- r.zipWithIndex
                   if v == '>' || v == 'v'
                 } yield ((y, x), v) ).toMap
  val h = xs.keys.map{_._1}.max + 1
  val w = xs.keys.map{_._2}.max + 1

  val modifier = Map('>' -> (0, 1), 'v' -> (1, 0))
  def nextCoord(coord: (Int, Int), dir: Char) =
    val ((y, x), (dy, dx)) = (coord, modifier(dir))
    ((y + dy) % h, (x + dx) % w)

  def advance(c: Char, m: Map[(Int, Int), Char]) =
    val (target, other) = m.partition{ case (coord, v) =>
                              c == v && '.' == m.getOrElse(nextCoord(coord, v), '.')}
    (!target.isEmpty, other ++ target.view.map{case (k, v) => (nextCoord(k, v), v)})

  println {
    Iterator.from(1).scanLeft(Option(xs)){(t, _) => t match {
      case Some(xs) =>
        val (modifiedRight, right) = advance('>', xs)
        val (modifiedDown, down) = advance('v', right)

        if modifiedRight || modifiedDown
          then Some(down)
          else None
      case _ => None
    }}.zipWithIndex.takeWhile(!_._1.isEmpty).flatMap{case (v, i) => v.map{(_, i)}}
      .to(LazyList).last._2 + 1
  }

