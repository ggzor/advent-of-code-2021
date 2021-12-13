import scala.io.Source.stdin

@main def main(idx: Int) =
  val (nums, folds) = stdin.getLines().span{_ != ""}
  val ns = nums.flatMap{_.split(",").map(_.toInt).sliding(2)
                         .map{case Array(a, b) => (a, b) }}.toSet
  val fs = folds.drop(1).flatMap{_.split("=").sliding(2)
                                  .map{case Array(a, b) => (a.last, b.toInt)}}.toList
  val List(w, h) = List('x', 'y').map{c => fs.filter{c == _._1}.map{_._2}.max * 2 + 1}

  val top = if (idx == 1) 1 else fs.length
  val (fw, fh, fns) = fs.take(top).foldLeft((w, h, ns)){(t, f) => (t, f) match {
    case ((w, h, nums), (dir, fold)) =>
      ( if (dir == 'x') w / 2 else w
      , if (dir == 'y') h / 2 else h
      , nums.map{case (x, y) => dir match {
          case 'x' if x >= fold => (w - x - 1, y)
          case 'y' if y >= fold => (x, h - y - 1)
          case _ => (x, y)
        }}
      )
  }}

  println{
    if idx == 1
      then fns.size
      else (0 until fh).map{y => (0 until fw).map{x =>
              if (fns.contains{(x, y)}) '#' else ' '}.mkString}
           .mkString("\n")
  }

