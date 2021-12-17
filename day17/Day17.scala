import scala.io.Source.stdin

@main def main(idx: Int) =
  val Seq(xmin, xmax, ymin, ymax) = raw"-?\d+".r.findAllIn(stdin.getLines.next)
                                                .map{_.toInt}.toSeq
  var maxY, hitCount = 0
  for { sxv <- 0 to (xmax + 1); syv <- ymin to 300 } do
    var (x, y, xv, yv, curMaxY, hit) = (0, 0, sxv, syv, 0, false)
    while !(y < ymin || x > xmax || (xv == 0 && x < xmin)) do
      x += xv; y += yv
      xv -= xv.sign; yv -= 1
      curMaxY = curMaxY.max(y)
      hit |= xmin <= x && x <= xmax && ymin <= y && y <= ymax
    if hit then
      maxY = maxY.max(curMaxY)
      hitCount += 1

  println{ if (idx == 1) maxY else hitCount }

