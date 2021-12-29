import scala.io.Source.stdin

case class Range(start: Long, end: Long):
  def size = end - start + 1

  def &(other: Range): Option[Range] =
    val (a, b) = if (this.end > other.end) (other, this) else (this, other)
    if (a.end < b.start) None
    else if (a.start >= b.start) Some(a)
    else Range(a.start.max(b.start), a.end.min(b.end))

object Range:
  def apply(start: Long, end: Long): Option[Range] =
    if start <= end then Some(new Range(start, end)) else None

case class Cube(xs: Range, ys: Range, zs: Range):
  def size = xs.size * ys.size * zs.size

  def &(other: Cube): Option[Cube] =
    for { xs <- this.xs & other.xs; ys <- this.ys & other.ys; zs <- this.zs & other.zs}
      yield Cube(xs,ys,zs)

  def -(other: Cube): Iterator[Cube] =
    if (this & other).isEmpty then
      Iterator(this)
    else
      val alts = Iterator(
        ( Range(this.xs.start, other.xs.start - 1)
        , this.ys & other.ys
        , this.zs & other.zs
        ),
        ( Range(other.xs.end + 1, this.xs.end)
        , this.ys & other.ys
        , this.zs & other.zs
        ),
        ( Some(this.xs)
        , Range(this.ys.start, other.ys.start - 1)
        , this.zs & other.zs
        ),
        ( Some(this.xs)
        , Range(other.ys.end + 1, this.ys.end)
        , this.zs & other.zs
        ),
        ( Some(this.xs)
        , Some(this.ys)
        , Range(this.zs.start, other.zs.start - 1)
        ),
        ( Some(this.xs)
        , Some(this.ys)
        , Range(other.zs.end + 1, this.zs.end)
        ),
      )

      ( for { (oxs, oys, ozs) <- alts }
      yield for { xs <- oxs; ys <- oys; zs <- ozs }
              yield Cube(xs, ys, zs)
      ).flatten

val boundingCube = Cube(new Range(-50, 50), new Range(-50, 50), new Range(-50, 50))

@main def main(idx: Int) =
  val cubes = stdin.getLines()
                .map{ s => ( s.startsWith("on")
                           , raw"-?\d+".r.findAllIn(s).map{_.toLong}.toSeq)}
                .map{ case (action , Seq(x0,x1,y0,y1,z0,z1)) =>
                        ( action
                        , Cube(new Range(x0, x1), new Range(y0, y1), new Range(z0, z1)))
                      case _ => ??? }
                .flatMap{ case t@(b, c) => if idx == 1
                                              then (c & boundingCube).map{(b, _)}
                                              else Some(t)}

  val result = cubes.foldLeft(Vector[Cube]())((acc, t) => t match {
    case (true, newCube) =>
      acc ++ acc.foldLeft(Vector(newCube))((parts, old) => parts.flatMap(_ - old))
    case (false, newCube) => acc.flatMap(_ - newCube)
  })

  println(result.map{_.size}.sum)

