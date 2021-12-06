import scala.io.Source.stdin

def compact(ls: Iterable[(Long, Long)]) =
  ls.foldLeft{Map[Long, Long]()}
             {(m, t) => m.updated(t._1, m.getOrElse(t._1, 0L) + t._2)}

@main def main(idx: Int) =
  val xs = stdin.getLines.next.split(',').map{_.toLong}.map{(_, 1L)}
  val top = if idx == 1 then 80 else 256
  print {
    (1 to top).foldLeft{compact(xs)}{(m, _) =>
      compact(m.to(Iterable).flatMap{
        case (0L, v) => Iterator((6L, v), (8L, v))
        case (k, v)  => Iterator((k - 1, v))
      })
    }.values.sum
  }

