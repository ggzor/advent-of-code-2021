import scala.io.Source.stdin

@main def main(idx: Int) =
  val xs = stdin.getLines().next.split(',').map{_.toLong}
  val f : Long => Long = if idx == 1 then identity else (x => x * (x + 1) / 2)
  print((0L to xs.max).map{i => xs.map{j => f((i - j).abs)}.sum}.min)

