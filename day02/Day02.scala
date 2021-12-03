import scala.io.Source.stdin

@main def main(idx: Int) =
  val xs = stdin.getLines().map{_.split(" ")}.map{x => (x(0), x(1).toInt)}

  var x, y, aim = 0
  if idx == 1 then
    for ((dir, n) <- xs)
      dir match {
        case "forward" => x += n
        case "down" => y += n
        case "up" => y -= n
      }
    println(x * y)
  else
    for ((dir, n) <- xs)
      dir match {
        case "down" => aim += n
        case "up" => aim -= n
        case "forward" =>
          x += n
          y += n * aim
      }
    println(x * y)

