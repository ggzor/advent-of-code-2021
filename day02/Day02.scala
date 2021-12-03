import scala.io.Source.stdin

@main def main(idx: Int) =
  val xs = stdin.getLines().map{_.split(" ")}.map{x => (x(0), x(1).toInt)}

  if idx == 1 then
    var x = 0; var y = 0
    for ((dir, n) <- xs)
      dir match {
        case "forward" => x += n
        case "down" => y += n
        case "up" => y -= n
      }
    println(x * y)
  else
    var x = 0; var y = 0; var aim = 0
    for ((dir, n) <- xs)
      dir match {
        case "down" => aim += n
        case "up" => aim -= n
        case "forward" =>
          x += n
          y += n * aim
      }
    println(x * y)

