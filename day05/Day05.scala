import scala.io.Source.stdin

def step(a: Int, b: Int) = if (a < b) a + 1 else if (a > b) a - 1 else a

@main def main(idx: Int) =
    val criteria : (a: Array[Int]) => Boolean =
      if idx == 1 then {case Array(x1,y1,x2,y2) => x1 == x2 || y1 == y2}
                  else _ => true
    println {
      stdin.getLines()
           .map{_.split(" -> ").flatMap(_.split(",").map(_.strip.toInt))}
           .filter(criteria)
           .flatMap{case Array(x1,y1,x2,y2) => {
             var result: List[(Int, Int)] = List()
             var x = x1
             var y = y1

             while ! (x == x2 && y == y2) do
               result = (y, x) :: result
               x = step(x, x2)
               y = step(y, y2)
             result = (y, x) :: result

             result
           }}
           .to(Iterable).groupBy(t => t).map{_._2.size}.filter{_ > 1}
           .size
    }

