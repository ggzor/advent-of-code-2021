import scala.io.Source.stdin

@main def main(idx: Int) =
  var xs = stdin.getLines().map{_.toInt}
  if idx == 2 then xs = xs.sliding(3).map{_.sum}
  println(xs.sliding(2).count{w => w(0) < w(1)})
