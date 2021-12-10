import scala.io.Source.stdin

val starting = "([{<"
val ending = ")]}>"

@main def main(idx: Int) =
  val first: Either[Char, List[Char]] = Right(List[Char]())
  val xs = stdin.getLines().map{
             _.foldLeft{first}{(r, c) => r match {
               case Right(l) if l.isEmpty || starting.contains(c) => Right(c :: l)
               case Right(prev :: l) => if prev == starting(ending.indexOf(c))
                                        then Right(l)
                                        else Left(c)
               case r => r
             }}}
  if idx == 1 then
    val scores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    print{xs.flatMap(_.left.toOption).map(scores).sum}
  else
    val result = xs.flatMap(_.toOption).map{_.map{c => starting.indexOf(c) + 1}
                                             .foldLeft{0L}((a, b) => a * 5 + b)}
                                       .toIndexedSeq.sorted
    print{result(result.size / 2)}

