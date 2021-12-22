import scala.io.Source.stdin

@main def main(idx: Int) =
  val xs = stdin.getLines().map{_.split(' ').last.toInt}.toVector
  val dice = if idx == 1
             then Iterator.continually(1 to 100).flatten.map(x => Iterator(x))
             else Iterator.continually(Iterator(1, 2, 3))
  val top = if (idx == 1) 1000 else 21

  var universes = Map((xs, Vector(0, 0)) -> 1L)
  var previousUniverses = universes.empty
  var turn, rollCount = 0
  var counts = Vector(0L, 0L)
  while !universes.isEmpty do
    val (newUniverses, newCounts) = {
      for { r1 <- dice.next; r2 <- dice.next; r3 <- dice.next
            total = r1 + r2 + r3
            ((positions, scores), count) <- universes
            p = ((positions(turn) - 1) + total) % 10 + 1
            s = scores(turn) + p
          } yield ((positions.updated(turn, p), scores.updated(turn, s)), count)
    }.foldLeft(universes.empty, Array(0L, 0L)){ (t, item) => (t, item) match {
      case ((us, counts), ((ps, ss), c)) =>
        if ss.max >= top then
          if (ss(0) > ss(1)) counts(0) += c else counts(1) += c
          (us, counts)
        else
          ( us.updatedWith((ps, ss)){ case None => Some(c)
                                      case Some(prev) => Some(prev + c) }
          , counts)

    }}
    counts = counts.lazyZip(newCounts).map{case (x, y) => x + y}.toVector

    previousUniverses = universes
    universes = newUniverses
    turn = 1 - turn
    rollCount += 3

  println { if idx == 1
            then previousUniverses.keys.head._2.min * rollCount
            else counts.max }

