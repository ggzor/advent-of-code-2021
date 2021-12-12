import scala.io.Source.stdin

type Node = String
@main def main(idx: Int) =
  val g = stdin.getLines().flatMap{_.split("-").sliding(2).flatMap{
                             case Array(a, b) => List((a, b), (b, a))}}.toSet

  def dfsOne(cur: Node = "start", seen: Set[Node] = Set()): Int =
    if seen.contains(cur) then return 0
    if cur == "end" then return 1
    val newSeen = if cur.forall{_.isLower} then seen + cur else seen
    g.iterator.filter{_._1 == cur}.map{case (_, n) => dfsOne(n, newSeen)}.sum

  def dfsTwo(cur: Node = "start", startVisited: Boolean = false,
             counts: Map[String, Int] = Map()) : Int =
    if (cur == "start" && startVisited)
      || counts.values.count(_ == 2) > 1
      || counts.getOrElse(cur, 0) == 2 then return 0
    if cur == "end" then return 1
    val newCounts = if cur.forall{_.isLower} && cur != "start"
                    then counts.updatedWith(cur){_.map(_ + 1).orElse(Some(1))}
                    else counts
    g.iterator.filter{_._1 == cur}.map{case (_, n) => dfsTwo(n, true, newCounts)}.sum

  println { if idx == 1 then dfsOne() else dfsTwo() }

