import scala.io.Source.stdin

type Path = Vector[Char]
type Leaf = (Int, Path)
type Tree = Vector[Leaf]

def parseTree(s: String): Tree =
  s.foldLeft((Vector[Leaf](), Vector[Char]()))((t, c) => t match {
    case (acc, p) => c match {
      case '[' => (acc, p :+ 'L')
      case ',' => (acc, p.init :+ 'R')
      case ']' => (acc, p.init)
      case c => (acc :+ (c.asDigit, p), p)
    }
  })._1

def addTrees(t1: Tree, t2: Tree) =
  t1.map{case (v, p) => (v, 'L' +: p)} ++ t2.map{case (v, p) => (v, 'R' +: p)}

def tryExplode(t: Tree): Option[Tree] =
  val (r, changed) = (None +: t.map{Some(_)} :+ None)
    .foldLeft((Vector[Option[Leaf]](), false))
     {(acc, right) => acc match {
       case (rest :+ left :+ Some((av, ap)) :+ Some((bv, bp)), false)
       if ap.length == 5 && ap.init == bp.init =>
         (rest :+ left.map{case (v, p) => (v + av, p)}
               :+ Some((0, ap.init))
               :+ right.map{case (v, p) => (v + bv, p)}
         , true)
       case (other, done) => (other :+ right, done)
     }}
  if (changed) Some(r.flatten) else None

def trySplit(t: Tree): Option[Tree] =
  val (r, changed) = t.foldLeft((Vector[Leaf](), false))
    {(acc, right) => (acc, right) match {
      case ((acc, false), (v, p)) if v >= 10 =>
        (acc :+ (v / 2, p :+ 'L') :+ (v - v / 2, p :+ 'R'), true)
      case ((other, done), _) => (other :+ right, done)
    }}
  if (changed) Some(r) else None

def reduce(t: Tree): Tree =
  Iterator.unfold(t){t => tryExplode(t).orElse(trySplit(t)).map{r => (r, r)}}
          .to(LazyList).lastOption.getOrElse(t)

def magnitude(t: Tree): Int =
  val idx = t.map{_.swap}.toMap
  def go(p: Path): Int =
    idx.getOrElse(p, 3 * go(p :+ 'L') + 2 * go(p :+ 'R'))
  go(Vector())

@main def main(idx: Int) =
  val xs = stdin.getLines().map{parseTree}.toVector
  println {
    if (idx == 1) magnitude(xs.reduce((a, b) => reduce(addTrees(a, b))))
    else xs.zip(xs.tails.drop(1))
           .flatMap{case (t1, l) => l.iterator.flatMap{t2 => List((t1, t2), (t2, t1))}}
           .map{addTrees.tupled.andThen(reduce).andThen(magnitude)}
           .max
  }

