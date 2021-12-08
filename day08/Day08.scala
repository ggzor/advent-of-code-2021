import scala.io.Source.stdin
import scala.collection.mutable.Map

@main def main(idx: Int) =
  val xs = stdin.getLines.map{_.split('|').toList
                               .map{_.strip.split(' ').toList
                                     .map{_.toSet}}}.toList
  val knownLengths = Map(2 -> 1, 3 -> 7, 4 -> 4, 7 -> 8)

  var result = 0
  if idx == 1 then
    result = xs.map{_(1).count{s => knownLengths.contains(s.size)}}.sum
  else
    for { List(digits, target) <- xs } do
      val known = digits.flatMap{s => knownLengths.get(s.size).map(d => (d, s))}
                        .to(Map)

      val digitsOfLen = (len: Int) => digits.filter{s => s.size == len}
                                            .reduce{_ & _}

      val segs_adg = digitsOfLen(5)
      val segs_abfg = digitsOfLen(6)

      val seg_d = segs_adg &~ segs_abfg

      known(0) = known(8) &~ seg_d
      known(3) = segs_adg | known(1)
      known(5) = segs_abfg | seg_d

      known(2) = segs_adg | (known(8) &~ known(5))
      known(9) = known(5) | known(1)
      known(6) = known(5) | (known(8) &~ known(9))

      val mapping = known.iterator.map{_.swap}.toMap
      result += target.flatMap(mapping.get).mkString.toInt

  print(result)
