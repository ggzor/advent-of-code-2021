import scala.io.Source.stdin
import scala.util.control.Breaks._

enum Payload:
  case Literal(value: Long)
  case Operator(subpackets: Vector[Packet])

case class Packet(version: Long, ptype: Long, payload: Payload)

def parseBin(it: Iterator[Char]) = java.lang.Long.parseLong(it.mkString, 2)

def parse(it: Iterator[Char]): Packet =
  val version = parseBin(it take 3)
  val ptype = parseBin(it take 3)

  val payload = {
    if (ptype == 4) then
      var sub = ""
      breakable {
        while true do
          val shouldEnd = it.next == '0'
          sub ++= it.take(4)
          if shouldEnd then
            break
      }
      Payload.Literal(java.lang.Long.parseLong(sub, 2))
    else
      val subpackets = it.next match
        case '0' =>
          val subLen = parseBin(it.take(15))
          val subData = it.take(subLen.toInt).buffered

          var sub = Vector[Packet]()
          while subData.hasNext do
            sub :+= parse(subData)
          sub
        case '1' =>
          val subLen = parseBin(it.take(11))

          var sub = Vector[Packet]()
          for {_ <- 1L to subLen} do
            sub :+= parse(it)
          sub
      Payload.Operator(subpackets)
  }

  Packet(version, ptype, payload)

def sumVersions(p: Packet): Long = p match
  case Packet(v, _, Payload.Literal(_)) => v
  case Packet(v, _, Payload.Operator(ps)) => v + ps.map(sumVersions).sum

def eval(p: Packet): Long = p match
  case Packet(_, _, Payload.Literal(v)) => v
  case Packet(_, ty, Payload.Operator(ps)) =>
    val sub = ps.map(eval)

    def withFirstTwo(op: (Long, Long) => Boolean, ps: Seq[Long]) =
      if (op(ps(0), ps(1))) 1 else 0

    val f: Seq[Long] => Long = ty match
      case 0 => _.sum
      case 1 => _.product
      case 2 => _.min
      case 3 => _.max
      case 5 => withFirstTwo(_ > _, _)
      case 6 => withFirstTwo(_ < _, _)
      case 7 => withFirstTwo(_ == _, _)

    f(sub)

@main def main(idx: Int) =
  val xs = stdin.getLines().next.map{
             c => ("0000" + Integer.parseInt(c.toString, 16).toBinaryString)
                  .takeRight(4)}.mkString
  val result = parse(xs.iterator)
  println{if idx == 1 then sumVersions(result) else eval(result)}

