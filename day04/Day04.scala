import scala.collection.mutable.{ArraySeq, Set}
import scala.io.Source.stdin
import scala.util.control.Breaks._

type Board = ArraySeq[ArraySeq[(Int, Boolean)]]
val BOARD_SIZE = 5

@main def main(idx: Int) =
  val lines = stdin.getLines()
  val nums = lines.next.split(",").map{_.toInt}
  val boards = lines.grouped(BOARD_SIZE + 1)
                    .map{_.drop(1).map{_.split(" ").flatMap{_.toIntOption}
                          .map{(_, false)}.to(ArraySeq)}.to(ArraySeq)}
                    .toIndexedSeq
  val reverseIdx = boards.map{_.zipWithIndex
                               .flatMap{case (row, i) =>
                                 row.zipWithIndex
                                    .map{case ((value, _), j) => ((value, (i, j)))}}
                               .toMap}

  val isCompleted = (b: Board) => (b.iterator ++ b.transpose).exists(_.forall{_._2})
  val score_board = (b: Board) => b.flatMap{_.filter{_._2 == false}}.map{_._1}.sum

  // Part two, stop iteration early only for the first part
  var should_stop_first = idx == 1

  var completed: Set[Int] = Set()
  var last_board: Board = null
  var last_num : Int = 0
  breakable {
    for (n <- nums)
      last_num = n
      for (((b, revIdx), idx) <- boards.iterator.zip(reverseIdx).zipWithIndex)
        last_board = b
        revIdx.get(n).map{case (row, col) => b(row)(col) = (b(row)(col)._1, true)}
        if (!completed.contains(idx) && isCompleted(b))
            completed += idx
            if should_stop_first || completed.size == boards.length
               then break
  }

  print(score_board(last_board) * last_num)
