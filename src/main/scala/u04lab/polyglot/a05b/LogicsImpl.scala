package u04lab.polyglot.a05b

import u04lab.polyglot.Pair
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  var counter: Int = 0
  private val initial: Pair[Int, Int] = Pair(Random.nextInt(size - 2) + 1, Random.nextInt(size - 2) + 1)
  println(initial)

  override def tick(): Unit =
    counter = counter + 1

  override def isOver: Boolean =
    initial.getY - counter < 0 || initial.getY + counter >= size ||
      initial.getX - counter < 0 || initial.getX + counter >= size

  override def hasElement(x: Int, y: Int): Boolean =
   (x == initial.getX && Math.abs(y - initial.getY) <= counter) ||
     (y == initial.getY && Math.abs(x - initial.getX) <= counter) ||
     (x - y == initial.getX - initial.getY && Math.abs(x - initial.getX) <= counter) ||
     (x + y == initial.getX + initial.getX && Math.abs(x - initial.getX) <= counter)
