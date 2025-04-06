package polyglot.a05b

import polyglot.a05b.Logics

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:
  private var count = 0
  private case class pair(x: Int, y: Int)
  private val random = scala.util.Random()
  private val initial = pair(random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)

  override def tick(): Unit = count = count + 1

  override def isOver: Boolean =
    initial.y - count < 0 || initial.y + count >= size ||
    initial.x - count < 0 || initial.x + count >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initial.x && Math.abs(y - initial.y) <= count) ||
    (y == initial.y && Math.abs(x - initial.x) <= count) ||
    (x - y == initial.x - initial.y && Math.abs(x - initial.x) <= count) ||
    (x + y == initial.x + initial.y && Math.abs(x - initial.x) <= count)
