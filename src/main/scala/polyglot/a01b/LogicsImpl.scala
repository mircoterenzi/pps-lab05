package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.Sequence
import util.Streams.Stream

import scala.jdk.javaapi.OptionConverters

trait Logics:
  def hit(x: Int, y: Int): java.util.Optional[Integer]
  def won(): Boolean

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private case class pair(x: Int, y: Int)
  private var minesSet: Sequence[pair] = Sequence.Nil()
  private var selected: Sequence[pair] = Sequence.Nil()
  private val random = scala.util.Random()
  while (minesSet.size() != size)
    minesSet = Sequence.Cons(pair(random.nextInt(size), random.nextInt(size)), minesSet)
  println(minesSet)

  private def neighbours(x: Int, y: Int): Int = Stream.iterate(x - 1)(_ + 2)
    .flatMap(xx => Stream.iterate(y - 1)(_ + 2).map(yy => pair(xx, yy)))
    .toList
    .filter(minesSet.contains(_))
    .size()

  override def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val p = pair(x, y)
    OptionToOptional(if minesSet.contains(p) then ScalaOptional.Empty() else
      selected = Sequence.Cons(p, selected)
      ScalaOptional.Just(neighbours(x, y))
    )

  override def won(): Boolean = selected.size() + minesSet.size() == size * size
