package effects

import Control._
import scala.annotation.tailrec

sealed trait Control[A] {
  def map[B](f: A => B): Control[B] = flatMap((a: A) => Pure(f(a)))
  def flatMap[B](f: A => Control[B]): Control[B] = Bind(this, f(_))
  def andThen[B](c: Control[B]): Control[B] = Bind(this, (_: A) => c)

  def >>=[B](f: A => Control[B]): Control[B] = flatMap(f)
  def *>[B](c: Control[B]): Control[B] = andThen(c)
}

object Control {

  final case class Pure[A](value: A) extends Control[A]
  final case class Delay[A](thunk: () => A) extends Control[A]
  final case class Bind[A, B](fa: Control[A], f: A => Control[B]) extends Control[B]
  final case class Cont[A, B, C](handler: Handler[C], f: (A => Control[B]) => Control[C]) extends Control[A]
  final case class Prompt[A, B](handler: Handler[B], inner: Control[A]) extends Control[B]

  def apply[A](a: A): Control[A] = Pure(a)
  def delay[A](a: => A): Control[A] = Delay(() => a)

  type Func = Any => Control[Any]

  private sealed trait Step
  private final case class Computation(f: Func) extends Step
  private final case class Mark(handler: Handler[_]) extends Step

  private val controlId: Func = (x: Any) => Pure(x)

  private final class Pending(var steps: List[Step] = List.empty) { self =>
    def cont(mark: Handler[_]): Func = {

      @tailrec
      def loop(acc: Func, steps: List[Step]): Func = steps match {
        case Nil => throw new RuntimeException("internal error")
        case head :: tail => head match {
          case Computation(f) =>
            val newAcc = (x: Any) => acc(x).flatMap(f)
            loop(newAcc, tail)
          case Mark(h) =>
            if (h == mark) {
              self.steps = steps
              acc
            } else {
              val newAcc = (x: Any) => Prompt(h, acc(x)).asInstanceOf[Control[Any]]
              loop(newAcc, tail)
            }
        }
      }

      loop(controlId, steps)
    }

    @tailrec
    def pop(): Option[Func] = steps match {
      case head :: tail => head match {
        case s: Computation => steps = tail; Some(s.f)
        case Mark(handler) =>
          handler match {
            case h: StateHandler[_] => h.restore()
            case _ =>
          }
          steps = tail
          pop()
      }
      case Nil => None
    }

    def push(step: Step): Unit = steps = step +: steps
  }

  def run[A](p: Control[A]): A = {
    val pending = new Pending

    @tailrec
    def loop(c: Control[Any]): Any = c match {
      case Pure(v) => pending.pop() match {
        case None => v
        case Some(f) => loop(f(v))
      }
      case Delay(thunk) => pending.pop() match {
        case None => thunk()
        case Some(f) => loop(f(thunk()))
      }
      case Bind(fa, f) =>
        pending.push(Computation(f.asInstanceOf[Func]))
        loop(fa)
      case Prompt(handler, inner) =>
        pending.push(Mark(handler))
        handler match {
          case h: StateHandler[_] => h.backup()
          case _ =>
        }
        loop(inner)
      case Cont(handler, f) =>
        loop(f(pending.cont(handler)))
    }

    loop(p.asInstanceOf[Control[Any]]).asInstanceOf[A]
  }
}
