package effects

import Control._

trait Handler[Res] {
  def apply(p: this.type => Control[Res]): Control[Res] = handle(p)
  def handle(p: this.type => Control[Res]): Control[Res] = Prompt(this, p(this))
  def use[A](p: (A => Control[Res]) => Control[Res]): Control[A] = Cont(this, p)
}

trait StateHandler[Res] extends Handler[Res] {

  def backup(): Unit = _backup = data
  def restore(): Unit = data = _backup

  private var data = Map.empty[Field[_], Any]
  private var _backup: Map[Field[_], Any] = _

  // from scala-effekt
  def Field[T](value: T): Field[T] = {
    val field = new Field[T]()
    data = data.updated(field, value)
    field
  }

  // all the field data is stored in `data`
  class Field[T] () {
    def value: Control[T] = Control(data(this).asInstanceOf[T])
    def value_=(value: T): Control[Unit] = Control.delay {
      data = data.updated(this, value)
    }
    def update(f: T => T): Control[Unit] = for {
      old <- value
      _   <- value_=(f(old))
    } yield ()
  }
}
