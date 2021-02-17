package effects

object Eff {
  trait Amb {
    def flip(): Control[Boolean]
  }

  def ambList[T]: Amb with Handler[List[T]]  = new Amb with Handler[List[T]] {
    override def flip(): Control[Boolean] = use[Boolean] { resume =>
      for {
        left <- resume(true)
        right <- resume(false)
      } yield left ++ right
    }
  }

  trait State[T] {
    def get(): Control[T]
    def set(v: T): Control[Unit]
  }

  def state[T, R](init: T): State[T] with StateHandler[R] = new State[T] with StateHandler[R] {
    private val cell = Field(init)

    override def get(): Control[T] = use[T] { resume =>
      cell.value.flatMap(resume)
    }
    override def set(v: T): Control[Unit] = use[Unit] { resume =>
      for {
        _ <- cell.value = v
        r <- resume(())
      } yield r
    }
  }
}
