import cats.Monad
import cats.kernel.Monoid
import cats.implicits._

// Haskell的话就是class Ref t m v | t -> m, t -> v
// 注意此处RefValue依赖Monad[M]
trait RefValue[T, M[_]: Monad, V] {
  def get(v: T): M[V]
}

given ref_option_int: RefValue[Option[Int], Option, Int] with {
  override def get(v: Option[Int]): Option[Int] = v
}

// 合法，但是希望不合法
given ref_option_int_2: RefValue[Option[Int], Option, Unit] with {
  override def get(v: Option[Int]): Option[Unit] = None
}

given ref_list_int: RefValue[List[Int], List, Int] with {
  override def get(v: List[Int]): List[Int] = v
}

def use_using[T, M[_], V](
    v: T
)(using ref: RefValue[T, M, V], m: Monad[M]): M[V] = m.map(ref.get(v))(identity)
def test_function_above = {
  /* 如果交换上面use_using的ref和m顺序，以下代码会报错：
   * ambiguous implicit arguments: both value catsStdInstancesForOption in trait OptionInstances and
   * value catsStdInstancesForLazyList in trait LazyListInstances match type cats.Monad[M] of parameter m of method use_usingbloop
   */
  use_using(List(1))
}

// 一种设想
trait Test[T] {
  type M[_]
  type V
  val monad: Monad[M]
  val monoid: Monoid[V]
}

given some_t(using
    option_monad: Monad[Option],
    int_monoid: Monoid[Int]
): Test[Option[Int]] with {
  type M[X] = Option[X]
  type V = Int
  val monad = option_monad
  val monoid = int_monoid
}

def test[T](using t: Test[T]): t.M[t.V] = {
  t.monad.pure(t.monoid.empty)
}

// 缺点：=:= 似乎不适用于F[_]，无法保证T1.M与T2.M相同
// 而如果是最初的写法，就可以非常简单地写
// def connect[T1, T2, M[_], V1, V2](t1: T1, t2: T2)(using ref1: RefValue[T1, M, V1], ref2: RefValue[T2, M, V2])
def connect[T1, T2](t1: T1, t2: T2)(using test1: Test[T1], test2: Test[T2])(
    using test1.V =:= test2.V
    //, test1.M =:= test2.M
): Unit = {}

@main def hello: Unit = {
  println(test)
}

def msg = "I was compiled by Scala 3. :)"
