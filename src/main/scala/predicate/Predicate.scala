package predicate

/** Implementation of symbolic logic meta-operators */
object PredicateOps {
  @inline final implicit def apply[T](f: T => Boolean): PredicateOps[T] = new PredicateOps(f)
}

class PredicateOps[T](val f: T => Boolean) extends AnyVal {
  // Try to get it to inline, in the forlorn hope that it might get optimized further
  @inline final def unary_!                        = (x: T) => !f(x)
  @inline final def &  [T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) &  g(x)
  @inline final def |  [T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) |  g(x)
  @inline final def ^  [T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) ^  g(x)

  // Short circuit
  @inline final def && [T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) && g(x)
  @inline final def || [T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) || g(x)

  // == and != are already defined for Function1
  @inline final def ===[T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) == g(x)
  @inline final def !==[T2 >: T](g: Predicate[T2]) = (x: T) =>  f(x) != g(x)

  // implication, for those who don't remember boolean algebra
  @inline final def ==>[T2 >: T](g: Predicate[T2]) = (x: T) => !f(x) || g(x)
}
