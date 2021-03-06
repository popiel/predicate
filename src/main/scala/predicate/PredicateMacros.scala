package predicate

import scala.language.experimental.macros

/** Implementation of symbolic logic meta-operators */
class PredicateMacroOps[T](val f: T => Boolean) extends AnyVal {
  // Try to get it to inline, in the forlorn hope that it might get optimized further
  // @inline final def unary_!                        = (x: T) => !f(x)
  @inline final def unary_! : T => Boolean                        = macro PredicateImpl.unary_not_impl[T]
  @inline final def &  [T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) &  g(x)
  @inline final def |  [T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) |  g(x)
  @inline final def ^  [T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) ^  g(x)

  // Short circuit
  // @inline final def && [T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) && g(x)
  @inline final def && [T2 >: T](g: T2 => Boolean): T => Boolean = macro PredicateImpl.double_and_impl[T,T2]
  @inline final def || [T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) || g(x)

  // == and != are already defined for Function1
  @inline final def ===[T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) == g(x)
  @inline final def !==[T2 >: T](g: T2 => Boolean) = (x: T) =>  f(x) != g(x)

  // implication, for those who don't remember boolean algebra
  @inline final def ==>[T2 >: T](g: T2 => Boolean) = (x: T) => !f(x) || g(x)
}
object PredicateMacroOps {
  @inline final implicit def apply[T](f: T => Boolean) = new PredicateMacroOps(f)
}

class PredicateImpl(val c: scala.reflect.macros.blackbox.Context) {
  import scala.reflect.api.Universe
  import c.universe._
  private object NestedLambda {
    def unapply(tree: AnyRef): Option[(ValDef, Tree)] = tree match {
      case Function(vparams, body) if vparams.size == 1 => Some((vparams(0), body))
      case Expr(t) => unapply(t)
      case Block(s, e) if s.isEmpty => unapply(e)
      case Typed(e, t) => unapply(e)
      case _ => None
    }
  }
  private object PredicateApply {
    def unapply(tree: AnyRef): Option[Tree] = tree match {
      case q"predicate.PredicateMacroOps.apply[$t]($a)" => Some(a)
      case Expr(t) => unapply(t)
      case Block(s, e) if s.isEmpty => unapply(e)
      case Typed(e, t) => unapply(e)
      case _ => None
    }
  }

  def unary_not_impl[T: WeakTypeTag] = {
    c.prefix.tree match {
      case PredicateApply(NestedLambda(a, f)) => q"{ $a => ! ($f) }"
      case _ => {
        val arg = c.freshName()
        q"{ ($arg:${weakTypeOf[T]}) => ! (${c.prefix}.f($arg)) }"
      }
    }
  }

  def identTransformer(from: TermName, to: Tree) = 
    new Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case Ident(`from`) => to
          case _ => super.transform(tree)
        }
      }
    }
  
  def double_and_impl[T : WeakTypeTag, T2](g: c.Expr[T2 => Boolean]) = {
    println(s"Trying to make &&: ${c.prefix} && $g")
    val t = new Traverser() {
      var nest = 0
      override def traverse(tree: Tree) {
        println((" " * nest) + tree.getClass.getName + ": " + tree.hashCode() + ": " + tree)
        nest += 2
        super.traverse(tree)
        nest -= 2
      }
    }
    t.traverse(c.prefix.tree)
    t.traverse(g.tree)
    val name   = c.freshName("predicateArg")
    val NestedLambda(valdef, ident) = c.parse(s"{ ($name:${show(weakTypeOf[T])}) => $name }")
//    val valdef = ValDef(Modifiers(Flag.PARAM, TermName(""), Nil), name, TypeTree(weakTypeOf[T]), EmptyTree)
//    val ident  = Ident(name)
    val left = c.prefix.tree match {
      case PredicateApply(NestedLambda(fa, fb)) => identTransformer(fa.name, ident).transform(fb)
      case PredicateApply(f) => q"{ $f($ident) }"
      case _ => q"{ ${c.prefix}.f($ident) }"
    }
    val right = g match {
      case NestedLambda(ga, gb) => identTransformer(ga.name, ident).transform(gb)
      case _ => q"{ $g($ident) }"
    }
    val full = q"{ ($valdef) => ($left && $right) }"
    println(s"made full: $full")
    full
  }
}
