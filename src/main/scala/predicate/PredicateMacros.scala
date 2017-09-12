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

  def replace[T <: Universe.TreeApi](tree: T)(pf: PartialFunction[T, T]): T = {
    val deep = tree match {
      case ReferenceToBoxed(ident) => ReferenceToBoxed(replace(ident, pf))
      case Alternative(trees) => Alternative(trees.map(x => replace(x, pf)))
      case Annotated(a, b) => Annotated(replace(a, pf), replace(b, pf))
      case AppliedTypeTree(a, l) => AppliedTypeTree(replace(a, pf), l.map(x => replace(x, pf)))
      case Apply(a) => Apply(replace(a, pf), l.map(x => replace(x, pf)))
      case Assign(a, b) => Assign(replace(a, pf), replace(b, pf))
      case AssignOrNamedArg(a, b) => AssignOrNamedArg(replace(a, pf), replace(b, pf))
      case Bind(n, a) => Bind(n, replace(a, pf))
      case Block(l, a) => Block(l.map(x => replace(x, pf)), replace(a, pf))
      case CaseDef(a, b, c) => CaseDef(replace(a, pf), replace(b, pf), replace(c, pf))
      case ClassDef(m, n, p, a) => ClassDef(replace(m, pf), replace(n, pf), p.map(x => replace(x, pf)), replace(a, pf))
      case CompoundTypeTree(a) => CompoundTypeTree(replace(a, pf))
      case DefDef(m, n, tp, vp, a, b) => ClassDef(replace(m, pf), replace(n, pf), tp.map(x => replace(x, pf)), vp.map(_.map(x => replace(x, pf))), replace(a, pf), replace(b, pf))
      case ExistentialTypeTree(a, l) => ExistentialTypeTree(replace(a, pf), l.map(x => replace(x, pf)))
      case Function(l, a) => Function(l.map(x => replace(x, pf)), replace(a, pf))
      // case Ident(a) => Ident(replace(a, pf))
      case If(a, b, c) => If(replace(a, pf), replace(b, pf), replace(c, pf))
      case Import(a, l) => Import(replace(a, pf), l.map(x => replace(x, pf)))
      case LabelDef(n, p, a) => LabelDef(n, p.map(x => replace(x, pf)), replace(a, pf))
      // case Literal(a) => Literal(replace(a, pf))
      case Match(a, l) => Match(replace(a, pf), l.map(x => replace(x, pf)))
      case ModuleDef(m, n, a) => ModuleDef(m, n, replace(a, pf))
      case New(a) => New(replace(a, pf))
      case PackageDef(a, l) => PackageDef(replace(a, pf), l.map(x => replace(x, pf)))
      case RefTree(a, n) => RefTree(replace(a, pf), n)
      case Return(a) => Return(replace(a, pf))
      case Select(a, n) => Select(replace(a, pf), n)
      case SelectFromTypeTree(a, n) => SelectFromTypeTree(replace(a, pf), n)
      case SingletonTypeTree(a) => SingletonTypeTree(replace(a, pf))
      case Star(a) => Star(replace(a, pf))
      case Super(a, n) => Super(replace(a, pf), n)
      case Template(p, a, b) => Template(p.map(x => replace(x, pf)), replace(a, pf), b.map(x => replace(x, pf)))
      // case This(a) => This(replace(a, pf))
      case Throw(a) => Throw(replace(a, pf))
      case Try(a, l, f) => Try(replace(a, pf), l.map(x => replace(x, pf)), replace(f, pf))
      case TypeApply(a, l) => TypeApply(replace(a, pf), l.map(x => replace(x, pf)))
      case TypeBoundsTree(a, b) => TypeBoundsTree(replace(a, pf), replace(b, pf))
      case TypeDef(m, n, l, a) => TypeDef(m, n, l.map(x => replace(x, pf)), replace(a, pf))
      // case TypeTree(a) => TypeTree(replace(a, pf))
      case Typed(a, b) => Typed(replace(a, pf), replace(b, pf))
      case UnApply(a, l) => UnApply(replace(a, pf), l.map(x => replace(x, pf)))
      case ValDef(m, n, a, b) => ValDef(m, n, replace(a, pf), replace(b, pf))
      case _ => tree
    }.asInstanceOf[T]
    pf.applyOrElse(deep, deep).asInstanceOf[T]
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
  
  def double_and_impl[T : WeakTypeTag, T2](g: c.Expr[T2 => Boolean]) = {
    println(s"Trying to make &&: ${c.prefix} && $g")
    val full = (c.prefix.tree, g) match {
      case (PredicateApply(NestedLambda(fa, fb)), NestedLambda(ga, gb)) if (fa equalsStructure ga) =>
        println("Woot! Same name arg!")
        q"{ $fa => $fb && $gb }"
      case _ =>
        val arg = c.freshName("predicateArg")
        val left = c.prefix.tree match {
          case PredicateApply(NestedLambda(fa, fb)) => q"{ val ${fa.name} = $arg; $fb }"
          case PredicateApply(f)                    => q"{ $f($arg) }"
          case _                                    => q"{ ${c.prefix}.f($arg) }"
        }
        println(s"got left: $left")
        val right = g match {
          case NestedLambda(ga, gb) => q"{ val ${ga.name} = $arg; $gb }"
          case _                    => q"{ $g($arg) }"
        }
        println(s"got right: $right")
        q"{ (${ValDef(Modifiers(Flag.PARAM, TermName(""), Nil), arg, TypeTree(weakTypeOf[T]), EmptyTree)}) => ($left && $right) }"
    }
    println(s"made full: $full")
    full
  }
}
