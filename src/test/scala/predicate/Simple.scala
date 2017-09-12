package predicate

object Simple {
  import predicate.PredicateMacroOps._

  @inline final def isEven(x: String): Boolean = x.length % 2 == 0
  @inline final def isSquare(x: String): Boolean = {
    val root = Math.sqrt(x.length).toInt
    root * root == x.length
  }
  @inline final def isTriangle(x: String): Boolean = {
    val n = x.length
    val root = Math.sqrt(2 * n).toInt
    root * (root + 1) / 2 == n
  }

  def main(args: Array[String]) {
    val l = (1 to 100).map("x" * _)
    l.filter(x => x.length % 2 == 0)
    l.filter(isEven)
    l.filter(!(isEven _))
    l.filter((isEven _) && (isSquare _))
    l.filter(((isEven _) & (isSquare _)) | ((isEven _) & (isTriangle _)))
  }
}
