package tip.run
import tip.AST._
abstract class RuntimeType {
  def toOutputString() : String = toString();
}
case class Integer( value : Int ) extends RuntimeType {
  override def toOutputString() = value.toChar.toString;
  override def toString() = value.toString();
}
case class Pointer( var to : RuntimeType ) extends RuntimeType {
  override def toString() = s"&${to.toString()}"
}
case class Function( params : Seq[String], body : ABlockStmt ) extends RuntimeType {
  override def toString() = s"proc(${params.mkString(", ")})"  
}
case class Undefined() extends RuntimeType {
  override def toString() = s"void"
}