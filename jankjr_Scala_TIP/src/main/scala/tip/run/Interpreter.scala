package tip.run

import scala.collection.mutable.ArrayBuffer
import tip.AST._
import scala.collection.mutable.Map;
// Defines an environment
class Interpreter {
  class TIPTypeError(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
  class TIPUndefined(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)
  
  var environmentStack : ArrayBuffer[Map[String, RuntimeType]] = null;
  var void = Undefined();
  var zero = Integer(0);
  
  private def interpretOp( op: Operator, left : Int, right : Int ) : Integer = {
    op match {
      case Plus()      => Integer( left + right );
      case Minus()     => Integer( left - right );
      case Times()     => Integer( left * right );
      case Divide()    => Integer( left / right );
      case Eqq()       => if ( left == right ) Integer(1) else Integer(0);
      case GreatThan() => if ( left > right  ) Integer(1) else Integer(0);
      case _ => zero;
    }
  }
  private def createVar( id : String ) : Unit = {
    environmentStack.last += ( id -> void );
  }
  private def truthyValue( v:RuntimeType ) : Boolean = {
    v match {
      case Integer(value) => if ( value == 0 ) false else true;
      case _ => throw new TIPTypeError("While guard expression must be integer.");
    }
  }
  private def interpret( program : ASTNode ) : RuntimeType = {
    program match {
      case callExp:ACallFuncExpr => {
        val res = interpret( callExp.targetFun );
        val args = callExp.args.map { x => interpret(x) }
        res match {
          case fun : Function => call( fun, args );
          case _ => throw new TIPTypeError( "Expected Function." );
        }
      }
      case AIdentifier(id, _ ) => findOnEnv(id) 
      case op:ABinaryOp => {
        val left = interpret( op.left );
        val right = interpret( op.right );
        (left, right) match {
          case (l:Integer, r:Integer) => interpretOp( op.operator, l.value, r.value );
          case _ => throw new TIPTypeError( "Binary operator operands must be interer");
        }
      }
      case un:AUnaryOp => {
        ( interpret( un.target ), un.operator ) match {
          case ( Pointer(to), DerefOp() ) => to
          case ( to, RefOp() )            => Pointer(to)
          case _                          => throw new TIPTypeError( "" );
        }
      }
      case ANumber(value, offset) => Integer(value);
      case AInput(offset)         => void;
      case AoutputStmt(exp, off)  => {
        print( interpret( exp ).toOutputString() );
        Undefined()
      }
      case AReturnStmt(exp, off) => interpret(exp);
      case AVarStmt(decls, off)  => {
        decls.foreach( { x => createVar( x.value ) } )
        void;
      }
      case AAssignStmt(left, right, off) => {
        left match {
          case AIdentifier(id, _)  =>
            environmentStack.last.get(id) match{
              case Some(_) => environmentStack.last.update(id, interpret(right));
              case None    => throw new TIPUndefined( s"$id is not defined" );
            }
          case AUnaryOp(id:AIdentifier, op, _) =>
            environmentStack.last.get(id.value) match{
              case Some(ptr:Pointer) => ptr.to = interpret(right);
              case Some(_)           => throw new TIPTypeError( s"$id is not a pointer" );
              case None              => throw new TIPUndefined( s"$id is not defined" );
            }
          case _ => 
        }
        void;
      }
      case ABlockStmt( stms, _ ) => {
        stms.foreach(interpret);
        void
      }
      case AIfStmt( guard, then, el, _ ) => {
        val g = interpret( guard );
        (truthyValue(g), then, el) match {
          case (true, exp, _ )       => interpret(exp)
          case (false, _, Some(exp)) => interpret(exp)
          case _ =>
        }
        void;
      }
      case AMalloc(_) => Pointer(void);
      case ANull(_)   => void;
      case AWhileStmt(exp, body, off) => {
        while( truthyValue( interpret( exp ) ) ) interpret( body );
        void;
      }
      case _ => void
    }
  }
  
  private def defineFunction( fun : AFunDeclaration ) : Map[String, RuntimeType] = {
    environmentStack.last += ( fun.name.value -> Function( fun.args.map( _.value ), fun.stmts ) ) 
  }
  
  private def findFunction( id : String ) : Function = {
    environmentStack.head.get( id ) match {
      case Some(fun:Function) => fun;
      case Some(_)            => throw new TIPTypeError( s"$id is not a function");
      case _                  => throw new TIPUndefined( s"$id is not defined" );
    }
  }
  private def findOnEnv( id : String ) : RuntimeType = {
    environmentStack.last.get( id ) match {
      case Some(v) => v;
      case _ => throw new TIPUndefined( s"Cannot find $id" );
    }
  }
  
  private def call( fun : Function, args : Seq[ RuntimeType ] ) : RuntimeType = {
    val newEnv = fun.params.zip( args )
                           .foldLeft(Map[String, RuntimeType]())( ( env, nameValue ) => {
      env + nameValue
    } );
    environmentStack.append( newEnv );
    
    
    
    fun.body.content.dropRight( 1 )
                    .foreach( interpret );
    
    val result = interpret( fun.body.content.last );
    environmentStack.dropRight( 1 );
    return result;
  }
  
  def run( program : AProgram ) : RuntimeType = {
    environmentStack = ArrayBuffer[Map[String, RuntimeType]]();
    environmentStack.append( Map[String, RuntimeType]() );
    program.fun.foreach( defineFunction );
    val main = findFunction( program.fun.last.name.value ).asInstanceOf[Function];
    call( main, List.empty );
  }
}