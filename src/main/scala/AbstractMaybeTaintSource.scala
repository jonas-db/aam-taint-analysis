import UnaryOperator._
import BinaryOperator._
import scala.util.parsing.input.Position

/** Taint lattice */
trait AbstractMaybeTaintSource {
  def isTrue: Boolean = true
  def isFalse: Boolean = false 
  def isError: Boolean = false
  
  def unaryOp(op: UnaryOperator): AbstractMaybeTaintSource = op match {
    case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean 
      => AbstractMaybeTaintSource.AbstractUntainted
    case Not => this 
  }
  
  def binaryOp(op: BinaryOperator)(that: AbstractMaybeTaintSource): AbstractMaybeTaintSource = checkTaint(that)
  
  def checkTaint(that: AbstractMaybeTaintSource):AbstractMaybeTaintSource = (this, that) match { 

    //top,top
      case (AbstractMaybeTaintSource.AbstractTop(x), AbstractMaybeTaintSource.AbstractTop(y)) =>
            AbstractMaybeTaintSource.AbstractTop(x ++ y) 
 //top,taint
      case (AbstractMaybeTaintSource.AbstractTop(x), AbstractMaybeTaintSource.AbstractTainted(y)) =>
            AbstractMaybeTaintSource.AbstractTop(x ++ y)             
 //taint, top
      case (AbstractMaybeTaintSource.AbstractTainted(x), AbstractMaybeTaintSource.AbstractTop(y)) =>
            AbstractMaybeTaintSource.AbstractTop(x ++ y)             

//taint, taint
      case (AbstractMaybeTaintSource.AbstractTainted(x), AbstractMaybeTaintSource.AbstractTainted(y)) =>
            AbstractMaybeTaintSource.AbstractTainted(x ++ y)             
//top, untaint/Bot
            
      case (AbstractMaybeTaintSource.AbstractTop(x), _) =>
            AbstractMaybeTaintSource.AbstractTop(x)
//untaint/Bot, top
      case (_, AbstractMaybeTaintSource.AbstractTop(x)) =>
            AbstractMaybeTaintSource.AbstractTop(x)            

// taint, untaint/Bot            
      case (AbstractMaybeTaintSource.AbstractTainted(x), _) =>
            AbstractMaybeTaintSource.AbstractTainted(x)     
            
// untaint/Bot,taint
      case (_, AbstractMaybeTaintSource.AbstractTainted(x)) =>
            AbstractMaybeTaintSource.AbstractTainted(x)   

            case (x, AbstractMaybeTaintSource.AbstractBottom) => AbstractMaybeTaintSource.AbstractBottom
              case (AbstractMaybeTaintSource.AbstractBottom, x) => AbstractMaybeTaintSource.AbstractBottom
     case _ =>  AbstractMaybeTaintSource.AbstractUntainted            
      
            //      case AbstractMaybeTaintSource.AbstractTainted(x) => AbstractMaybeTaintSource.AbstractTainted(x)
//      
//      // this not tainted, bottom or untainted
//      case _ => that match { 
//          // that is tainted
//          case AbstractMaybeTaintSource.AbstractTop(x) => AbstractMaybeTaintSource.AbstractTop(x)
//          case AbstractMaybeTaintSource.AbstractTainted(x) => AbstractMaybeTaintSource.AbstractTainted(x)
//          case AbstractMaybeTaintSource.AbstractUntainted => AbstractMaybeTaintSource.AbstractUntainted
//          case AbstractMaybeTaintSource.AbstractBottom => AbstractMaybeTaintSource.AbstractBottom
//      }
  }
  
  def foldValues[A](f: AbstractMaybeTaintSource => Set[A]): Set[A] = f(this)
  
  def join(that: AbstractMaybeTaintSource): AbstractMaybeTaintSource = checkTaint(that)
  
  def meet(that: AbstractMaybeTaintSource): AbstractMaybeTaintSource= ???
  
  def subsumes(that: AbstractMaybeTaintSource): Boolean = this.equals(that)
  def and(that: => AbstractMaybeTaintSource): AbstractMaybeTaintSource = checkTaint(that)
  def or(that: => AbstractMaybeTaintSource): AbstractMaybeTaintSource = checkTaint(that)
}

/** Taint Lattice */
object AbstractMaybeTaintSource {

  case class AbstractTainted(sources: Set[Position]) extends AbstractMaybeTaintSource {
    override def toString = "Tainted"
  }

  object AbstractUntainted extends AbstractMaybeTaintSource {
    override def toString = "Untainted"
  }  
  
  case class AbstractTop(sources: Set[Position]) extends AbstractMaybeTaintSource {
    override def toString = "top"
    override def isTrue = true
    override def isFalse = true
    override def subsumes(that: AbstractMaybeTaintSource) = true
    override def unaryOp(op: UnaryOperator) = ???
    override def binaryOp(op: BinaryOperator)(that: AbstractMaybeTaintSource) = op match {
      case Plus | Minus | Times | Div | Modulo | Lt | NumEq => AbstractTop(sources)
      case _ => super.binaryOp(op)(that)
    }
    override def and(that: => AbstractMaybeTaintSource) = ???
    override def or(that: => AbstractMaybeTaintSource) = ???
  }
  
  case class AbstractError(s:String) extends AbstractMaybeTaintSource {
    override def toString = s
    override def isError = true
  }
  
  
  object AbstractBottom extends AbstractMaybeTaintSource {
    override def toString = "bottom"
    override def isTrue = false
    override def isFalse = false
    override def join(that: AbstractMaybeTaintSource) = that
  }
  case class AbstractPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](prim: Primitive[Addr, Exp, Abs]) extends AbstractMaybeTaintSource {
    override def toString = s"#<prim ${prim.name}>"
  }
  /* We need to be able to represent multiple closures in this lattice */
  case class AbstractClosures[Exp : Expression, Addr : Address](clos: Set[(Exp, Environment[Addr])]) extends AbstractMaybeTaintSource {
    override def toString = "#<clos>"
    override def join(that: AbstractMaybeTaintSource) = that match {
      case other: AbstractClosures[Exp, Addr] => AbstractClosures(clos ++ other.clos)
      case x => throw new Error("Type lattice cannot join a closure with something else"+x)
    }
  }
  case class AbstractTid[TID : ThreadIdentifier](t: TID) extends AbstractMaybeTaintSource {
    override def toString = "#<thread $t>"
  }

  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractMaybeTaintSource

  case class AbstractObject[Addr : Address](base: Addr) extends AbstractMaybeTaintSource
  
  
  implicit object AbstractMaybeTaintSourceAbstractValue extends AbstractValue[AbstractMaybeTaintSource] {
    def name = "MaybeTaint"

    def isTrue(x: AbstractMaybeTaintSource) = x.isTrue
    def isFalse(x: AbstractMaybeTaintSource) = x.isFalse
    def isError(x: AbstractMaybeTaintSource) = x.isError
    def unaryOp(op: UnaryOperator)(x: AbstractMaybeTaintSource) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: AbstractMaybeTaintSource, y: AbstractMaybeTaintSource) = x.binaryOp(op)(y)
    def foldValues[B](x: AbstractMaybeTaintSource, f: AbstractMaybeTaintSource => Set[B]) = x.foldValues(f)
    def join(x: AbstractMaybeTaintSource, y: AbstractMaybeTaintSource) = x.join(y)
    def meet(x: AbstractMaybeTaintSource, y: AbstractMaybeTaintSource) = x.meet(y)
    def subsumes(x: AbstractMaybeTaintSource, y: AbstractMaybeTaintSource) = x.subsumes(y)
    def and(x: AbstractMaybeTaintSource, y: => AbstractMaybeTaintSource) = x.and(y)
    def or(x: AbstractMaybeTaintSource, y: => AbstractMaybeTaintSource) = x.or(y)
    def car[Addr : Address](x: AbstractMaybeTaintSource) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractMaybeTaintSource) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }
    def objects[Addr : Address](x: AbstractMaybeTaintSource) = x match {
      case AbstractObject(base: Addr) => Set(base)
      case _ => Set()
    }    
    def symbols(x: AbstractMaybeTaintSource):Set[String] = Set()
    
    private def toString[Addr : Address, Exp : Expression]
     (x: AbstractMaybeTaintSource, store: Store[Addr, Exp, AbstractMaybeTaintSource], inside: Boolean): String = x.toString
     
    def toString[Addr : Address, Exp : Expression](x: AbstractMaybeTaintSource, store: Store[Addr, Exp, AbstractMaybeTaintSource]) = toString(x, store, false)

    def getClosures[Exp : Expression, Addr : Address](x: AbstractMaybeTaintSource) = x match {
      case v: AbstractClosures[Exp, Addr] => v.clos
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](x: AbstractMaybeTaintSource) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, Exp, Abs]) => Some(prim)
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](x: AbstractMaybeTaintSource) = x match {
      case AbstractTid(t: TID) => Set(t)
      case _ => Set()
    }

    def sanitize(x: AbstractMaybeTaintSource) = AbstractUntainted
    def sink(x: AbstractMaybeTaintSource) = x match {      
      case AbstractTop(x) => AbstractError("MAYBE_TAINTED | source="+x.mkString(",")) //+x.foldLeft("")((xx, acc) => acc + "" + xx))
      case AbstractTainted(x) => AbstractError("IS_TAINTED | source="+x.mkString(","))
      case _ => x // AbstractUntainted, AbstractBottom, Object
    }
    def taint(x: AbstractMaybeTaintSource, sources:Set[Position]) = AbstractTainted(sources)
    def top = ???
    def bottom = AbstractBottom
    def error(x: AbstractMaybeTaintSource) = AbstractError("error taint detected")
    def inject(x: Int) = AbstractUntainted
    def inject(x: String) = AbstractUntainted
    def inject(x: Boolean) = AbstractUntainted
    def inject(x: Char) = AbstractUntainted
    def inject[Addr : Address, Exp : Expression, Abs : AbstractValue](x: Primitive[Addr, Exp, Abs]) = AbstractPrimitive(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosures[Exp, Addr](Set((x._1, x._2)))
    def injectTid[TID : ThreadIdentifier](t: TID) = AbstractTid(t)
    def injectSymbol(x: String) = AbstractUntainted
    def nil = AbstractUntainted
    def cons[Addr : Address](car: Addr, cdr : Addr) = AbstractCons(car, cdr)
    def obj[Addr : Address](base: Addr) = AbstractObject(base)
  }
}
