import UnaryOperator._
import BinaryOperator._
import scala.util.parsing.input.Position
/** Taint lattice */
trait AbstractMaybeTaint {
  def isTrue: Boolean = true
  def isFalse: Boolean = false 
  def isError: Boolean = false
  
  def unaryOp(op: UnaryOperator): AbstractMaybeTaint = op match {
    case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean 
      => AbstractMaybeTaint.AbstractUntainted
    case Not => this 
  }
  
  def binaryOp(op: BinaryOperator)(that: AbstractMaybeTaint): AbstractMaybeTaint = checkTaint(that)
  
  def checkTaint(that: AbstractMaybeTaint):AbstractMaybeTaint = this match { 
      // if this is tainted it becomes tainted
      case AbstractMaybeTaint.AbstractTop => AbstractMaybeTaint.AbstractTop 
      case AbstractMaybeTaint.AbstractTainted => AbstractMaybeTaint.AbstractTainted
      
      // this not tainted, bottom or untainted
      case _ => that match { 
          // that is tainted
          case AbstractMaybeTaint.AbstractTop => AbstractMaybeTaint.AbstractTop
          case AbstractMaybeTaint.AbstractTainted => AbstractMaybeTaint.AbstractTainted
          case AbstractMaybeTaint.AbstractUntainted => AbstractMaybeTaint.AbstractUntainted
          case AbstractMaybeTaint.AbstractBottom => AbstractMaybeTaint.AbstractBottom
      }
  }
  
  def foldValues[A](f: AbstractMaybeTaint => Set[A]): Set[A] = f(this)
  
  def join(that: AbstractMaybeTaint): AbstractMaybeTaint =
    if (this.equals(that) || that.equals(AbstractMaybeTaint.AbstractBottom)) { this } 
    else { AbstractMaybeTaint.AbstractTop }
  
  def meet(that: AbstractMaybeTaint): AbstractMaybeTaint =
    if (this.equals(that) || that.equals(AbstractMaybeTaint.AbstractTop)) { this } 
    else { AbstractMaybeTaint.AbstractBottom }
  
  def subsumes(that: AbstractMaybeTaint): Boolean = this.equals(that)
  def and(that: => AbstractMaybeTaint): AbstractMaybeTaint = checkTaint(that)
  def or(that: => AbstractMaybeTaint): AbstractMaybeTaint = checkTaint(that)
}

/** Taint Lattice */
object AbstractMaybeTaint {

  object AbstractTainted extends AbstractMaybeTaint {
    override def toString = "Tainted"
  }

  object AbstractUntainted extends AbstractMaybeTaint {
    override def toString = "Untainted"
  }  
  
  object AbstractTop extends AbstractMaybeTaint {
    override def toString = "top"
    override def isTrue = true
    override def isFalse = true
    override def subsumes(that: AbstractMaybeTaint) = true
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractTop
      case Not => AbstractTop
      case Ceiling | Log | Random => AbstractTop
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractMaybeTaint) = op match {
      case Plus | Minus | Times | Div | Modulo | Lt | NumEq => AbstractTop
      case _ => super.binaryOp(op)(that)
    }
    override def and(that: => AbstractMaybeTaint) = AbstractTop
    override def or(that: => AbstractMaybeTaint) = AbstractTop
  }
  
  case class AbstractError(s:String) extends AbstractMaybeTaint {
    override def toString = s
    override def isError = true
  }
  
  
  object AbstractBottom extends AbstractMaybeTaint {
    override def toString = "bottom"
    override def isTrue = false
    override def isFalse = false
    override def join(that: AbstractMaybeTaint) = that
  }
  case class AbstractPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](prim: Primitive[Addr, Exp, Abs]) extends AbstractMaybeTaint {
    override def toString = s"#<prim ${prim.name}>"
  }
  /* We need to be able to represent multiple closures in this lattice */
  case class AbstractClosures[Exp : Expression, Addr : Address](clos: Set[(Exp, Environment[Addr])]) extends AbstractMaybeTaint {
    override def toString = "#<clos>"
    override def join(that: AbstractMaybeTaint) = that match {
      case other: AbstractClosures[Exp, Addr] => AbstractClosures(clos ++ other.clos)
      case x => throw new Error("Type lattice cannot join a closure with something else"+x)
    }
  }
  case class AbstractTid[TID : ThreadIdentifier](t: TID) extends AbstractMaybeTaint {
    override def toString = "#<thread $t>"
  }

  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractMaybeTaint

  case class AbstractObject[Addr : Address](base: Addr) extends AbstractMaybeTaint
  
  
  implicit object AbstractMaybeTaintAbstractValue extends AbstractValue[AbstractMaybeTaint] {
    def name = "MaybeTaint"

    def isTrue(x: AbstractMaybeTaint) = x.isTrue
    def isFalse(x: AbstractMaybeTaint) = x.isFalse
    def isError(x: AbstractMaybeTaint) = x.isError
    def unaryOp(op: UnaryOperator)(x: AbstractMaybeTaint) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: AbstractMaybeTaint, y: AbstractMaybeTaint) = x.binaryOp(op)(y)
    def foldValues[B](x: AbstractMaybeTaint, f: AbstractMaybeTaint => Set[B]) = x.foldValues(f)
    def join(x: AbstractMaybeTaint, y: AbstractMaybeTaint) = x.join(y)
    def meet(x: AbstractMaybeTaint, y: AbstractMaybeTaint) = x.meet(y)
    def subsumes(x: AbstractMaybeTaint, y: AbstractMaybeTaint) = x.subsumes(y)
    def and(x: AbstractMaybeTaint, y: => AbstractMaybeTaint) = x.and(y)
    def or(x: AbstractMaybeTaint, y: => AbstractMaybeTaint) = x.or(y)
    def car[Addr : Address](x: AbstractMaybeTaint) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractMaybeTaint) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }
    def objects[Addr : Address](x: AbstractMaybeTaint) = x match {
      case AbstractObject(base: Addr) => Set(base)
      case _ => Set()
    }    
    def symbols(x: AbstractMaybeTaint):Set[String] = Set()
    
    private def toString[Addr : Address, Exp : Expression]
     (x: AbstractMaybeTaint, store: Store[Addr, Exp, AbstractMaybeTaint], inside: Boolean): String = x.toString
     
    def toString[Addr : Address, Exp : Expression](x: AbstractMaybeTaint, store: Store[Addr, Exp, AbstractMaybeTaint]) = toString(x, store, false)

    def getClosures[Exp : Expression, Addr : Address](x: AbstractMaybeTaint) = x match {
      case v: AbstractClosures[Exp, Addr] => v.clos
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](x: AbstractMaybeTaint) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, Exp, Abs]) => Some(prim)
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](x: AbstractMaybeTaint) = x match {
      case AbstractTid(t: TID) => Set(t)
      case _ => Set()
    }

    def sanitize(x: AbstractMaybeTaint) = AbstractUntainted
    def sink(x: AbstractMaybeTaint) = x match {      
      case AbstractTop => AbstractError("MAYBE_TAINTED")
      case AbstractTainted => AbstractError("IS_TAINTED")
      case _ => x // AbstractUntainted, AbstractBottom, Object
    }
    def taint(x: AbstractMaybeTaint, s:Set[Position]) = AbstractTainted
    def top = AbstractTop
    def bottom = AbstractBottom
    def error(x: AbstractMaybeTaint) = AbstractError("error taint detected")
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
