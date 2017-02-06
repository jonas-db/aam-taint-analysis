import UnaryOperator._
import BinaryOperator._
import scala.util.parsing.input.Position
/** Taint lattice */
trait AbstractTaint {
  def isTrue: Boolean = true
  def isFalse: Boolean = false
  def isError: Boolean = false
  
  def unaryOp(op: UnaryOperator): AbstractTaint = op match {
    case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractTaint.AbstractBottom
    case Not => this 
  }
  
  def binaryOp(op: BinaryOperator)(that: AbstractTaint): AbstractTaint = checkTaint(that)
  
  def checkTaint(that: AbstractTaint):AbstractTaint = {
    this match { 
      case AbstractTaint.AbstractTop => AbstractTaint.AbstractTop // if the other is tainted it becomes tainted
      case _ => that match { // this not tainted
          // that is tainted
          case AbstractTaint.AbstractTop => AbstractTaint.AbstractTop
          case _ => that
      }
    }
  }
  
  def foldValues[A](f: AbstractTaint => Set[A]): Set[A] = f(this)
  
  def join(that: AbstractTaint): AbstractTaint =
    if (this.equals(that) || that.equals(AbstractTaint.AbstractBottom)) { this } else { AbstractTaint.AbstractTop }
  
  def meet(that: AbstractTaint): AbstractTaint =
    if (this.equals(that) || that.equals(AbstractTaint.AbstractTop)) { this } else { AbstractTaint.AbstractBottom }
  
  def subsumes(that: AbstractTaint): Boolean = this.equals(that)
  def and(that: => AbstractTaint): AbstractTaint = checkTaint(that)
  def or(that: => AbstractTaint): AbstractTaint = checkTaint(that)
}

/** Taint Lattice */
object AbstractTaint {
  
  object AbstractTop extends AbstractTaint {
    override def toString = "top"
    override def isTrue = true
    override def isFalse = true
    override def subsumes(that: AbstractTaint) = true
    override def unaryOp(op: UnaryOperator) = op match {
      case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger | IsBoolean => AbstractTop
      case Not => AbstractTop
      case Ceiling | Log | Random => AbstractTop
      case _ => super.unaryOp(op)
    }
    override def binaryOp(op: BinaryOperator)(that: AbstractTaint) = op match {
      case Plus | Minus | Times | Div | Modulo | Lt | NumEq => AbstractTop
      case _ => super.binaryOp(op)(that)
    }
    override def and(that: => AbstractTaint) = AbstractTop
    override def or(that: => AbstractTaint) = AbstractTop
  }
  
  object AbstractError extends AbstractTaint {
    override def toString = "error taint detected"
    override def isError = true
  }
  
  
  object AbstractBottom extends AbstractTaint {
    override def toString = "bottom"
    override def isTrue = false
    override def isFalse = false
    override def join(that: AbstractTaint) = that
  }
  case class AbstractPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](prim: Primitive[Addr, Exp, Abs]) extends AbstractTaint {
    override def toString = s"#<prim ${prim.name}>"
  }
  /* We need to be able to represent multiple closures in this lattice */
  case class AbstractClosures[Exp : Expression, Addr : Address](clos: Set[(Exp, Environment[Addr])]) extends AbstractTaint {
    override def toString = "#<clos>"
    override def join(that: AbstractTaint) = that match {
      case other: AbstractClosures[Exp, Addr] => AbstractClosures(clos ++ other.clos)
      case x => throw new Error("Type lattice cannot join a closure with something else"+x)
    }
  }
  case class AbstractTid[TID : ThreadIdentifier](t: TID) extends AbstractTaint {
    override def toString = "#<thread $t>"
  }

  case class AbstractCons[Addr : Address](car: Addr, cdr: Addr) extends AbstractTaint

  case class AbstractObject[Addr : Address](base: Addr) extends AbstractTaint
  
  
  implicit object AbstractTaintAbstractValue extends AbstractValue[AbstractTaint] {
    def name = "Taint"

    def isTrue(x: AbstractTaint) = x.isTrue
    def isFalse(x: AbstractTaint) = x.isFalse
    def isError(x: AbstractTaint) = x.isError
    def unaryOp(op: UnaryOperator)(x: AbstractTaint) = x.unaryOp(op)
    def binaryOp(op: BinaryOperator)(x: AbstractTaint, y: AbstractTaint) = x.binaryOp(op)(y)
    def foldValues[B](x: AbstractTaint, f: AbstractTaint => Set[B]) = x.foldValues(f)
    def join(x: AbstractTaint, y: AbstractTaint) = x.join(y)
    def meet(x: AbstractTaint, y: AbstractTaint) = x.meet(y)
    def subsumes(x: AbstractTaint, y: AbstractTaint) = x.subsumes(y)
    def and(x: AbstractTaint, y: => AbstractTaint) = x.and(y)
    def or(x: AbstractTaint, y: => AbstractTaint) = x.or(y)
    def car[Addr : Address](x: AbstractTaint) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(car)
      case _ => Set()
    }
    def cdr[Addr : Address](x: AbstractTaint) = x match {
      case AbstractCons(car: Addr, cdr: Addr) => Set(cdr)
      case _ => Set()
    }
    def objects[Addr : Address](x: AbstractTaint) = x match {
      case AbstractObject(base: Addr) => Set(base)
      case _ => Set()
    }    
    def symbols(x: AbstractTaint):Set[String] = Set()
 
  case class AbstractSymbol(v: String) extends AbstractTaint {
    override def toString = v.toString

  }    
    
    private def toString[Addr : Address, Exp : Expression]
     (x: AbstractTaint, store: Store[Addr, Exp, AbstractTaint], inside: Boolean): String = x.toString
     
    def toString[Addr : Address, Exp : Expression](x: AbstractTaint, store: Store[Addr, Exp, AbstractTaint]) = toString(x, store, false)

    def getClosures[Exp : Expression, Addr : Address](x: AbstractTaint) = x match {
      case v: AbstractClosures[Exp, Addr] => v.clos
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](x: AbstractTaint) = x match {
      case AbstractPrimitive(prim: Primitive[Addr, Exp, Abs]) => Some(prim)
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](x: AbstractTaint) = x match {
      case AbstractTid(t: TID) => Set(t)
      case _ => Set()
    }

    def sanitize(x: AbstractTaint) = bottom
    def sink(x: AbstractTaint) = {
      if(x == AbstractTop) AbstractError else x
    }
    def taint(x: AbstractTaint, s:Set[Position]) = AbstractTop
    def top = AbstractTop
    def bottom = AbstractBottom
    def error(x: AbstractTaint) = AbstractError
    def inject(x: Int) = AbstractBottom
    def inject(x: String) = AbstractBottom
    def inject(x: Boolean) = AbstractBottom
    def inject(x: Char) = AbstractBottom
    def inject[Addr : Address, Exp : Expression, Abs : AbstractValue](x: Primitive[Addr, Exp, Abs]) = AbstractPrimitive(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = AbstractClosures[Exp, Addr](Set((x._1, x._2)))
    def injectTid[TID : ThreadIdentifier](t: TID) = AbstractTid(t)
    def injectSymbol(x: String) = AbstractBottom
    def nil = AbstractBottom
    def cons[Addr : Address](car: Addr, cdr : Addr) = AbstractCons(car, cdr)
    def obj[Addr : Address](base: Addr) = AbstractObject(base)
  }
}
