import UnaryOperator._
import BinaryOperator._
import scala.util.parsing.input.Position

/**
 * Product lattice, combines two lattices X and Y as a product (X, Y).
 * Here's an example on how to use it, to combine the type lattice with the typeset lattice:
 *   val prod = new ProductLattice[AbstractType, AbstractTypeSet] // create the product lattice
 *   import prod._ // import its elements (and most importantly, Product)
 *   run(new Free[SchemeExp, Product, ClassicalAddress], new SchemeSemantics[Product, ClassicalAddress]) _ // run a machine with it
 */
class ProductLattice[X : AbstractValue, Y : AbstractValue] {
  val xabs = implicitly[AbstractValue[X]]
  val yabs = implicitly[AbstractValue[Y]]

  trait Product
  case class Prim[Addr : Address, Exp : Expression, Abs : AbstractValue](prim: Primitive[Addr, Exp, Abs]) extends Product {
    override def toString = s"#<prim ${prim.name}>"
  }
  case class Prod(x: X, y: Y) extends Product

  implicit object ProductAbstractValue extends AbstractValue[Product] {
    def name = s"(${xabs.name}, ${yabs.name})"

    private def err(reason: String) = error(inject(reason))

    def isTrue(p: Product) = p match {
      case Prod(x, y) => xabs.isTrue(x) || yabs.isTrue(y)
      case Prim(_) => true
    }
    def isFalse(p: Product) = p match {
      case Prod(x, y) => xabs.isFalse(x) || yabs.isFalse(y)
      case Prim(_) => false
    }
    def isError(p: Product) = p match {
      case Prod(x, y) => xabs.isError(x) || yabs.isError(y)
      case Prim(_) => false
    }
    def unaryOp(op: UnaryOperator)(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.unaryOp(op)(x), yabs.unaryOp(op)(y))
      case Prim(_) => op match {
        case IsNull | IsCons | IsChar | IsSymbol | IsString | IsInteger => inject(false)
        case _ => err(s"operator $op cannot work on primitive (argument was $p)")
      }
    }
    def binaryOp(op: BinaryOperator)(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.binaryOp(op)(x1, x2), yabs.binaryOp(op)(y1, y2))
      case _ => err("operator $op cannot work on primitives (arguments were $p1 and $p2)")
    }
    def foldValues[B](p: Product, f: Product => Set[B]) = ???
    def join(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.join(x1, x2), yabs.join(y1, y2))
      case _ => ???
    }
    def meet(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.meet(x1, x2), yabs.meet(y1, y2))
      case _ => ???
    }
    def subsumes(p1: Product, p2: Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => xabs.subsumes(x1, x2) && yabs.subsumes(y1, y2)
      case (Prim(prim1), Prim(prim2)) => prim1 == prim2
      case _ => false
    }
    def and(p1: Product, p2: => Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.and(x1, x2), yabs.and(y1, y2))
      case _ => err(s"and cannot work on primitives (arguments were $p1 and $p2)")
    }
    def or(p1: Product, p2: => Product) = (p1, p2) match {
      case (Prod(x1, y1), Prod(x2, y2)) => Prod(xabs.or(x1, x2), yabs.or(y1, y2))
      case _ => err(s"or cannot work on primitives (arguments were $p1 and $p2)")
    }
    def car[Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.car[Addr](x) ++ yabs.car[Addr](y)
      case _ => Set[Addr]()
    }
    def cdr[Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.cdr[Addr](x) ++ yabs.cdr[Addr](y)
      case _ => Set[Addr]()
    }
    def objects[Addr : Address](p: Product) = p match {
      case Prod(x, y) => {
//        println(xabs.objects[Addr](x))
//        println(yabs.objects[Addr](y))
//        println(xabs.objects[Addr](x) ++ yabs.objects[Addr](y))
        xabs.objects[Addr](x) ++ yabs.objects[Addr](y)
      }
      case _ => Set[Addr]()
    }
    def symbols(p: Product) = p match {
      case Prod(x, y) => xabs.symbols(x) ++ yabs.symbols(y)
      case _ => Set()
    }    
    def toString[Addr : Address, Exp : Expression](p: Product, store: Store[Addr, Exp ,Product]) = p.toString // s"(${xabs.toString(p.x, store)}, ${yabs.toString(p.y, store)})"
    def getClosures[Exp : Expression, Addr : Address](p: Product) = p match {
      case Prod(x, y) => xabs.getClosures[Exp, Addr](x) ++ yabs.getClosures[Exp, Addr](y)
      case _ => Set()
    }
    def getPrimitive[Addr : Address, Exp : Expression, Abs : AbstractValue](p: Product) = p match {
      case Prim(prim: Primitive[Addr, Exp, Abs]) => Some(prim)
      case _ => None
    }
    def getTids[TID : ThreadIdentifier](p: Product) = p match {
      case Prod(x, y) => xabs.getTids[TID](x) ++ yabs.getTids[TID](y)
      case _ => Set()
    }


    def top = Prod(xabs.top, yabs.top)
    def bottom = Prod(xabs.bottom, yabs.bottom)
    def sanitize(x: Product) = x match {
      case Prod(a, t) => Prod(xabs.sanitize(a), yabs.sanitize(t))
    } 
    def sink(x: Product) = x match {
      case Prod(a, t) => Prod(xabs.sink(a), yabs.sink(t))
    } 
    def taint(x: Product, sources:Set[Position]) = x match {
      case Prod(a, t) => Prod(xabs.taint(a, sources), yabs.taint(t, sources))
    }    
    def error(p: Product) = p match {
      case Prod(x, y) => Prod(xabs.error(x), yabs.error(y))
      case Prim(_) => error(inject(p.toString))
    }
    def inject(x: Int) = Prod(xabs.inject(x), yabs.inject(x))
    def inject(x: String) = { System.out.println(x); Prod(xabs.inject(x), yabs.inject(x))}
    def inject(x: Char) = Prod(xabs.inject(x), yabs.inject(x))
    def inject(x: Boolean) = Prod(xabs.inject(x), yabs.inject(x))
    def inject[Addr : Address, Exp : Expression, Abs : AbstractValue](x: Primitive[Addr, Exp, Abs]) = Prim(x)
    def inject[Exp : Expression, Addr : Address](x: (Exp, Environment[Addr])) = Prod(xabs.inject[Exp, Addr](x), yabs.inject[Exp, Addr](x))
    def injectTid[TID : ThreadIdentifier](t: TID) = Prod(xabs.injectTid[TID](t), yabs.injectTid[TID](t))
    def injectSymbol(x: String) = Prod(xabs.injectSymbol(x), yabs.injectSymbol(x))
    def nil = Prod(xabs.nil, yabs.nil)
    def cons[Addr : Address](car: Addr, cdr: Addr) = Prod(xabs.cons[Addr](car, cdr), yabs.cons[Addr](car, cdr))
    def obj[Addr : Address](base: Addr) = Prod(xabs.obj[Addr](base), yabs.obj[Addr](base))
  }
}
