import scalaz.Scalaz._

case class ObjectStore[Addr : Address, Exp : Expression, Abs : AbstractValue]
  (content: Map[Addr, (Int, JSObject[Addr, Exp, Abs])], counting: Boolean) {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]
  
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  
  def keys: collection.Iterable[Addr] = content.keys
  
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, JSObject[Addr, Exp, Abs])) => Boolean) = content.forall({
    case (a, (_, v)) => p(a, v)
  })
  
  def lookup(a: Addr): JSObject[Addr, Exp, Abs] = content.get(a) match {
    case None => throw new Exception(s"Unbound address (should not happen): $a")
    case Some(v) => v._2
  }
  
  /** Looks up a value in the store (returning bottom if value not present) */
  //def lookupBot(a: Addr): Abs = content.getOrElse(a, (0, abs.bottom))._2
  
  /** Adds a new element to the store */
  def extend(a: Addr, v: JSObject[Addr, Exp, Abs]): ObjectStore[Addr, Exp, Abs] = { 
    //println("extending object store")
    content.get(a) match { 
      case None => ObjectStore(content + (a -> (0, v)), counting)
      case Some((n, v2)) => {
        //println("joining objects")
        val nw = ObjectStore(content + (a -> (if (counting) { Math.min(n+1, 1) } else { n }, v2.join(v))), counting)
        nw
      }
    } 
  }

  def defineDataProperty(a: Addr, property: String, prop:JSProperty[Addr,Exp,Abs]): ObjectStore[Addr, Exp, Abs] = {
    content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((_, JSObject(prototype, properties))) => 
          update(a, JSObject(prototype, properties + (property -> Set(prop))))
    }
  }  

  def setProperty(a: Addr, property: String, prop:JSProperty[Addr,Exp,Abs]): ObjectStore[Addr, Exp, Abs] = {
    content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((_, JSObject(prototype, properties))) => properties.get(property) match {
          //property is not defined while setting, so we define it
          case None => update(a, JSObject(prototype, properties + (property -> Set(prop))))
          // if it is defined
          case Some(p) => p.size match {
            // if it is a single data property we can replace it
            case 1 => p.head match {
              case  JSDataProperty(v) => update(a, JSObject(prototype, properties + (property -> Set(prop))))
              // it is a accessor property, we cannot do anything, a setter will be called instead
              case _ => this
            } 
            // multiple, we add it to the current set ONLY if it contains at least one data property
            // because if it are all setters, we shouldn't add it!
            // this happens when we have joined props (Data, Acc), this creates 2 branches
            // each one generates a different store (1) with added data prop, (2) unchanged, but acc may have side effects
            // in this way we have both possible actions
            case _ => p.exists(_.isInstanceOf[JSDataProperty[Addr, Exp,Abs]]) match {
              case false => this
              case true => update(a, JSObject(prototype, properties + (property -> (p ++ Set(prop)))))
            }
          }
        }          
    }
  }  
  
  def deleteProperty(a: Addr, p:String): ObjectStore[Addr, Exp, Abs] = {
    content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((_, JSObject(proto, properties))) => 
          update(a, JSObject(proto, properties - p))
    }
  }
  
  def setPrototype(a: Addr, protos:Set[Addr]): ObjectStore[Addr, Exp, Abs] = {
    content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((_, JSObject(_, properties))) => 
          update(a, JSObject(protos, properties))
    }
  }   
  
  /** Updates an element in the store. Might perform a strong update if this store supports strong updates */
  def update(a: Addr, v: JSObject[Addr, Exp, Abs]): ObjectStore[Addr, Exp, Abs] = {
    println("updating")
    println(v)
    if (counting) {
      content.get(a) match {
        case None => throw new RuntimeException("Updating store at an adress not used")
        case Some((0, _)) => ObjectStore(content + (a -> (0, v)), counting)
        case _ => extend(a, v)
      }
    } else {
      extend(a, v)
    }}
  
  /** Joins two stores */
  def join(that: Store[Addr, Exp, Abs]): Store[Addr, Exp, Abs] = ???
  
  /** Checks whether this store subsumes another store */
  def subsumes(that: ObjectStore[Addr, Exp, Abs]): Boolean =
    that.forall((binding: (Addr, JSObject[Addr, Exp, Abs])) => content.get(binding._1) match {
      case None => false
      case Some(v) => v == binding._2
  })
  
  def diff(that: ObjectStore[Addr, Exp, Abs]): ObjectStore[Addr, Exp, Abs] = ???
    
  /** Returns a store containing items that are not equal with the other store */
//  def diff(that: Store[Addr, Abs]): Store[Addr, Abs] = {
//    Store(content.filter({ case (a, (n, v)) => that.content.get(a) match {
//      case Some((n2, v2)) => n != n2 && v != v2
//      case None => true
//    }}), counting)
//  }
  
}


object ObjectStore {
  /* TODO: have abstract counting as a parameter of the analysis. Also, when it is
   * turned on, it prevents AAC and Free from converging. For now, it's only
   * enabled with the AbstractConcrete lattice. */
  
  //TODO: counting true+fact.scm quote = infinite loop
  val counting = true //implicitly[AbstractValue[Abs]].name == "Concrete";
  
  def empty[Addr : Address, Exp : Expression, Abs : AbstractValue] =
    ObjectStore(Map[Addr, (Int, JSObject[Addr, Exp, Abs])](), counting)
    
  def initial[Addr : Address, Exp : Expression, Abs : AbstractValue](values: List[(Addr, JSObject[Addr, Exp, Abs])]): ObjectStore[Addr, Exp, Abs] =
    ObjectStore(values.map({ case (a, v) => (a, (0, v)) }).toMap, counting)
}
