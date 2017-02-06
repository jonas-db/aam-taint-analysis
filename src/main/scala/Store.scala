import scalaz.Scalaz._

case class Store[Addr : Address, Exp : Expression, Abs : AbstractValue]
  (content: Map[Addr, (Int, Abs)], 
      objectStore: ObjectStore[Addr, Exp, Abs],
      listenerStore: ListenerStore[Addr, Exp, Abs],
      eventQueue: EventQueue[Addr, Abs],
      counting: Boolean) {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]
  
  override def toString = content.filterKeys(a => !addr.isPrimitive(a)).toString
  def keys: collection.Iterable[Addr] = content.keys
  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((Addr, Abs)) => Boolean) = content.forall({
    case (a, (_, v)) => p(a, v)
  })
  def lookup(a: Addr): Abs = content.get(a) match {
    case None => throw new Exception(s"Unbound address (should not happen): $a")
    case Some(v) => v._2
  }
  /** Looks up a value in the store (returning bottom if value not present) */
  def lookupBot(a: Addr): Abs = content.getOrElse(a, (0, abs.bottom))._2
  
  def lookupObjectBot(a: Addr): JSObject[Addr, Exp, Abs] = 
      objectStore.content.getOrElse(a, (0, new JSObject[Addr, Exp, Abs]()))._2

//  def lookupListenerBot(a: Addr): Map[String, (Int, Abs)] = 
//    listenerStore.content.getOrElse(a, Map[String, (Int, Abs)]())     
      
  /** Adds a new element to the store */
  def extend(a: Addr, v: Abs): Store[Addr, Exp, Abs] = content.get(a) match {
    case None => Store(content + (a -> (0, v)), objectStore, listenerStore, eventQueue, counting)
    case Some((n, v2)) => Store(content + (a -> (if (counting) { n+1 } else { n }, abs.join(v2, v))), objectStore, listenerStore, eventQueue, counting)
  }
  /** Updates an element in the store. Might perform a strong update if this store supports strong updates */
  def update(a: Addr, v: Abs): Store[Addr, Exp, Abs] =
    if (counting) {
      content.get(a) match {
        case None => throw new RuntimeException("Updating default-store at an adress not used")
        case Some((0, _)) => Store(content + (a -> (0, v)), objectStore, listenerStore, eventQueue, counting)
        case _ => extend(a, v)
      }
    } else {
      extend(a, v)
    }
  /** Joins two stores */
  //TODO: not working for FREE, not joining obj/listenstore
  def join(that: Store[Addr, Exp, Abs]): Store[Addr, Exp, Abs] = Store(this.content |+| that.content, objectStore, listenerStore, eventQueue, counting)
  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[Addr, Exp, Abs]): Boolean = {
    val store = that.forall((binding: (Addr, Abs)) 
        => abs.subsumes(lookupBot(binding._1), binding._2))
        
    val objectStore = that.objectStore.forall((binding: (Addr, JSObject[Addr, Exp, Abs])) 
        => lookupObjectBot(binding._1).equals(binding._2))
     
    //val objectStore = that.objectStore.equals(this.objectStore);     
    val listenerStore = that.listenerStore.equals(this.listenerStore);    
//    val listenerStore = that.listenerStore.forall((binding: (Addr, Map[String, (Int, Abs)])) => 
//      lookupListenerBot(binding._1).forall({
//        case (event, (_,v1)) => {
//          binding._2.get(event) match {
//            case None => false
//            case Some((_, v2)) => abs.subsumes(v1, v2)
//          }
//        }
//      })     
//    )
   
    store && objectStore && listenerStore && that.eventQueue.equals(this.eventQueue);
  }
  
  /** Returns a store containing items that are not equal with the other store */
  def diff(that: Store[Addr, Exp, Abs]): Store[Addr, Exp, Abs] = {
    Store(content.filter({ case (a, (n, v)) => that.content.get(a) match {
      case Some((n2, v2)) => n != n2 && v != v2
      case None => true
    }}), objectStore, listenerStore, eventQueue, counting)
  }
  
  // Objects

  def extendListener(a : Addr) = {
    Store(content, objectStore, listenerStore.extend(a), eventQueue, counting)
  }
  def extendObject(a : Addr, v : JSObject[Addr, Exp, Abs]) = {
    Store(content, objectStore.extend(a, v), listenerStore, eventQueue, counting)
  }
  def updateObject(a : Addr, v : JSObject[Addr, Exp, Abs]) = Store(content, objectStore.update(a, v), listenerStore, eventQueue, counting)
  def defineDataProperty(a : Addr, p:String, v:JSProperty[Addr,Exp,Abs]) = Store(content, objectStore.defineDataProperty(a,p,v), listenerStore, eventQueue, counting)
  def setPrototype(a : Addr, proto:Set[Addr]) = Store(content, objectStore.setPrototype(a, proto), listenerStore, eventQueue, counting)
  def setProperty(a : Addr, p:String,v:JSProperty[Addr,Exp,Abs]) = Store(content, objectStore.setProperty(a, p,v), listenerStore, eventQueue, counting)
  def deleteProperty(a : Addr, p:String) = Store(content, objectStore.deleteProperty(a, p), listenerStore, eventQueue, counting)
  def objectLookup(a : Addr) : JSObject[Addr, Exp, Abs] = objectStore.lookup(a);
  
  /*
   * Events
   */
  
  //def listenerLookup(a: Addr, name:String) = listenerStore.lookup(a, name);
  
  def getEvenListenerStore() = this.listenerStore.content
  def addEventListener(obj: Addr, name: String, handler: Abs) = Store(content, objectStore, listenerStore.update(obj, name, handler), eventQueue, counting)
  def removeEventListener(obj: Addr, name: String, handler: Abs) = Store(content, objectStore, listenerStore.remove(obj, name, handler), eventQueue, counting)
  def getEventListeners(o: Addr, ev: String):List[Abs] = listenerStore.getListenersForObject(o, ev)
  //def getAllEventListeners() : List[Abs] = listenerStore.getListeners()

  def scheduleListeners(obj: Addr, event: String, listeners:List[(Abs, Abs)], abs:Boolean) = Store(content, objectStore, listenerStore, eventQueue.scheduleListeners(obj, event, listeners, abs), counting)
  def removeFromQueue(obj: Addr, event: String) = Store(content, objectStore, listenerStore, eventQueue.removeListeners(obj, event), counting)

}

object Store {
  /* TODO: have abstract counting as a parameter of the analysis. Also, when it is
   * turned on, it prevents AAC and Free from converging. For now, it's only
   * enabled with the AbstractConcrete lattice. */
  val counting = true; //implicitly[AbstractValue[Abs]].name == "Concrete"
  
  def empty[Addr : Address, Exp : Expression, Abs : AbstractValue] =
    Store(Map[Addr, (Int, Abs)](), ObjectStore.empty[Addr, Exp, Abs], ListenerStore.empty[Addr, Exp, Abs], EventQueue.empty[Addr, Abs], counting)
  def initial[Addr : Address, Exp : Expression, Abs : AbstractValue](values: List[(Addr, Abs)]): Store[Addr, Exp, Abs] =
    Store(values.map({ case (a, v) => (a, (0, v)) }).toMap, ObjectStore.empty[Addr, Exp, Abs], ListenerStore.empty[Addr, Exp, Abs], EventQueue.empty[Addr, Abs], counting)
}
