import scalaz.Scalaz._
import scala.collection.mutable.ListBuffer

//permutate if not joined
//all combinations in case of joined
case class ListenerStore[Addr : Address, Exp : Expression, Abs : AbstractValue]
  (content: Map[Addr, (Int, Map[String, List[Abs]])], counting: Boolean) {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]

  def getListenersForObject(a: Addr, event:String) : List[Abs] = content.get(a) match {
    case None => {
      List()
    }
    case Some((count, map)) => map.get(event) match {
      case None => List()
      case Some(l) => l
    }
  }
  
  def isJoined(a:Addr):Boolean =content.get(a) match {
    case None => false
    case Some((count, map)) => if(counting) count != 0 else true
    
  }
  
  def mergeSets(s1:List[Abs], s2:List[Abs]):List[Abs] = {
    (s1.toSet[Abs] ++ s2.toSet[Abs]).toList
  }

  def joinListeners(l1:Map[String, List[Abs]], l2:Map[String, List[Abs]])
      : Map[String, List[Abs]]= {
    
    // TODO:we do not take subsumption of closures into account
    // this is sound, but we might traverse extra unnecessary states
    //require(1 == 2, "noooo")
    
    val keys = l1.keys.toSet ++ l2.keys.toSet
    keys.foldLeft(Map[String, List[Abs]]())({ case (map, k) => {
      val v1 = l1.getOrElse(k, List[Abs]())
      val v2 = l2.getOrElse(k, List[Abs]())
      
      map + (k -> mergeSets(v1, v2))
    }})    
 
  }
  
  def extend(a: Addr): ListenerStore[Addr, Exp, Abs] = content.get(a) match {
    case None => ListenerStore(content + (a -> (0, Map[String, List[Abs]]())), counting)
    case Some((n, listeners)) => {
      ListenerStore(content + (a -> (if (counting) { Math.min(n+1, 1) } else { n }, joinListeners(listeners, Map[String, List[Abs]]()))), counting)
    }
  }

  def checkList(list:List[Abs],  handler:Abs) = {
    //TODO: subsumption?.
    val exists = list.exists { x => x.equals(handler) }
    
    // only add the handler if it isn't there!
    if(exists)
    {
      list
    }
    else
    {
      list :+ handler      
    }
  }
  
  def forEachSet(set:Set[List[Abs]], handler:Abs) = {
    // we joined the listeners of two objects
    // for each current list in the set, we add the handler, because we don't know 
    // to which list we have to add it, as we don't know which list is from which object
    // given Set(List())
    // this results in Set(List(), List(handler)), 
    // which is sound because we don't know which object we add the handler to

    val newSet = set.map { list => checkList(list, handler) }
    
    // now we add the newset to the old set
    // because we need to have every combination
    set ++ newSet
  }
    
  def addToFirst(set:Set[List[Abs]], handler:Abs) = {
    require(set.size == 1, "cannot happen")
    
    // we add the handler to the end of the list, this list is the only list
    set.map { list => checkList(list, handler) }
  }

  def removeFromFirst(set:List[Abs], handler:Abs) = {
   
    // we remove the handler from the list
    //TODO: no subsumption yet
    println(set.size)
    val r = set.filter { e => !e.equals(handler) }
    println(r.size)
    r
  }
  
  /** Adds a new handler to the store */
  def update(a: Addr, event:String, handler: Abs): ListenerStore[Addr, Exp, Abs] = content.get(a) match {
    // we should not throw an exception, since this can only happen when we update an object
    // such as an event object, i.e. the ones which do not support events
    case None => this// throw new RuntimeException("Updating listenerstore at an adress not used")
    // existing object with some events
    // count indicates joined or not.
    case Some((count, map)) => map.get(event) match {        
        // already has an event handler for this event
      //note: we don't distinguish between counting or not because we have check for duplicates anyways
      // so simply adding/joining is the same operation
        case Some(set) => ListenerStore(content + (a -> (count, (map + (event -> checkList(set, handler))))), counting)
        // no handler for this event, we add it
        case None => ListenerStore(content + (a -> (count, (map + (event -> List(handler))))), counting)
      }
  }

  /** Remove a new handler to the store */
  def remove(a: Addr, event:String, handler: Abs): ListenerStore[Addr, Exp, Abs] = content.get(a) match {
    // removing from an object with no handlers == no-op
    case None => this
    // existing object with some events
    // count indicates joined or not.
    case Some((count, map)) => map.get(event) match {        
        // already has an event handler for this event
       // counting or concrete, just to make sure if counting is disabled using concrete lattice, it will work
        case Some(set) => (counting || implicitly[AbstractValue[Abs]].name == "Concrete") match {
          // counting = 
          case true => {
            count match {
              // we joined, removing has no effect
              case 1 => this
              // we didn't join yet, so we remove it from the first (and only) list
              // this case means, that we have an object which already have an handler for this event
              case 0 => {
                ListenerStore(content + (a -> (count, (map + (event -> removeFromFirst(set, handler))))), counting)
              }
            }
          }
          // we are not counting, so we don't know whether we have joined or not
          // removing has no effect
          case false => {
            this
          }
        }
        // no handler for this event, so we can't remove anything
        case None => this
      }
  }  
  
  /** Joins two stores */
  def join(that: ListenerStore[Addr, Exp, Abs]): ListenerStore[Addr, Exp, Abs] = ???
  
  /** Checks whether this store subsumes another store */
  def subsumes(that: ListenerStore[Addr, Exp, Abs]): Boolean = ???
  
  def diff(that: ListenerStore[Addr, Exp, Abs]): ListenerStore[Addr, Exp, Abs] = ???

}


object ListenerStore {
  val counting = true
  
  def empty[Addr : Address, Exp : Expression, Abs : AbstractValue] =
    ListenerStore(Map[Addr, (Int, Map[String, List[Abs]])](), counting)
    
  def initial[Addr : Address, Exp : Expression, Abs : AbstractValue](values: List[(Addr, Abs)]): ListenerStore[Addr, Exp, Abs] =
    ListenerStore(Map[Addr, (Int, Map[String, List[Abs]])](), counting)
}
