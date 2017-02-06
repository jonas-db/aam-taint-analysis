import scalaz.Scalaz._
import scala.collection.mutable.ListBuffer

case class EventQueue[Addr : Address, Abs : AbstractValue]
  (content: Map[Addr, Map[String, List[(Abs, Abs)]]]) {
  val abs = implicitly[AbstractValue[Abs]]
  val addr = implicitly[Address[Addr]]
  
  def scheduleListeners(a: Addr, event:String, listeners:List[(Abs, Abs)], abs:Boolean) : EventQueue[Addr, Abs] = content.get(a) match {
    case None => EventQueue(content + (a -> Map(event -> listeners)))
    case Some(map) => map.get(event) match {
      case None => EventQueue(content + (a -> (map + (event -> listeners))))
      case Some(l) => {
        //require(1 ==2, "should not happen idk")
        val nw = (l ++ listeners)
       this
        //EventQueue(content + (a -> (map + (event -> (if(abs) { nw.toSet.toList } else nw ) ))))
      }
    }
  }
  
  def getListenersForObject(a: Addr, event:String) : List[(Abs, Abs)] = content.get(a) match {
    case None => List()
    case Some(map) => map.get(event) match {
      case None => List()
      case Some(l) => l
    }
  }
  
  def removeListeners(a: Addr, event:String): EventQueue[Addr, Abs] = content.get(a) match {
    case None => this
    case Some(map) => map.get(event) match {
      case None => this
      case Some(l) => EventQueue(content + (a -> (map - event)))
    }
  }
}

object EventQueue {
  def empty[Addr : Address, Abs : AbstractValue] =
    EventQueue(Map[Addr, Map[String, List[(Abs, Abs)]]]())
}
