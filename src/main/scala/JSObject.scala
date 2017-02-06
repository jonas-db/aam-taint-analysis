import scalaz.Scalaz._


//rename to ecmascriptobject?
//prototype should be an address, because changing a property of a prototype object is immutable
// thus any object with a reference to it keeps the old properties
// but when joining two prototypes, we "join two addresses", we can look them up, then join them
// but do we need to store it? -> problem in objectstore.extend, then join should return a new store

case class JSObject[Addr : Address, Exp : Expression, Abs : AbstractValue]
(prototype : Set[Addr] = Set[Addr](), 
    properties: Map[String, Set[JSProperty[Addr, Exp, Abs]]] = Map[String, Set[JSProperty[Addr, Exp, Abs]]]()) {

  def join(that: JSObject[Addr, Exp, Abs]) = {
//    System.err.println("@@@ joining objects");
//    System.err.println(this.toString());
//    System.err.println(that.toString());
    val x = that.properties.getOrElse("type", JSDataProperty(implicitly[AbstractValue[Abs]].inject(false)))
    val jso = JSObject[Addr, Exp, Abs](this.prototype ++ that.prototype, joinProperties(properties, that.properties))
    jso
  }
  
  def joinProperties(p1:Map[String, Set[JSProperty[Addr, Exp, Abs]]], p2:Map[String, Set[JSProperty[Addr, Exp, Abs]]]):Map[String, Set[JSProperty[Addr, Exp, Abs]]] = {
    val keys = p1.keys.toSet ++ p2.keys.toSet
    keys.foldLeft(Map[String, Set[JSProperty[Addr, Exp, Abs]]]())({ case (map, k) => {
      val v1 = p1.getOrElse(k, Set(JSDataProperty(implicitly[AbstractValue[Abs]].inject(false))))
      val v2 = p2.getOrElse(k, Set(JSDataProperty(implicitly[AbstractValue[Abs]].inject(false))))
      
      map + (k -> (v1 ++ v2))
    }})
  }
  
  def get(property: String, store:Store[Addr, Exp, Abs], ownProperties:Boolean=false):Set[JSProperty[Addr, Exp, Abs]] = {
    val res = properties.get(property)
    //println("size="+prototype.size)
    res match {     
      case None => prototype.size match {
         //TODO: return a property with value "undefined" #f, e.g. UndefinedProperty()
        case 0 => Set(JSDataProperty(implicitly[AbstractValue[Abs]].inject(false)))
        case _ => {
          println("ok more then one="+store.objectStore.lookup(prototype.head))
          if(ownProperties)
          {
            // we only look at our own properties, it was't defined so we return undefined
            Set(JSDataProperty(implicitly[AbstractValue[Abs]].inject(false)))
          }
          else
          {
            prototype.flatMap({ x => store.objectStore.lookup(x).get(property, store) })            
          }
        }
      }
      case Some(p) => p
    }
  }
  


  //
  def set(property: String, props: Set[JSProperty[Addr, Exp, Abs]]): JSObject[Addr, Exp, Abs] = 
  {
    //System.out.println("setting property="+property);
    
    JSObject(prototype, properties + (property -> props))
  }

  def setPrototype(proto: Set[Addr]): JSObject[Addr, Exp, Abs] = 
  {
    println("setting prototype="+proto);
    
    // Strong update
    if(prototype.size <= 1)
    {
      JSObject(proto, properties)
    }
    else
    {
      JSObject(prototype ++ proto, properties)
    }
  }  
  
  override def toString():String =
  {   
    var ps:Array[String] = new Array[String](0)
    
    for(oa <- properties.toIterator)
    {
      ps = ps :+ oa.toString();  
    }
    
    "[JSObject properties=["+ps.mkString(",")+"]]"
  }

//  override def equals(that:Any):Boolean =
//  { 
//    that match {
//      case o: JSObject[Addr, Exp, Abs] => {
//        properties.equals(o.properties)
//      }
//      case _ => false
//    }
//  }
  
  //TODO: def equals, because typeset uses subsumes == equals to check if it has to add or skip adding
  
  //def join(that : JSObject[Addr, Exp, Abs]):JSObject[Addr, Exp, Abs] = ???
  
}