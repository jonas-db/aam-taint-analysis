abstract class JSProperty[Addr : Address, Exp : Expression, Abs : AbstractValue] {
  // TODO: Enumerable, Configurable

}

case class JSDataProperty[Addr : Address, Exp : Expression, Abs : AbstractValue](value: Abs) 
        extends JSProperty[Addr, Exp, Abs] {
  // TODO: Writable
 
  override def toString(): String =
  {
    "[Data property: value="+value.toString()+"]";
  }
}

case class JSAccessorProperty[Addr : Address, Exp : Expression, Abs : AbstractValue](
    getter : Option[Abs] = None, 
    setter : Option[Abs] = None) extends JSProperty[Addr, Exp, Abs] {
  
  override def toString(): String =
  {
    "[Accessor property: getter="+getter.toString()+", setter="+setter.toString()+"]";
  }
}
