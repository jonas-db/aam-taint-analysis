import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

/**
 * Basic Scheme semantics, without any optimization
 */
class BaseSchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends BaseSemantics[SchemeExp, Abs, Addr, Time] {

  val schemeFalse = SchemeValue(ValueBoolean(false), scala.util.parsing.input.NoPosition)
  var eventID = 0;
  
  trait SchemeFrame extends Frame {
    def subsumes(that: Frame) = that.equals(this)
    override def toString = s"${this.getClass.getSimpleName}"
  }
  case class FrameFuncallOperator(fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameFuncallOperands(f: Abs, fexp: SchemeExp, cur: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameIf(cons: SchemeExp, alt: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLet(variable: String, bindings: List[(String, Abs)], toeval: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetStar(variable: String, bindings: List[(String, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameLetrec(addr: Addr, bindings: List[(Addr, SchemeExp)], body: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameSet(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameBegin(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCond(cons: List[SchemeExp], clauses: List[(SchemeExp, List[SchemeExp])], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCase(clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameAnd(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameOr(rest: List[SchemeExp], ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefine(variable: String, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasOld(variable: String, enew: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameCasNew(variable: String, old: Abs, ρ: Environment[Addr]) extends SchemeFrame
  
  /*
   * Object properties
   */
  case class FrameGetProperty(exp : SchemeExp, property: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameGetPropertyString(property: SchemeExp, obj: Abs, ρ: Environment[Addr]) extends SchemeFrame

  case class FrameDeleteProperty(exp : SchemeExp, property: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDeletePropertyString(property: SchemeExp, obj: Abs, ρ: Environment[Addr]) extends SchemeFrame  
  
  case class FrameSetPrototype(exp:SchemeExp, proto:SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameSetPrototype2(exp:SchemeExp, v:Abs, ρ: Environment[Addr]) extends SchemeFrame
  
  case class FrameSetProperty(exp : SchemeExp, property: SchemeExp, value : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame  
  case class FrameSetPropertyString(exp : SchemeExp, obj : Abs, value : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame  
  case class FrameSetPropertyValue(exp : SchemeExp, obj: Abs, property: Abs, ρ: Environment[Addr]) extends SchemeFrame 
  case class FrameSetPropertyReturn(ret : Abs) extends SchemeFrame
  case class FrameEvalSetProperties(exp : SchemeExp, value: Abs, property: String, props : List[JSProperty[Addr, SchemeExp, Abs]], objects:List[Addr], ρ: Environment[Addr]) extends SchemeFrame

  case class FrameDefineDataProperty(exp : SchemeExp, property: SchemeExp, value : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame  
  case class FrameDefineDataPropertyName(exp : SchemeExp, obj : Abs, value: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame 
  case class FrameDefineDataPropertyValue(exp : SchemeExp, obj : Abs, property: Abs, ρ: Environment[Addr]) extends SchemeFrame 
  //case class FrameSetPropertyReturn(ret : Abs) extends SchemeFrame  
  
  case class FrameDefineAccessorProperty(property: SchemeExp, getter : SchemeExp, setter : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefineAccessorPropertyName(obj: Abs, getter : SchemeExp, setter : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefineAccessorPropertySetter(property: Abs, obj: Abs, setter : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameDefineAccessorPropertyGetter(property: Abs, obj: Abs, getter : Abs, ρ: Environment[Addr]) extends SchemeFrame

  /*
   * Events
   */
  case class FrameAddEventListener(name: SchemeExp, handler: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameAddEventListenerName(obj:Abs, handler: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameAddEventListenerHandler(obj: Abs, name: Abs, ρ: Environment[Addr]) extends SchemeFrame

  case class FrameRemoveEventListener(name: SchemeExp, handler: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameRemoveEventListenerName(obj: Abs, handler: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameRemoveEventListenerHandler(obj: Abs, name: Abs, ρ: Environment[Addr]) extends SchemeFrame
  
  case class FrameDispatchEvent(exp : SchemeExp, evt: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameEvent(exp : SchemeExp, obj : Abs, ρ: Environment[Addr]) extends SchemeFrame  

  case class FrameEmitEvent(exp : SchemeExp, evt: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame
  case class FrameEmit(exp : SchemeExp, obj : Abs, ρ: Environment[Addr]) extends SchemeFrame   

  case class FrameFireNextListeners(exp: SchemeExp, listeners: List[(Abs, Abs)], obj: Addr, name: String, framefn: Int,ρ: Environment[Addr]) extends SchemeFrame     
  case class FrameFireEventQueue(exp: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame  
  case class FrameFireEventLoop(exp: SchemeExp, ρ: Environment[Addr]) extends SchemeFrame 
  case class FrameSink(v : SchemeExp, param : SchemeExp, ρ: Environment[Addr]) extends SchemeFrame 
  
  object FrameHalt extends SchemeFrame {
    override def toString() = "FHalt"
  }

  protected def evalBody(body: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs]): Action[SchemeExp, Abs, Addr] = body match {
    case Nil => ActionReachedValue(abs.inject(false), σ)
    case List(exp) => ActionEval(exp, ρ, σ)
    case exp :: rest => ActionPush(exp, FrameBegin(rest, ρ), ρ, σ)
  }

  def conditional(v: Abs, t: Action[SchemeExp, Abs, Addr], f: Action[SchemeExp, Abs, Addr]): Set[Action[SchemeExp, Abs, Addr]] =
    (if (abs.isTrue(v)) Set(t) else Set()) ++ (if (abs.isFalse(v)) Set(f) else Set())

  def evalCall(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
   //println("evalcall="+abs.getClosures[SchemeExp, Addr](function).size)
    val fromClo: Set[Action[SchemeExp, Abs, Addr]] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body, pos), ρ1) =>
        if (args.length == argsv.length) {
          bindArgs(args.zip(argsv), ρ1, σ, t) match {
            case (ρ2, σ) =>
              if (body.length == 1)
                ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), ρ1), body.head, ρ2, σ, argsv)
              else
                ActionStepIn[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), ρ1), SchemeBegin(body, pos), ρ2, σ, argsv)
          }
        } else { ActionError[SchemeExp, Abs, Addr](s"Arity error when calling $fexp (${args.length} arguments expected, got ${argsv.length})") }
      case (λ, _) => ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
    })
    val fromPrim = abs.getPrimitive[Addr, SchemeExp, Abs](function) match {
      case Some(prim) => prim.call(fexp, argsv, σ, t) match {
        case Right((res, σ2)) => Set(ActionReachedValue[SchemeExp, Abs, Addr](res, σ2))
        case Left(err) => Set(ActionError[SchemeExp, Abs, Addr](err))
      }
      case None => Set()
    }
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(ActionError(s"Called value is not a function: $function"))
    } else {
      fromClo ++ fromPrim
    }
  }

  def evalCallListener(function: Abs, fexp: SchemeExp, argsv: List[(SchemeExp, Abs)], ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
   //println("evalcall="+abs.getClosures[SchemeExp, Addr](function).size)
    val fromClo: Set[Action[SchemeExp, Abs, Addr]] = abs.getClosures[SchemeExp, Addr](function).map({
      case (SchemeLambda(args, body, pos), ρ1) =>
        if (args.length == argsv.length) {
          bindArgs(args.zip(argsv), ρ1, σ, t) match {
            case (ρ2, σ) =>
              if (body.length == 1)
                ActionStepInEvent[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), ρ1), body.head, ρ2, σ, argsv)
              else
                ActionStepInEvent[SchemeExp, Abs, Addr](fexp, (SchemeLambda(args, body, pos), ρ1), SchemeBegin(body, pos), ρ2, σ, argsv)
          }
        } else { ActionError[SchemeExp, Abs, Addr](s"Arity error when calling $fexp (${args.length} arguments expected, got ${argsv.length})") }
      case (λ, _) => ActionError[SchemeExp, Abs, Addr](s"Incorrect closure with lambda-expression ${λ}")
    })
    val fromPrim = abs.getPrimitive[Addr, SchemeExp, Abs](function) match {
      case Some(prim) => prim.call(fexp, argsv, σ, t) match {
        case Right((res, σ2)) => Set(ActionReachedValue[SchemeExp, Abs, Addr](res, σ2))
        case Left(err) => Set(ActionError[SchemeExp, Abs, Addr](err))
      }
      case None => Set()
    }
    if (fromClo.isEmpty && fromPrim.isEmpty) {
      Set(ActionError(s"Called value is not a function: $function"))
    } else {
      fromClo ++ fromPrim
    }
  }  
  
  protected def evalValue(v: Value): Option[Abs] = v match {
    case ValueString(s) => Some(abs.inject(s))
    case ValueInteger(n) => Some(abs.inject(n))
    case ValueBoolean(b) => Some(abs.inject(b))
    case _ => None
  }

  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ, t)
    case e :: rest => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
  }
  protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] =
    funcallArgs(f, fexp, List(), args, ρ, σ, t)

  protected def evalQuoted(exp: SExp, σ: Store[Addr, SchemeExp, Abs], t: Time): (Abs, Store[Addr, SchemeExp, Abs]) = exp match {
    case SExpIdentifier(sym) => (abs.injectSymbol(sym), σ)
    case SExpPair(car, cdr) => {
      val care: SchemeExp = SchemeIdentifier(car.toString, car.pos)
      val cdre: SchemeExp = SchemeIdentifier(cdr.toString, cdr.pos)
      val cara = addr.cell(care, t)
      val (carv, σ2) = evalQuoted(car, σ, t)
      val cdra = addr.cell(cdre, t)
      val (cdrv, σ3) = evalQuoted(cdr, σ2, t)
      (abs.cons(cara, cdra), σ3.extend(cara, carv).extend(cdra, cdrv))
    }
    case SExpValue(v) => (v match {
      case ValueString(str) => abs.inject(str)
      case ValueCharacter(c) => throw new Exception("character not yet supported")
      case ValueSymbol(sym) => abs.injectSymbol(sym) /* shouldn't happen */
      case ValueInteger(n) => abs.inject(n)
      case ValueFloat(n) => throw new Exception("floats not yet supported")
      case ValueBoolean(b) => abs.inject(b)
      case ValueNil() => abs.nil
    }, σ)
    case SExpQuoted(q) => evalQuoted(SExpPair(SExpIdentifier("quote"), SExpPair(q, SExpValue(ValueNil()))), σ, t)
  }

  def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time) = 
  {
    e match {
    case λ: SchemeLambda => Set(ActionReachedValue(abs.inject[SchemeExp, Addr]((λ, ρ)), σ))
    case SchemeFuncall(f, args,_) => Set(ActionPush(f, FrameFuncallOperator(f, args, ρ), ρ, σ))
    case SchemeIf(cond, cons, alt,_) => Set(ActionPush(cond, FrameIf(cons, alt, ρ), ρ, σ))
    case SchemeLet(Nil, body,_) => Set(evalBody(body, ρ, σ))
    case SchemeLet((v, exp) :: bindings, body,_) => Set(ActionPush(exp, FrameLet(v, List(), bindings, body, ρ), ρ, σ))
    case SchemeLetStar(Nil, body,_) => Set(evalBody(body, ρ, σ))
    case SchemeLetStar((v, exp) :: bindings, body,_) => Set(ActionPush(exp, FrameLetStar(v, bindings, body, ρ), ρ, σ))
    case SchemeLetrec(Nil, body,_) => Set(evalBody(body, ρ, σ))
    case SchemeLetrec((v, exp) :: bindings, body,_) => {
      val variables = v :: bindings.map(_._1)
      val addresses = variables.map(v => addr.variable(v, t))
      val (ρ1, σ1) = variables.zip(addresses).foldLeft((ρ, σ))({ case ((ρ, σ), (v, a)) => (ρ.extend(v, a), σ.extend(a, abs.bottom)) })
      Set(ActionPush(exp, FrameLetrec(addresses.head, addresses.tail.zip(bindings.map(_._2)), body, ρ1), ρ1, σ1))
    }
    case SchemeSet(variable, exp,_) => Set(ActionPush(exp, FrameSet(variable, ρ), ρ, σ))
    case SchemeBegin(body,_) => Set(evalBody(body, ρ, σ))
    case SchemeCond(Nil,_) => Set(ActionError(s"cond without clauses"))
    case SchemeCond((cond, cons) :: clauses,_) => Set(ActionPush(cond, FrameCond(cons, clauses, ρ), ρ, σ))
    case SchemeCase(key, clauses, default,_) => Set(ActionPush(key, FrameCase(clauses, default, ρ), ρ, σ))
    case SchemeAnd(Nil,_) => Set(ActionReachedValue(abs.inject(true), σ))
    case SchemeAnd(exp :: exps,_) => Set(ActionPush(exp, FrameAnd(exps, ρ), ρ, σ))
    case SchemeOr(Nil,_) => Set(ActionReachedValue(abs.inject(false), σ))
    case SchemeOr(exp :: exps,_) => Set(ActionPush(exp, FrameOr(exps, ρ), ρ, σ))
    case SchemeDefineVariable(name, exp,_) => Set(ActionPush(exp, FrameDefine(name, ρ), ρ, σ))
    case SchemeDefineFunction(name, args, body, pos) => {
      val a = addr.variable(name, t)
      val v = abs.inject[SchemeExp, Addr]((SchemeLambda(args, body, pos), ρ))
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      Set(ActionReachedValue(v, σ))
    }
    case SchemeIdentifier(name,_) => ρ.lookup(name) match {
      case Some(a) => Set(ActionReachedValue(σ.lookup(a), σ, Set[Addr](a))) /* reads on a */
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case SchemeQuoted(quoted,_) => evalQuoted(quoted, σ, t) match {
      case (value, σ2) => Set(ActionReachedValue(value, σ2))
    }
    case SchemeValue(v,_) => evalValue(v) match {
      case Some(v) => Set(ActionReachedValue(v, σ))
      case None => Set(ActionError(s"Unhandled value: $v"))
    }
    case SchemeCas(variable, eold, enew,_) => Set(ActionPush(eold, FrameCasOld(variable, enew, ρ), ρ, σ))
    case SchemeAcquire(variable,_) => ρ.lookup(variable) match {
      case Some(a) => {
        val v = σ.lookup(a)
        /* Only performs a step if the lock is possibly unlocked (true is unlocked, false is locked) */
        if (abs.isTrue(v)) Set(ActionReachedValue(abs.inject(true), σ.update(a, abs.inject(false)))) else Set()
      }
      case None => Set(ActionError(s"Unbound variable: $variable"))
    }
    case SchemeRelease(variable,_) => ρ.lookup(variable) match {
      case Some(a) => Set(ActionReachedValue(abs.inject(true), σ.update(a, abs.inject(true))))
      case None => Set(ActionError(s"Unbound variable: $variable"))
    }
    /*
     * Object properties
     */
    case SchemeSink(obj,_) => Set(ActionPush(obj, FrameSink(e, obj, ρ), ρ, σ))
    case SchemeGetProperty(property, obj,_) => Set(ActionPush(obj, FrameGetProperty(e, property, ρ), ρ, σ))
    case SchemeDeleteProperty(property, obj,_) => Set(ActionPush(obj, FrameDeleteProperty(e, property, ρ), ρ, σ))
    case SchemeSetProperty(property, obj, value,_) => Set(ActionPush(obj, FrameSetProperty(e, property, value, ρ), ρ, σ)) 
    case SchemeSetPrototype(obj, proto, _) => Set(ActionPush(obj, FrameSetPrototype(e, proto, ρ), ρ, σ)) 

    case SchemeDefineDataProperty(property, obj, value,_) 
      => Set(ActionPush(obj, FrameDefineDataProperty(e, property, value, ρ), ρ, σ)) 

    case SchemeDefineAccessorProperty(property, obj, getter, setter,_) 
      => Set(ActionPush(obj, FrameDefineAccessorProperty(property, getter, setter, ρ), ρ, σ)) 
      
    /*
     * Events  
     */
    case SchemeAddEventListener(obj, name, handler,_) => Set(ActionPush(obj, FrameAddEventListener(name, handler, ρ), ρ, σ))
    case SchemeRemoveEventListener(obj, name, handler,_) => Set(ActionPush(obj, FrameRemoveEventListener(name, handler, ρ), ρ, σ))
    case SchemeDispatchEvent(obj, event,_) => Set(ActionPush(obj, FrameDispatchEvent(e, event, ρ), ρ, σ))  
    case SchemeEmitEvent(obj, event,_) => Set(ActionPush(obj, FrameEmitEvent(e, event, ρ), ρ, σ)) 
    case SchemeEventLoop(_) => fireEventLoop(e, ρ, σ, t)
    case SchemeEventQueue(_) => fireEventQueue(e, ρ, σ, t)    
  }}
  
  def stepKont(v: Abs, frame: Frame, σ: Store[Addr, SchemeExp, Abs], t: Time) = frame match {
    case FrameHalt => Set()
    case FrameFuncallOperator(fexp, args, ρ) => funcallArgs(v, fexp, args, ρ, σ, t)
    case FrameFuncallOperands(f, fexp, exp, args, toeval, ρ) => funcallArgs(f, fexp, (exp, v) :: args, toeval, ρ, σ, t)
    case FrameIf(cons, alt, ρ) =>
      conditional(v, ActionEval(cons, ρ, σ), ActionEval(alt, ρ, σ))
    case FrameLet(name, bindings, Nil, body, ρ) => {
      val variables = name :: bindings.reverse.map(_._1)
      val addresses = variables.map(v => addr.variable(v, t))
      val (ρ1, σ1) = ((name, v) :: bindings).zip(addresses).foldLeft((ρ, σ))({
        case ((ρ, σ), ((variable, value), a)) => (ρ.extend(variable, a), σ.extend(a, value))
      })
      Set(evalBody(body, ρ1, σ1))
    }
    case FrameLet(name, bindings, (variable, e) :: toeval, body, ρ) =>
      Set(ActionPush(e, FrameLet(variable, (name, v) :: bindings, toeval, body, ρ), ρ, σ))
    case FrameLetStar(name, bindings, body, ρ) => {
      val a = addr.variable(name, t)
      val ρ1 = ρ.extend(name, a)
      val σ1 = σ.extend(a, v)
      bindings match {
        case Nil => Set(evalBody(body, ρ1, σ1))
        case (variable, exp) :: rest => Set(ActionPush(exp, FrameLetStar(variable, rest, body, ρ1), ρ1, σ1))
      }
    }
    case FrameLetrec(a, Nil, body, ρ) => Set(evalBody(body, ρ, σ.update(a, v)))
    case FrameLetrec(a, (a1, exp) :: rest, body, ρ) =>
      Set(ActionPush(exp, FrameLetrec(a1, rest, body, ρ), ρ, σ.update(a, v)))
    case FrameSet(name, ρ) => ρ.lookup(name) match {
      case Some(a) => {
        Set(ActionReachedValue(abs.inject(false), σ.update(a, v), Set[Addr](), Set[Addr](a))) /* writes on a */
      }
      case None => Set(ActionError(s"Unbound variable: $name"))
    }
    case FrameBegin(body, ρ) => Set(evalBody(body, ρ, σ))
    case FrameCond(cons, clauses, ρ) =>
      conditional(v, if (cons.isEmpty) { ActionReachedValue(v, σ) } else { evalBody(cons, ρ, σ) },
        clauses match {
          case Nil => ActionReachedValue(abs.inject(false), σ)
          case (exp, cons2) :: rest => ActionPush(exp, FrameCond(cons2, rest, ρ), ρ, σ)
        })
    case FrameCase(clauses, default, ρ) => {
      val fromClauses = clauses.flatMap({ case (values, body) =>
        if (values.exists(v2 => evalValue(v2.value) match {
          case None => false
          case Some(v2) => abs.subsumes(v, v2)
        }))
          /* TODO: precision could be improved by restricting v to v2 */
          Set[Action[SchemeExp, Abs, Addr]](evalBody(body, ρ, σ))
        else
          Set[Action[SchemeExp, Abs, Addr]]()
      })
      /* TODO: precision could be improved in cases where we know that default is not
       * reachable */
      fromClauses.toSet + evalBody(default, ρ, σ)
    }
    case FrameAnd(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameAnd(e :: rest, ρ) =>
      conditional(v, ActionPush(e, FrameAnd(rest, ρ), ρ, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameOr(Nil, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionReachedValue(abs.inject(false), σ))
    case FrameOr(e :: rest, ρ) =>
      conditional(v, ActionReachedValue(v, σ), ActionPush(e, FrameOr(rest, ρ), ρ, σ))
    case FrameDefine(name, ρ) => throw new Exception(s"TODO: define not handled (no global environment)")
    case FrameCasOld(variable, enew, ρ) =>
      Set(ActionPush(enew, FrameCasNew(variable, v, ρ), ρ, σ))
    case FrameCasNew(variable, old, ρ) =>
      ρ.lookup(variable) match {
        case Some(a) => conditional(abs.binaryOp(BinaryOperator.Eq)(σ.lookup(a), old),
          /* Compare and swap succeeds */
          ActionReachedValue(abs.inject(true), σ.update(a, v)),
          /* Compare and swap fails */
          ActionReachedValue(abs.inject(false), σ))
        case None => Set(ActionError(s"Unbound variable: $variable"))
      }
      
      /*
       * Object properties
       */
    case FrameSink(exp, paramexp, ρ) => {
      val res = abs.sink(v)
      abs.isError(res) match {
        case true => {
          if(res.toString().contains("MAYBE_TAINTED"))
          {
            Set(ActionError[SchemeExp, Abs,Addr](paramexp.toString()+" is MAYBE_TAINTED ("+exp.pos+")")) //abs.error(abs.inject(exp.toString()+" is maybe tainted"))
          }
          else if(res.toString().contains("IS_TAINTED"))
          {
            Set(ActionError[SchemeExp, Abs,Addr](paramexp.toString()+" is IS_TAINTED ("+exp.pos+")")) //abs.error(abs.inject(exp.toString()+" is tainted"))
          }
          else
          {
            Set(ActionReachedValue(res, σ))
          }
        }
        case false => Set(ActionReachedValue(res, σ))
      }
    }
    case FrameSetPrototype(exp, proto, ρ) => Set(ActionPush(proto, FrameSetPrototype2(exp, v, ρ), ρ, σ))
    case FrameSetPrototype2(exp, obj, ρ) => {
      var objs = abs.objects(obj);
      var protos = abs.objects(v);
      
      var result = Set[Action[SchemeExp, Abs, Addr]]()
      
      // for each object, set the prototype
      for(oa <- objs.toIterator)
      {  
        result = result ++ Set(ActionReachedValue(abs.inject(false), σ.setPrototype(oa, protos)))
      }
      
      result
    }
    case FrameDeleteProperty(exp, property, ρ) => Set(ActionPush(property, FrameDeletePropertyString(exp, v, ρ), ρ, σ))   
    case FrameDeletePropertyString(exp, obj, ρ) => {
      val objs = abs.objects(obj)
      val names = abs.symbols(v)
      var result = Set[Action[SchemeExp, Abs, Addr]]()
      
      // for each object
      for(oa <- objs.toIterator)
      {    
        for(name <- names)
        {
          println("delete "+name)
          val σ2 = σ.deleteProperty(oa, name)
          // return value is always true because we do not support non-writeable props
          result = result ++ Set(ActionReachedValue(abs.inject(true), σ2))         
        }     
      }

      result      
    }
    case FrameGetProperty(exp, property, ρ) => {
      if(abs.objects(v).size > 0)
        Set(ActionPush(property, FrameGetPropertyString(exp, v, ρ), ρ, σ))
      else
        Set(ActionError("No object given"))
    }
    case FrameGetPropertyString(exp, obj, ρ) => {     
      val objs = abs.objects(obj)
      val names = abs.symbols(v)
      var result = Set[Action[SchemeExp, Abs, Addr]]()
      
      // for each object
      for(oa <- objs.toIterator)
      {    
        for(name <- names)
        {
          // obj has for each property a list of properties, possibly with multiple values
          val properties = σ.objectLookup(oa).get(name, σ)
        
          // for each property join their value..
          result = result ++ evalGetProperties(exp, properties.toList, ρ, σ, t)           
        }     
      }

      result
    }
    case FrameSetProperty(exp, property, value, ρ) => Set(ActionPush(property, FrameSetPropertyString(exp, v, value, ρ), ρ, σ))
    case FrameSetPropertyString(exp, obj, value, ρ) => Set(ActionPush(value, FrameSetPropertyValue(exp,obj, v, ρ), ρ, σ))
    case FrameSetPropertyValue(exp, o, property, ρ) => {
      //System.err.println("setting property="+property)
      val newValue = v
      
      // get set of addrs and look them up
      val objs = abs.objects(o)
      val objList = objs.toList
              println("objs size="+objList.size)
      val names = abs.symbols(property)

      require(objs.size > 0, "FrameSetPropertyValue: no object found");      

      var result = Set[Action[SchemeExp, Abs, Addr]]()
      for(o <- objs)
      {
        for(name <- names)
        {
          result = result ++ setProperty(exp, newValue, name, o, ρ, σ, t)        
        }       
      }
      println("size="+result.size)
      // set is a simple assignment, so we simply return the value given as argument
      result
    }
    case FrameSetPropertyReturn(ret) => Set(ActionReachedValue(ret, σ))
    // discard return value
//    case FrameEvalSetProperties(exp, value, property, props, objects, ρ) 
//      => evalSetProperties(exp, value, property, props, objects, ρ, σ, t)
      
    case FrameDefineDataProperty(exp, property, value, ρ) => 
      Set(ActionPush(property, FrameDefineDataPropertyName(exp, v, value, ρ), ρ, σ))
    case FrameDefineDataPropertyName(exp, obj, value, ρ) => 
      Set(ActionPush(value, FrameDefineDataPropertyValue(exp, obj, v, ρ), ρ, σ))
      
    case FrameDefineDataPropertyValue(exp, o, property, ρ) => {  
      val objs = abs.objects(o)
      val names = abs.symbols(property)

      require(objs.size > 0, "FrameDefineDataPropertyValue: no object found");
      println(objs)
      
      var results = Set[Action[SchemeExp, Abs, Addr]]()
      
      // for each obj
      for(oa <- objs.toIterator)
      {
        // for each property of that obj
        for(name <- names)
        {
          val σ2 = σ.defineDataProperty(oa, name, JSDataProperty[Addr, SchemeExp, Abs](v))
          results = results ++ Set(ActionReachedValue(o, σ2))
        }
      }
      
      results
    }

    case FrameDefineAccessorProperty(property, getter, setter, ρ) 
      => Set(ActionPush(property, FrameDefineAccessorPropertyName(v, getter, setter, ρ), ρ, σ))
    case FrameDefineAccessorPropertyName(obj, getter, setter, ρ) 
      => Set(ActionPush(getter, FrameDefineAccessorPropertySetter(v, obj, setter, ρ), ρ, σ))
    case FrameDefineAccessorPropertySetter(property, obj, setter, ρ) 
      => Set(ActionPush(setter, FrameDefineAccessorPropertyGetter(property, obj, v, ρ), ρ, σ))
    case FrameDefineAccessorPropertyGetter(property, obj, getter, ρ) => {
      val setter = v
      
      val objs = abs.objects(obj);
      val names = abs.symbols(property);
      println(names)
      require(objs.size > 0, "FrameDefineAccessorPropertyGetter: no object found");
      
      // for each object, add the accessor property
      var results = Set[Action[SchemeExp, Abs, Addr]]()   
      
      for(oa <- objs.toIterator)
      {
        for(name <- names)
        {
          val σ2 = σ.defineDataProperty(oa, name, JSAccessorProperty[Addr, SchemeExp, Abs](Some(getter), Some(setter)))
          results = results ++ Set(ActionReachedValue(obj, σ2))
        }
      }
       
      results
    }
    
    /*
     * Events
     */
    case FrameAddEventListener(name, handler, ρ)
      => Set(ActionPush(name, FrameAddEventListenerName(v, handler, ρ), ρ, σ))
    case FrameAddEventListenerName(obj, handler, ρ)
      => Set(ActionPush(handler, FrameAddEventListenerHandler(obj, v, ρ), ρ, σ))      
    case FrameAddEventListenerHandler(obj, event, ρ) => 
    {
        val handler = v;
        val addrs = abs.objects(obj);
        val names = abs.symbols(event);
        
        require(addrs.size > 0, "FrameAddEventListenerHandler: no object found");
        require(abs.getClosures[SchemeExp, Addr](handler).size > 0, "FrameAddEventListenerHandler: no closure found");
        
        // for each object, add the handler
        var result = Set[Action[SchemeExp, Abs, Addr]]()
        
        // for each object
        for(oa <- addrs.toIterator)
        {
          // for each event
          for(name <- names)
          {
            val σ2 = σ.addEventListener(oa, name, handler);
            if(σ == σ2)
            {
              System.out.println("already existing event listener: "+oa.toString()+","+name+", objs="+addrs.size)
              result = result ++ Set(ActionReachedValue(abs.inject(false), σ2)) // no return value
            }
            else
            {
              System.out.println("new event listener: "+oa.toString()+","+name+", objs="+addrs.size)
              result = result ++ Set(ActionReachedValueContext(abs.inject(false), σ2, obj, name, handler)) // no return value
              
            }
          }        
        }        

        result
    }

    case FrameRemoveEventListener(name, handler, ρ)
      => Set(ActionPush(name, FrameRemoveEventListenerName(v, handler, ρ), ρ, σ))      
    case FrameRemoveEventListenerName(obj, handler, ρ)
      => Set(ActionPush(handler, FrameRemoveEventListenerHandler(obj, v, ρ), ρ, σ)) 
    case FrameRemoveEventListenerHandler(obj, event, ρ) => 
    {
        val handler = v;
        val addrs = abs.objects(obj);
        val names = abs.symbols(event);
        
        require(addrs.size > 0, "FrameRemoveEventListenerHandler: no object found");

        // for each object, add the handler
        var result = Set[Action[SchemeExp, Abs, Addr]]()
        
        // for each object
        for(oa <- addrs.toIterator)
        {
          // for each event
          for(name <- names)
          {
            val σ2 = σ.removeEventListener(oa, name, handler);
            System.out.println("remove event listener: "+oa.toString()+","+name+", objs="+addrs.size)
            result = result ++ Set(ActionReachedValue(abs.inject(false), σ2)) // no return value
          }        
        }        

        result        
    }     
 
    case FrameEmitEvent(exp, event, ρ)
        => Set(ActionPush(event, FrameEmit(exp, v, ρ), ρ, σ))    
    
    case FrameDispatchEvent(exp, event, ρ)
        => Set(ActionPush(event, FrameEvent(exp, v, ρ), ρ, σ))

    case FrameEmit(exp, obj, ρ) => {
      val addrs = abs.objects(obj);
      val eventObjects = abs.objects(v)
      
      require(addrs.size > 0, "FrameEvent: no object found to dispatch");
      //require(abs.objects(v).size > 1, "FrameEvent: multiple event objects");
            
      var results = Set[Action[SchemeExp, Abs, Addr]]()
      //for each object
      for(oa <- addrs.toIterator)
      { 
        // for each event object
        for(evtobj <- eventObjects.toIterator)
        {
          // for each property contained for "type" of the event object
          val eventTypes = σ.objectStore.lookup(evtobj).get("type", σ)
          for(evtType <- eventTypes.toIterator)
          {
            evtType match {
              case JSDataProperty(absv) => {
                // for each symbol in the abs value
                for(name <- abs.symbols(absv).toIterator)
                { 
                  println("scheduling")
                  println(σ.getEventListeners(oa, name))
                  // get listeners and map it to the event obj
                  val listeners = σ.getEventListeners(oa, name).map { x => (x, v) }
                  val σ2 = σ.scheduleListeners(oa, name, listeners, false)
                  results = results ++ Set( ActionReachedValue(abs.inject(true), σ2))                      
                }
              }
              //TODO: accessor properties, execute getter and get result
              // we know that it always be a data property, so we can ignore this?
              case _ =>  require(true, "FrameEvent: accessorproperty..")
            }
          }
        }
      }
      
      results
    }        
        
    case FrameEvent(exp, obj, ρ) => {
      val addrs = abs.objects(obj);
      val eventObjects = abs.objects(v)
      
      require(addrs.size > 0, "FrameEvent: no object found to dispatch");
      //require(abs.objects(v).size > 1, "FrameEvent: multiple event objects");
            
      var results = Set[Action[SchemeExp, Abs, Addr]]()
      //for each object
      for(oa <- addrs.toIterator)
      { 
        // for each event object
        for(evtobj <- eventObjects.toIterator)
        {
          // for each property contained for "type" of the event object
          val eventTypes = σ.objectStore.lookup(evtobj).get("type", σ)
          for(evtType <- eventTypes.toIterator)
          {
            evtType match {
              case JSDataProperty(absv) => {
                // for each symbol in the abs value
                for(name <- abs.symbols(absv).toIterator)
                { 
                  val listeners = σ.getEventListeners(oa, name); 
                  System.out.println("dispatchin event="+name+" to listener, size= "+listeners.size)
                  val l = listeners.map(x => (x, abs.obj[Addr](evtobj)))
                  
                  //if joined or not
                  val orderedListeners = createCombinations(oa, l, σ)     
          
                  for (l <- orderedListeners) 
                  { 
                    val result = fireOrderedListeners(exp, l, oa, name, 1, ρ, σ, t, true)
          
                    for (ac <- result) 
                    { 
                      results += ac
                    } 
                  }                       
                }
              }
              //TODO: accessor properties, execute getter and get result
              // we know that it always be a data property, so we can ignore this?
              case _ =>  require(1 == 2, "FrameEvent: accessorproperty..")
            }
          }
        }
      }
      
      results
    }        

    case FrameFireNextListeners(exp, rest, obj, name, eventLoop, ρ) => {
        fireOrderedListeners(exp, rest, obj, name, eventLoop, ρ, σ, t)
    }
    case FrameFireEventQueue(exp, ρ) => {
        fireEventQueue(exp, ρ, σ, t)
    }
    case FrameFireEventLoop(exp, ρ) => {
        fireEventLoop(exp, ρ, σ, t)
    } 

    case FrameFireEventLoop2(exp, obj, name, ρ) => {    
      Set(ActionPushContext[SchemeExp, Abs, Addr](schemeFalse, obj, name, FrameFireEventLoop(exp, ρ), ρ, σ))
      //fireEventLoop(exp, ρ, σ, t)
    }    
    
  }

  case class FrameFireEventLoop2(exp: SchemeExp, obj : Addr, name: String, ρ: Environment[Addr]) extends SchemeFrame
  
def power[A](s: List[A]): List[List[A]] = {
        @annotation.tailrec 
        def pwr(s: List[A], acc: List[List[A]]): List[List[A]] = s match {
          case Nil     => acc 
          case a :: as => pwr(as, acc ::: (acc map (a :: _)))
        }
       pwr(s, Nil :: Nil)
      }  
  


     
// if joined, we create powerset of listeners, list  
// problem a b a -> a has twice the listeners of a then b
def createCombinations(obj:Addr, listeners: List[(Abs,Abs)],σ: Store[Addr, SchemeExp, Abs]): List[List[(Abs,Abs)]] = {        
    //println("lattice name="+implicitly[AbstractValue[Abs]].name)
    
    if(σ.listenerStore.isJoined(obj))
    {
      //require(1==2, "check")
      println("joined")
      val pset = (0 to listeners.size) flatMap listeners.combinations
    
      // all permutations of all combinations, because we don't which event listener 
      // belongs this object + we don't know in which order
      pset.toList.foldLeft(Set[List[(Abs,Abs)]]())(
          { case (acc, x) => (x.permutations.toSet ++ acc) }).toList
    }
    // else we didn't join, but we are in abstract lattice -> permutate
    //TODO:hack
    else if(implicitly[AbstractValue[Abs]].name == "(TypeSet, MaybeTaint)")
    {
      //println(listeners.size+","+listeners.permutations.toList.size)
      val r = listeners.permutations.toList
//      r match {
//        case x :: Nil => println("okkk")
//      }
      r
    }
    else
    {
      List(listeners)
    }
}

  protected def fireEventQueue  
  (
     exp : SchemeExp,
     ρ: Environment[Addr], 
     σ: Store[Addr, SchemeExp, Abs], 
     t: Time) : Set[Action[SchemeExp, Abs, Addr]] = 
  {
    val results = ListBuffer[Action[SchemeExp, Abs, Addr]]() 

    for ((obj, map) <- σ.eventQueue.content) 
    {
      for ((name, listeners) <- map) 
      {
        println("scheduling event="+name)

        var orderedListeners = createCombinations(obj, listeners, σ)
        println("combs="+orderedListeners.size)
        val σ3 = σ.removeFromQueue(obj, name);      
//var xp:SchemeExp = SchemeValue(ValueString("(event-loop " + name + ")"), scala.util.parsing.input.NoPosition)
        for (l <- orderedListeners) 
        { 
          val result = fireOrderedListeners(exp, l, obj, name, 2, ρ, σ3, t, true)

          for (ac <- result) 
          { 
            results += ac
          } 
        }         
       
      }
    }

    results.toSet
  }   
  
  protected def fireEventLoop
  (
     exp : SchemeExp,
     ρ: Environment[Addr], 
     σ: Store[Addr, SchemeExp, Abs], 
     t: Time) : Set[Action[SchemeExp, Abs, Addr]] = 
  {
    val results = ListBuffer[Action[SchemeExp, Abs, Addr]]() 
    
    for ((obj, (count, listenersMap)) <- σ.listenerStore.content) 
    {
      for ((name, listeners) <- listenersMap) 
      {
        //println("scheduling event="+name)

        //TODO: exp -> ? everyobj has same exp
        var xp:SchemeExp = SchemeValue(ValueString("(event-loop " + name + ")"), scala.util.parsing.input.NoPosition)
        //xp = exp
        val obja = addr.cell[SchemeExp, Time](xp, t)
        val event = abs.obj(obja)
        
        val σ2 = σ.extendObject(obja, 
            new JSObject[Addr, SchemeExp, Abs]
                (Set[Addr](), 
                 Map[String, Set[JSProperty[Addr, SchemeExp, Abs]]]
                  ("type" -> Set(JSDataProperty(abs.injectSymbol(name))))))        
        
//        val event = abs.inject(false)
//        val σ2 = σ
        
        val l = listeners.map(x => (x, event))
        
        var orderedListeners = createCombinations(obj, l, σ2)

        for (ol <- orderedListeners) 
        { 
          val result = fireOrderedListeners(xp, ol, obj, name, 0, ρ, σ2, t, true)

          for (ac <- result) 
          { 
            results += ac
          } 
        }        
      }
    }

    results.toSet
  } 
 

//    protected def fireEventLoop2  
//  (
//     exp : SchemeExp,
//     ρ: Environment[Addr], 
//     σ: Store[Addr, SchemeExp, Abs], 
//     t: Time) : Set[Action[SchemeExp, Abs, Addr]] = 
//  {
//    val results = ListBuffer[Action[SchemeExp, Abs, Addr]]() 
//    
//    for ((obj, (_, ls)) <- σ.getEvenListenerStore()) 
//    {
//      for ((name,l) <- ls) 
//      {
//        println("scheduling event="+name)
//        
//        var xp:SchemeExp = SchemeValue(ValueString("(event-loop " + name + ")"), scala.util.parsing.input.NoPosition)
//        //xp = exp
//        val obja = addr.cell[SchemeExp, Time](xp, t)
//        val event = abs.obj(obja)
//        
//        val σ2 = σ.extendObject(obja, 
//            new JSObject[Addr, SchemeExp, Abs]
//                (Set[Addr](), 
//                 Map[String, Set[JSProperty[Addr, SchemeExp, Abs]]]
//                  ("type" -> Set(JSDataProperty(abs.injectSymbol(name))))))   
//        val actions: Set[Action[SchemeExp, Abs, Addr]] 
//          = evalCall(l.head, exp, List[(SchemeExp, Abs)]((exp, event)), ρ, σ, t)
//
//        val result: Set[Action[SchemeExp, Abs, Addr]] = actions.map({
//            case (ActionStepIn(_, _, e, ρ2, σ2, _,_, _)) => {  
//              ActionPushEvent[SchemeExp, Abs, Addr](e, FrameFireEventLoop2(exp, obj, name, ρ), name,ρ2, σ2)
//              //ActionPush[SchemeExp, Abs, Addr](e, FrameFireEventLoop2(exp, obj, name, ρ), ρ2, σ2)
//            }
//        })
//  
//        for (ac <- result) 
//        { 
//          results += ac
//        }        
//      }
//    }
//
//    results.toSet
//  }  
  
    // we keep executing the event listeners, until we rech the last one
    // DispatchedActionReachedValue changes context
  protected def fireOrderedListeners    
  (
     exp : SchemeExp,
     listeners : List[(Abs,Abs)],
     obj : Addr,
     name : String,
     frame:Int,
     ρ: Environment[Addr], 
     σ: Store[Addr, SchemeExp, Abs], 
     t: Time, first: Boolean=  false) : Set[Action[SchemeExp, Abs, Addr]] = listeners match
  {
    case Nil =>
    {
      frame match {
        // event loop
        case 0 => Set(ActionPushContext[SchemeExp, Abs, Addr](schemeFalse, obj, name, FrameFireEventLoop(exp, ρ), ρ, σ)) 
        // dispatch event
        case 1 => Set(DispatchedActionReachedValue(abs.inject(true), obj, name, σ))
        // event queue
        case 2 => Set(ActionPushContext[SchemeExp, Abs, Addr](schemeFalse, obj, name, FrameFireEventQueue(exp, ρ), ρ, σ))
      }
    }
    case (listener,arg) :: rest =>
    {
      //println(name+","+listeners.size);
      
      //TODO: exp -> ??
      val actions: Set[Action[SchemeExp, Abs, Addr]] 
        = evalCallListener(listener, exp, List[(SchemeExp, Abs)]((exp, arg)), ρ, σ, t)
      val closures = abs.getClosures[SchemeExp, Addr](listener).map({ case(exp, env) => exp })
      val edgeLabel = name //+ "," + closures
      
      val result: Set[Action[SchemeExp, Abs, Addr]] = actions.map({
          case (ActionStepInEvent(_, _, e, ρ2, σ2, _,_, _)) => {
            //TODO: too lazy
            if(first)
            ActionPushEvent[SchemeExp, Abs, Addr](e, FrameFireNextListeners(exp, rest, obj, name, frame, ρ), edgeLabel,ρ2, σ2)
            else
            ActionPush[SchemeExp, Abs, Addr](e, FrameFireNextListeners(exp, rest, obj, name, frame, ρ), ρ2, σ2)  
          }
      })
      
      result
    }
  }   
    
    protected def setProperty
    (
     exp : SchemeExp,
     newVal : Abs,
     property : String, 
     obj : Addr,
     ρ: Environment[Addr], 
     σ: Store[Addr, SchemeExp, Abs], t: Time) : Set[Action[SchemeExp, Abs, Addr]] =  
  {
      
      //lookup
      val ob = σ.objectLookup(obj)
      
      // obj has for each property a list of properties, possibly with multiple values
      val properties = ob.get(property, σ, true)    
      var results = Set[Action[SchemeExp, Abs, Addr]]()
      
      for(prop <- properties)
      {
        val res = prop match {          
          // if it is a DataNamedProperty we have to return the new value
          case JSDataProperty(_) => {
            //update the store, because in this branch we update the data property
            val σ2 = σ.setProperty(obj, property, JSDataProperty(newVal))
            Set( ActionReachedValue( newVal, σ2))  
          }
          
          // if it is a DataAccessNamedProperty we have undefined(=None) or a closure(=Some(callback))
          // we have to evaluate the callback
          case JSAccessorProperty(_, setter) => setter match {
            // no setter, so just add it
            case None => Set() 
            // there is a setter, call it and add it back to the list
            case Some(setter) => {
              
              // eval setter, call it with the value as argument
              val actions: Set[Action[SchemeExp, Abs, Addr]] 
                = evalCall(setter, exp, List[(SchemeExp, Abs)]((exp, newVal)), ρ, σ, t)
    
              val result: Set[Action[SchemeExp, Abs, Addr]] = actions.map({
                  case (ActionStepIn(_, _, e, ρ2, σ2, _,_, _)) => {
                    // evaluate body, once done return the newval result
                    ActionPush[SchemeExp, Abs, Addr](e, FrameSetPropertyReturn(newVal), ρ2, σ2)
                  }
              })
              
              result
            }
          }      
        }
        
        results = results ++ res
      }
      
      results
  }
     
  
  protected def evalGetProperties
    (exp : SchemeExp, props : List[JSProperty[Addr, SchemeExp, Abs]], ρ: Environment[Addr], 
     σ: Store[Addr, SchemeExp, Abs], t: Time)
          : Set[Action[SchemeExp, Abs, Addr]] = props match 
  {
    case Nil => 
    {
      
       // we have all values, join them and return the value
      //val result = done.foldLeft(abs.bottom)((acc, x) => abs.join(acc, x))
      //System.err.println("reached=");
      Set()
    }
    case e :: rest => {
      //System.err.println("evalget, left="+rest.size);
      
      val actions = e match {    
        // if it is a DataNamedProperty we have undefined(=None) or a abstract value (=Some(value))
        // absv does not need to be evaluated anymore, cons it to the done list and recusively call ourselves
        case JSDataProperty(absv) =>  Set(ActionReachedValue(absv, σ))        
        
        // if it is a DataAccessNamedProperty we have undefined(=None) or a closure(=Some(callback))
        // we have to evaluate the callback
        case JSAccessorProperty(getter, _) => getter match {
          // no getter, so undefined is returned
          // TODO: bottom instead of false?
          case None => { 
            //NOT SUPPORTED
            Set(ActionReachedValue(abs.inject(false),  σ))
          }
          // there is a getter, call it and return the value, which we add to the list
          case Some(getter) => {
            //System.err.println("some getter");
            // eval getter
            val actions: Set[Action[SchemeExp, Abs, Addr]] 
              = evalCall(getter, exp, List[(SchemeExp, Abs)](), ρ, σ, t)
  
            actions
//            val result: Set[Action[SchemeExp, Abs, Addr]] = actions.map({
//                case (ActionStepIn(_, _, e, ρ2, σ2, _,_, _)) => {
//                  ActionPush[SchemeExp, Abs, Addr](e, FrameEvalProperties(exp, rest, done, ρ), ρ2, σ2)
//                }
//            })            
//              
//           result
          }
        }
      }
      
      actions ++ evalGetProperties(exp, rest, ρ, σ, t) 
    }
  }    
  
  def parse(program: String): SchemeExp = Scheme.parse(program)
}

/**
 * Extend base Scheme semantics with:
 *   - atomic evaluation: parts of some constructs can be evaluated atomically
 *     without needing to introduce more states in the state graph. For example,
 *     (+ 1 1) can directly be evaluated to 2 without modifying the store. Also,
 *     to evaluate (+ 1 (f)), we can directly push the continuation and jump to
 *     the evaluation of (f), instead of evaluating +, and 1 in separate states.
 */
class SchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends BaseSchemeSemantics[Abs, Addr, Time] {

  /** Tries to perform atomic evaluation of an expression. Returns the result of
    * the evaluation if it succeeded, otherwise returns None */
  protected def atomicEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs]): Option[(Abs, Set[Addr])] = e match {
    case λ: SchemeLambda => Some((abs.inject[SchemeExp, Addr]((λ, ρ)), Set[Addr]()))
    case SchemeIdentifier(name,_) => ρ.lookup(name).map(a => (σ.lookup(a), Set[Addr](a)))
    case SchemeValue(v,_) => evalValue(v).map(value => (value, Set[Addr]()))
    case _ => None
  }

  protected def addRead(action: Action[SchemeExp, Abs, Addr], read: Set[Addr]): Action[SchemeExp, Abs, Addr] = action match {
    case DispatchedActionReachedValue(v,x,y, σ, read2, write) => DispatchedActionReachedValue(v, x,y, σ, read ++ read2, write)
    case ActionReachedValue(v, σ, read2, write) => ActionReachedValue(v, σ, read ++ read2, write)
    case ActionReachedValueContext(v,  x,y,z,σ,read2, write) => ActionReachedValueContext(v,x,y,z,σ, read ++ read2, write)
    case ActionPushContext(e, obj, name, frame, ρ, σ, read2, write) => ActionPushContext(e, obj, name,frame, ρ, σ, read ++ read2, write)
    case ActionPush(e, frame, ρ, σ, read2, write) => ActionPush(e, frame, ρ, σ, read ++ read2, write)
    case ActionEval(e, ρ, σ, read2, write) => ActionEval(e, ρ, σ, read ++ read2, write)
    case ActionStepIn(fexp, clo, e, ρ, σ, argsv, read2, write) => ActionStepIn(fexp, clo, e, ρ, σ, argsv, read ++ read2, write)
    case ActionError(err) => action
  }

  override protected def funcallArgs(f: Abs, fexp: SchemeExp, args: List[(SchemeExp, Abs)], toeval: List[SchemeExp], ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time): Set[Action[SchemeExp, Abs, Addr]] = toeval match {
    case Nil => evalCall(f, fexp, args.reverse, ρ, σ, t)
    case e :: rest => atomicEval(e, ρ, σ) match {
      case Some((v, as)) => funcallArgs(f, fexp, (e, v) :: args, rest, ρ, σ, t).map(addRead(_, as))
      case None => Set(ActionPush(e, FrameFuncallOperands(f, fexp, e, args, rest, ρ), ρ, σ))
    }
  }

  /**
   * Optimize the following pattern: when we see an ActionPush(exp, frame, ρ, σ)
   * where exp is an atomic expression, we can atomically evaluate exp to get v,
   * and call stepKont(v, σ, frame).
   */
  protected def optimizeAtomic(actions: Set[Action[SchemeExp, Abs, Addr]], t: Time): Set[Action[SchemeExp, Abs, Addr]] = {
    //actions 
    //TODO: BUGFIX
    actions.flatMap({
      case ActionPush(exp, frame, ρ, σ, read, write) => atomicEval(exp, ρ, σ) match {
        case Some((v, read2)) => stepKont(v, frame, σ, t).map(addRead(_, read ++ read2))
        case None => Set[Action[SchemeExp, Abs, Addr]](ActionPush(exp, frame, ρ, σ, read, write))
      }
      case action => Set[Action[SchemeExp, Abs, Addr]](action)
    })
  }

  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time) =
    optimizeAtomic(super.stepEval(e, ρ, σ, t), t)

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, SchemeExp, Abs], t: Time) =
    optimizeAtomic(super.stepKont(v, frame, σ, t), t)
}

class ConcurrentSchemeSemantics[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
    extends SchemeSemantics[Abs, Addr, Time] {
  def thread = implicitly[ThreadIdentifier[TID]]

  case class FrameJoin(ρ: Environment[Addr]) extends SchemeFrame

  override def addRead(action: Action[SchemeExp, Abs, Addr], read: Set[Addr]) = action match {
    case ActionSpawn(t: TID, e, ρ, act, read2, write) => ActionSpawn(t, e, ρ, act, read ++ read2, write)
    case ActionJoin(tid, σ, read2, write) => ActionJoin(tid, σ, read ++ read2, write)
    case _ => super.addRead(action, read)
  }


  override def stepEval(e: SchemeExp, ρ: Environment[Addr], σ: Store[Addr, SchemeExp, Abs], t: Time) = e match {
    case SchemeSpawn(exp,_) =>
      val tid = thread.thread[SchemeExp, Time](exp, t)
      Set(ActionSpawn(tid, exp, ρ, ActionReachedValue(abs.injectTid(tid), σ)))
    case SchemeJoin(exp,_) => optimizeAtomic(Set(ActionPush(exp, FrameJoin(ρ), ρ, σ)), t)
    case _ => super.stepEval(e, ρ, σ, t)
  }

  override def stepKont(v: Abs, frame: Frame, σ: Store[Addr, SchemeExp, Abs], t: Time) = frame match {
    case FrameJoin(ρ) => Set(ActionJoin(v, σ))
    case _ => super.stepKont(v, frame, σ, t)
  }
}
