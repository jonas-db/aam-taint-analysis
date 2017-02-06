/**
 * Abstract syntax of Scheme programs (probably far from complete)
 */
import scala.util.parsing.input.Position

trait SchemeExp {
  val pos: Position
}

/**
 * A lambda expression: (lambda (args...) body...)
 * Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
 */
case class SchemeLambda(args: List[String], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
}


/**
 * A function call: (f args...)
 */
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
}
/**
 * An if statement: (if cond cons alt)
 * If without alt clauses need to be encoded with an empty begin as alt clause
 */
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(if $cond $cons $alt)"
}
/**
 * Let-bindings: (let ((v1 e1) ...) body...)
 */
case class SchemeLet(bindings: List[(String, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
}
/**
 * Let*-bindings: (let* ((v1 e1) ...) body...)
 */
case class SchemeLetStar(bindings: List[(String, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
}
/**
 * Letrec-bindings: (letrec ((v1 e1) ...) body...)
 */
case class SchemeLetrec(bindings: List[(String, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
}
/**
 * A set! expression: (set! variable value)
 */
case class SchemeSet(variable: String, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(set! $variable $value)"
}
/**
 * A begin clause: (begin body...)
 */
case class SchemeBegin(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
}
/**
 * A cond expression: (cond (test1 body1...) ...)
 */
case class SchemeCond(clauses: List[(SchemeExp, List[SchemeExp])], pos: Position) extends SchemeExp {
  override def toString() = {
    val c = clauses.map({ case (cond, cons) => {
      val b = cons.mkString(" ")
      s"($cond $b)"
    }}).mkString(" ")
    s"(cond $c)"
  }
}

/**
 * A case expression: (case key ((vals1...) body1...) ... (else default...))
 */
case class SchemeCase(key: SchemeExp, clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val c = clauses.map({ case (datums, cons) => {
      val d = datums.mkString(" ")
      val b = cons.mkString(" ")
      s"(($d) $b)"
    }}).mkString(" ")
    if (default.isEmpty) {
      s"(case $key $c)"
    } else {
      s"(case $key $c (else ${default.mkString(" ")}))"
    }
  }
}


/**
 * An and expression: (and exps...)
 */
case class SchemeAnd(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
}
/**
 * An or expression: (or exps...)
 */
case class SchemeOr(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
}
/**
 * A variable definition: (define name value)
 */
case class SchemeDefineVariable(name: String, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(define $name $value)"
}
/**
 * A function definition: (define (name args...) body...)
 */
case class SchemeDefineFunction(name: String, args: List[String], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
}
/**
 * An identifier: name
 */
case class SchemeIdentifier(name: String, pos: Position) extends SchemeExp {
  override def toString() = name
}

/**
 * A quoted expression: '(foo (bar baz))
 *  The quoted expression is *not* converted to a Scheme expression, and remains
 * a simple s-expression, because that's exactly what it should be.
 */
case class SchemeQuoted(quoted: SExp, pos: Position) extends SchemeExp {
  override def toString() = s"'$quoted"
}

/**
 * A literal value (number, symbol, string, ...)
 */
case class SchemeValue(value: Value, pos: Position) extends SchemeExp {
  override def toString() = value.toString
}

/**
 * Compare-and-swap, concurrency synchronization primitive.
 */
case class SchemeCas(variable: String, eold: SchemeExp, enew: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(cas $variable $eold $enew)"
}

/**
 * Acquire a lock
 */
case class SchemeAcquire(exp: String, pos: Position) extends SchemeExp {
  override def toString() = s"(acquire $exp)"
}

/**
 * Release a lock
 */
case class SchemeRelease(exp: String, pos: Position) extends SchemeExp {
  override def toString() = s"(release $exp)"
}

/**
 * Spawn a new thread to compute an expression
 */
case class SchemeSpawn(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(spawn $exp)"
}

/**
 * Wait for a thread (whose identifier is the value of exp) to terminate
 */
case class SchemeJoin(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(join $exp)"
}

/**
 * Sink taint analysis
 */
case class SchemeSink(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(sink $exp)"
}

/**
 * Get object property
 */
case class SchemeGetProperty(property: SchemeExp, obj: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(get-property $obj $property)"
}

/**
 * Set object property
 */
case class SchemeSetProperty(property: SchemeExp, obj: SchemeExp, value:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(set-property $obj $property $value)"
}

/**
 * Delete object property
 */
case class SchemeDeleteProperty(property: SchemeExp, obj: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(delete-property $obj $property)"
}

/**
 * Define object data property
 */
case class SchemeDefineDataProperty(property: SchemeExp, obj: SchemeExp, value:SchemeExp, pos: Position) extends SchemeExp {
   override def toString() = s"(define-data-property $obj $property $value)"
}

/**
 * Define object accessor property
 */
case class SchemeDefineAccessorProperty(property: SchemeExp, obj: SchemeExp, getter:SchemeExp, setter:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(define-accessor-property $obj $property $getter $setter)"
}
/**
 * Set object prototype
 */
case class SchemeSetPrototype(obj: SchemeExp, proto:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(set-prototype $obj $proto)"
}

/**
 * Add event listener
 */

case class SchemeAddEventListener(obj: SchemeExp, property: SchemeExp, handler:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(add-event-listener $obj $property $handler)"
}

/**
 * Remove event listener
 */

case class SchemeRemoveEventListener(obj: SchemeExp, property: SchemeExp, handler:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(remove-event-listener $obj $property $handler)"
}

/**
 * Dispatch Event
 */

case class SchemeDispatchEvent(obj: SchemeExp, event:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(dispatch-event $obj $event)"
}


/**
 * Emit Event
 */

case class SchemeEmitEvent(obj: SchemeExp, event:SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(emit $obj $event)"
}

/**
 * Event loop
 */

case class SchemeEventLoop(pos: Position) extends SchemeExp {
   override def toString() = s"(event-loop)"
}

/**
 * Event queue
 */

case class SchemeEventQueue(pos: Position) extends SchemeExp {
   override def toString() = s"(event-queue)"
}


/**
 * Object that provides a method to compile an s-expression into a Scheme expression
 */
object SchemeCompiler {
  /**
    * Reserved keywords
    */
  val reserved: List[String] = List("lambda", "if", "let", "let*", "letrec", "cond", "case", "set!", "begin", "define", "cas", "acquire", "release")

  def compile(exp: SExp): SchemeExp = 
    exp match {
      case SExpPair(SExpIdentifier("quote"), rest) => compile(SExpQuoted(rest).setPos(exp.pos))
      case SExpPair(SExpIdentifier("lambda"),
        SExpPair(args, SExpPair(first, rest))) =>
        SchemeLambda(compileArgs(args), compile(first) :: compileBody(rest), exp.pos)
      case SExpPair(SExpIdentifier("lambda"), _) =>
        throw new Exception(s"Invalid Scheme lambda: $exp (${exp.pos})")
      case SExpPair(SExpIdentifier("if"),
        SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil()))))) =>
        SchemeIf(compile(cond), compile(cons), compile(alt), exp.pos)
      case SExpPair(SExpIdentifier("if"),
        SExpPair(cond, SExpPair(cons, SExpValue(ValueNil())))) =>
        /* Empty else branch is replaced by #f (R5RS states it's unspecified) */
        SchemeIf(compile(cond), compile(cons), SchemeValue(ValueBoolean(false), exp.pos), exp.pos)
      case SExpPair(SExpIdentifier("if"), _) =>
        throw new Exception(s"Invalid Scheme if: $exp (${exp.pos})")
      case SExpPair(SExpIdentifier("let"),
        SExpPair(bindings, SExpPair(first, rest))) =>
        SchemeLet(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
      case SExpPair(SExpIdentifier("let"), _) =>
        throw new Exception(s"Invalid Scheme let: $exp")
      case SExpPair(SExpIdentifier("let*"),
        SExpPair(bindings, SExpPair(first, rest))) =>
        SchemeLetStar(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
      case SExpPair(SExpIdentifier("let*"), _) =>
        throw new Exception(s"Invalid Scheme let*: $exp")
      case SExpPair(SExpIdentifier("letrec"),
        SExpPair(bindings, SExpPair(first, rest))) =>
        SchemeLetrec(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
      case SExpPair(SExpIdentifier("letrec"), _) =>
        throw new Exception(s"Invalid Scheme letrec: $exp")
      case SExpPair(SExpIdentifier("set!"),
        SExpPair(SExpIdentifier(variable), SExpPair(value, SExpValue(ValueNil())))) =>
      SchemeSet(variable, compile(value), exp.pos)
      case SExpPair(SExpIdentifier("set!"), _) =>
        throw new Exception(s"Invalid Scheme set!: $exp")
      case SExpPair(SExpIdentifier("begin"), body) =>
        SchemeBegin(compileBody(body), exp.pos)
      case SExpPair(SExpIdentifier("cond"), clauses) =>
        SchemeCond(compileCondClauses(clauses), exp.pos)
      case SExpPair(SExpIdentifier("case"), SExpPair(exp, clauses)) => {
        val (c, d) = compileCaseClauses(clauses)
        SchemeCase(compile(exp), c, d, exp.pos)
      }
      case SExpPair(SExpIdentifier("and"), args) =>
        SchemeAnd(compileBody(args), exp.pos)
      case SExpPair(SExpIdentifier("or"), args) =>
        SchemeOr(compileBody(args), exp.pos)
      case SExpPair(SExpIdentifier("define"),
        SExpPair(SExpIdentifier(name), SExpPair(value, SExpValue(ValueNil())))) =>
        SchemeDefineVariable(name, compile(value), exp.pos)
      case SExpPair(SExpIdentifier("define"),
        SExpPair(SExpPair(SExpIdentifier(name), args),
          SExpPair(first, rest))) =>
        SchemeDefineFunction(name, compileArgs(args), compile(first) :: compileBody(rest), exp.pos)
      case SExpPair(SExpIdentifier("cas"),
        SExpPair(SExpIdentifier(variable),
          SExpPair(eold, SExpPair(enew, SExpValue(ValueNil()))))) =>
        SchemeCas(variable, compile(eold), compile(enew), exp.pos)
      case SExpPair(SExpIdentifier("cas"), _) =>
        throw new Exception(s"Invalid Scheme cas: $exp")
      case SExpPair(SExpIdentifier("acquire"),
        SExpPair(SExpIdentifier(variable), SExpValue(ValueNil()))) =>
        SchemeAcquire(variable, exp.pos)
      case SExpPair(SExpIdentifier("acquire"), _) =>
        throw new Exception(s"Invalid Scheme acquire: $exp")
      case SExpPair(SExpIdentifier("release"),
        SExpPair(SExpIdentifier(variable), SExpValue(ValueNil()))) =>
        SchemeRelease(variable, exp.pos)
      case SExpPair(SExpIdentifier("release"), _) =>
        throw new Exception(s"Invalid Scheme release: $exp")
      case SExpPair(SExpIdentifier("spawn"),
        SExpPair(exp, SExpValue(ValueNil()))) =>
        SchemeSpawn(compile(exp), exp.pos)
      case SExpPair(SExpIdentifier("spawn"), _) =>
        throw new Exception(s"Invalid Scheme spawn: $exp")
      case SExpPair(SExpIdentifier("join"),
        SExpPair(exp, SExpValue(ValueNil()))) =>
        SchemeJoin(compile(exp), exp.pos)
      case SExpPair(SExpIdentifier("join"), _) =>
        throw new Exception(s"Invalid Scheme join: $exp")

//      case SExpPair(SExpIdentifier("sink"),
//        SExpPair(obj, SExpValue(ValueNil()))) =>
//        SchemeSink(compile(obj), exp.pos)        
        
      case SExpPair(SExpIdentifier("get-property"),
        SExpPair(obj, SExpPair(name, SExpValue(ValueNil())))) =>
        SchemeGetProperty(compile(name), compile(obj), exp.pos)
        
      case SExpPair(SExpIdentifier("set-property"),
        SExpPair(obj, SExpPair(name, SExpPair(value, SExpValue(ValueNil()))))) =>
        SchemeSetProperty(compile(name), compile(obj), compile(value), exp.pos)
        
      case SExpPair(SExpIdentifier("delete-property"),
        SExpPair(obj, SExpPair(name, SExpValue(ValueNil())))) =>
        SchemeDeleteProperty(compile(name), compile(obj), exp.pos)
        
      case SExpPair(SExpIdentifier("set-prototype"),
        SExpPair(obj, SExpPair(proto, SExpValue(ValueNil())))) =>
        SchemeSetPrototype(compile(obj), compile(proto), exp.pos)
        
      case SExpPair(SExpIdentifier("define-data-property"),
        SExpPair(obj, SExpPair(name, SExpPair(value, SExpValue(ValueNil()))))) =>
        SchemeDefineDataProperty(compile(name), compile(obj), compile(value), exp.pos)

      case SExpPair(SExpIdentifier("define-accessor-property"),
        SExpPair(obj, SExpPair(name, SExpPair(getter, SExpPair(setter, SExpValue(ValueNil())))))) =>
        SchemeDefineAccessorProperty(compile(name), compile(obj), compile(getter), compile(setter), exp.pos)

      case SExpPair(SExpIdentifier("add-event-listener"),
        SExpPair(obj, SExpPair(name, SExpPair(handler, SExpValue(ValueNil()))))) =>
        SchemeAddEventListener(compile(obj), compile(name), compile(handler), exp.pos)        

      case SExpPair(SExpIdentifier("remove-event-listener"),
        SExpPair(obj, SExpPair(name, SExpPair(handler, SExpValue(ValueNil()))))) =>
        SchemeRemoveEventListener(compile(obj), compile(name), compile(handler), exp.pos)         
        
      case SExpPair(SExpIdentifier("dispatch-event"),
        SExpPair(obj, SExpPair(event, SExpValue(ValueNil())))) =>
        SchemeDispatchEvent(compile(obj), compile(event), exp.pos)      

      case SExpPair(SExpIdentifier("emit"),
        SExpPair(obj, SExpPair(event, SExpValue(ValueNil())))) =>
        SchemeEmitEvent(compile(obj), compile(event), exp.pos)      
        
        
      case SExpPair(SExpIdentifier("event-loop"), SExpValue(ValueNil())) => SchemeEventLoop(exp.pos)
      case SExpPair(SExpIdentifier("event-queue"), SExpValue(ValueNil())) => SchemeEventQueue(exp.pos)
        
//      case SExpPair(SExpIdentifier("set-access-property"),
//        SExpPair(SExpValue(ValueString(name)), SExpPair(obj, SExpValue(ValueNil())))) =>
//        SchemeSetAccessProperty(name, compile(obj), getter, setter)        
        
      case SExpPair(f, args) =>
        SchemeFuncall(compile(f), compileBody(args), exp.pos)
      case SExpIdentifier(name) => if (reserved.contains(name)) {
        throw new Exception(s"Invalid Scheme identifier (reserved): $exp")
      } else {
        SchemeIdentifier(name, exp.pos)
      }
      case SExpValue(value) => SchemeValue(value, exp.pos)
      case SExpQuoted(quoted) => SchemeQuoted(quoted, exp.pos)
    }


  def compileArgs(args: SExp): List[String] = args match {
    case SExpPair(SExpIdentifier(id), rest) => id :: compileArgs(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme argument list: $args")
  }

  def compileBody(body: SExp): List[SchemeExp] = body match {
    case SExpPair(exp, rest) => compile(exp) :: compileBody(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme body: $body")
  }

  def compileBindings(bindings: SExp): List[(String, SchemeExp)] = bindings match {
    case SExpPair(SExpPair(SExpIdentifier(name),
      SExpPair(value, SExpValue(ValueNil()))), rest) =>
      (name, compile(value)) :: compileBindings(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme bindings: $bindings")
  }

  def compileCondClauses(clauses: SExp): List[(SchemeExp, List[SchemeExp])] = clauses match {
    case SExpPair(SExpPair(SExpIdentifier("else"), SExpPair(first, rest)),
                  SExpValue(ValueNil())) =>
      List((SchemeValue(ValueBoolean(true), clauses.pos), compile(first) :: compileBody(rest)))
    case SExpPair(SExpPair(cond, SExpPair(first, rest)), restClauses) =>
      (compile(cond), compile(first) :: compileBody(rest)) :: compileCondClauses(restClauses)
    case SExpPair(SExpPair(cond, SExpValue(ValueNil())), restClauses) =>
      (compile(cond), Nil) :: compileCondClauses(restClauses)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme cond clauses: $clauses")
  }

  def compileCaseClauses(clauses: SExp): (List[(List[SchemeValue], List[SchemeExp])], List[SchemeExp]) = clauses match {
    case SExpPair(SExpPair(SExpIdentifier("else"), SExpPair(first, rest)),
                  SExpValue(ValueNil())) =>
      (List(), compile(first) :: compileBody(rest))
    case SExpPair(SExpPair(objects, body), restClauses) =>
      val (compiled, default) = compileCaseClauses(restClauses)
      ((compileCaseObjects(objects), compileBody(body)) :: compiled, default)
    case SExpValue(ValueNil()) => (Nil, Nil)
    case _ => throw new Exception(s"Invalid Scheme case clauses: $clauses")
  }

  def compileCaseObjects(objects: SExp): List[SchemeValue] = objects match {
    case SExpPair(SExpValue(v), rest) =>
      SchemeValue(v, objects.pos) :: compileCaseObjects(rest)
    case SExpPair(SExpIdentifier(id), rest) =>
      /* identifiers in case expressions are treated as symbols */
      SchemeValue(ValueSymbol(id), objects.pos) :: compileCaseObjects(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme case objects: $objects")
  }
}

/**
 * Object that provides a method to rename variables in a Scheme program in
 * order to have only unique names. For example, (let ((x 1)) (let ((x 2)) x))
 * will be converted to (let ((_x0 1)) (let ((_x1 2)) _x1)). This is useful to
 * perform ANF conversion.
 */
object SchemeRenamer {
  /** Maps each variables to their alpha-renamed version (eg. x -> _x0) */
  type NameMap = Map[String, String]
  /** Map each variables to the number of times it is bound */
  type CountMap = Map[String, Integer]

  def rename(exp: SchemeExp): SchemeExp =
    rename(exp, Map[String, String](), Map[String, Integer]()) match {
      case (e, _) => e
    }

  def rename(exp: SchemeExp, names: NameMap, count: CountMap): (SchemeExp, CountMap) = exp match {
      case SchemeLambda(args, body, pos) =>
        countl(args, names, count) match {
          case (args1, names1, count1) => renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLambda(args1, body1, pos), count2)
          }
        }
      case SchemeFuncall(f, args, pos) =>
        rename(f, names, count) match {
          case (f1, count1) => renameList(args, names, count1) match {
            case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
          }
        }
      case SchemeIf(cond, cons, alt, pos) =>
        rename(cond, names, count) match {
          case (cond1, count1) => rename(cons, names, count1) match {
            case (cons1, count2) => rename(alt, names, count2) match {
              case (alt1, count3) => (SchemeIf(cond1, cons1, alt1, pos), count3)
            }
          }
      }
      case SchemeLet(bindings, body, pos) =>
        countl(bindings.map(_._1), names, count) match {
          /* Use old names for expressions of bindings */
          case (variables, names1, count1) => renameList(bindings.map(_._2), names, count1) match {
            case (exps, count2) => renameList(body, names1, count2) match {
            case (body1, count3) => (SchemeLet(variables.zip(exps), body1, pos), count3)
            }
          }
        }
      case SchemeLetStar(bindings, body, pos) =>
        renameLetStarBindings(bindings, names, count) match {
          case (bindings1, names1, count1) => renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLetStar(bindings1, body1, pos), count2)
          }
        }
      case SchemeLetrec(bindings, body, pos) =>
        countl(bindings.map(_._1), names, count) match {
        /* Use new names for expressions of bindings */
          case (variables, names1, count1) => renameList(bindings.map(_._2), names1, count1) match {
            case (exps, count2) => renameList(body, names1, count2) match {
            case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1, pos), count3)
            }
          }
        }
      case SchemeSet(variable, value, pos) =>
        rename(value, names, count) match {
          case (value1, count1) => (SchemeSet(names.get(variable) match {
            case Some(n) => n
          case None => variable
          }, value1, pos), count1)
        }
      case SchemeBegin(body, pos) =>
        renameList(body, names, count) match {
          case (body1, count1) => (SchemeBegin(body1, pos), count1)
        }
      case SchemeCond(clauses, pos) =>
        clauses.foldLeft((List[(SchemeExp, List[SchemeExp])](), count))(
          (st: (List[(SchemeExp, List[SchemeExp])], CountMap),
            cl: (SchemeExp, List[SchemeExp])) =>
          (st, cl) match {
            case ((l, cs), (e, body)) => rename(e, names, cs) match {
              case (e1, count1) => renameList(body, names, count1) match {
                case (body1, count2) =>
                  ((e1, body1) :: l, count2)
              }
            }
          }) match {
          case (l, count1) => (SchemeCond(l.reverse, pos), count1)
      }
      case SchemeCase(exp, clauses, default, pos) =>
        rename(exp, names, count) match {
          case (exp1, count1) => clauses.foldLeft((List[(List[SchemeValue], List[SchemeExp])](), count))(
            (st: (List[(List[SchemeValue], List[SchemeExp])], CountMap),
              cl: (List[SchemeValue], List[SchemeExp])) =>
            (st, cl) match {
              case ((l, cs), (objs, body)) => renameList(body, names, cs) match {
                case (body1, count1) => ((objs, body1) :: l, count1)
              }
            }) match {
            case (l, count1) => renameList(default, names, count1) match {
              case (default1, count2) => (SchemeCase(exp1, l.reverse, default1, pos), count2)
            }
          }
        }
      case SchemeAnd(exps, pos) =>
        renameList(exps, names, count) match {
          case (exps1, count1) => (SchemeAnd(exps1, pos), count1)
        }
      case SchemeOr(exps, pos) =>
      renameList(exps, names, count) match {
        case (exps1, count1) => (SchemeOr(exps1, pos), count1)
      }
      case SchemeDefineVariable(name, value, pos) =>
        /* Keeps name untouched (maybe not correct?) */
        rename(value, names, count) match {
          case (value1, count1) => (SchemeDefineVariable(name, value1, pos), count1)
        }
      case SchemeDefineFunction(name, args, body, pos) =>
      countl(args, names, count) match {
        case (args1, names1, count1) => renameList(body, names1, count1) match {
          case (body1, count2) =>
            (SchemeDefineFunction(name, args1, body1, pos), count2)
        }
      }
      case SchemeQuoted(quoted, pos) =>
        (SchemeQuoted(quoted, pos), count)
      case SchemeIdentifier(name, pos) => names.get(name) match {
      case Some(n) => (SchemeIdentifier(n, pos), count)
        case None => (SchemeIdentifier(name, pos), count) /* keep original name */
      }
      case SchemeValue(v, pos) =>
        (SchemeValue(v, pos), count)
      case _ => throw new Exception(s"Unhandled expression in renamer: $exp")
    }
   
  


  /** Renames a list of expressions executed sequentially (eg. within a begin) */
  def renameList(exps: List[SchemeExp], names: NameMap, count: CountMap): (List[SchemeExp], CountMap) = exps match {
    case exp :: rest =>
      val (exp1, count1) = rename(exp, names, count)
      val (rest1, count2) = renameList(rest, names, count1)
      (exp1 :: rest1, count2)
    case Nil => (Nil, count)
  }

  def renameLetStarBindings(bindings: List[(String, SchemeExp)], names: NameMap, count: CountMap): (List[(String, SchemeExp)], NameMap, CountMap) = bindings match {
    case (v, e) :: rest =>
      count1(v, names, count) match {
        /* use old names, as with a let* the variable is not yet bound in its
         * definition */
        case (v1, names1, count1) => rename(e, names, count1) match {
          case (e1, count2) => renameLetStarBindings(rest, names1, count2) match {
            case (rest1, names2, count3) =>
              ((v1, e1) :: rest1, names2, count3)
          }
        }
      }
    case Nil => (Nil, names, count)
  }

  /** To be called when a new variable is introduced in the scope. Adds it to the
    * name map and count map */
  def count1(variable: String, names: NameMap, count: CountMap): (String, NameMap, CountMap) = {
    val c: Int  = count.get(variable) match {
      case Some(x) => x + 1
      case None => 0
    }
    val n = s"_$variable$c"
    (n, names + (variable -> n), count + (variable -> c))
  }

  /** Same as count1 but for a list of variables */
  def countl(variables: List[String], names: NameMap, count: CountMap): (List[String], NameMap, CountMap) =
    variables.foldLeft((List[String](), names, count))(
      (st: (List[String], NameMap, CountMap), v: String) => st match {
      case (l, ns, cs) => count1(v, ns, cs) match {
        case (v1, ns1, cs1) => ((v1 :: l), ns1, cs1)
      }}) match {
      case (l, ns, cs) => (l.reverse, ns, cs)
    }
}

/**
 * Remove defines from a Scheme expression, replacing them by let bindings.
 * For example:
 *   (define foo 1)
 *   (define (f x) x)
 *   (f foo)
 * Will be converted to:
 *   (letrec ((foo 1)
 *            (f (lambda (x) x)))
 *     (f foo))
 * Which is semantically equivalent with respect to the end result
 */
object SchemeUndefiner {
  def undefine(exps: List[SchemeExp]): SchemeExp =
    undefine(exps, List())

  def undefine(exps: List[SchemeExp], defs: List[(String, SchemeExp)]): SchemeExp = exps match {
    case Nil => SchemeBegin(Nil, scala.util.parsing.input.NoPosition)
    case SchemeDefineFunction(name, args, body, pos) :: rest => undefine(SchemeDefineVariable(name, SchemeLambda(args, body, exps.head.pos), pos) :: rest, defs)
    case SchemeDefineVariable(name, value, _) :: rest => undefine(rest, (name, value) :: defs)
    case _ :: _ => if (defs.isEmpty) {
      undefineBody(exps) match {
        case Nil => SchemeBegin(Nil, scala.util.parsing.input.NoPosition)
        case exp :: Nil => exp
        case exps => SchemeBegin(exps, exps.head.pos)
      }
    } else {
      SchemeLetrec(defs.reverse, undefineBody(exps), exps.head.pos)
    }
  }

  def undefine1(exp: SchemeExp): SchemeExp = undefine(List(exp))

  def undefineBody(exps: List[SchemeExp]): List[SchemeExp] = exps match {
    case Nil => Nil
    case SchemeDefineFunction(_, _, _, _) :: _ => List(undefine(exps, List()))
    case SchemeDefineVariable(_, _, _) :: _ => List(undefine(exps, List()))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeLambda(args, body, pos) => SchemeLambda(args, undefineBody(body), pos)
        case SchemeFuncall(f, args, pos) => SchemeFuncall(undefine1(f), args.map(undefine1), pos)
        case SchemeIf(cond, cons, alt, pos) => SchemeIf(undefine1(cond), undefine1(cons), undefine1(alt), pos)
        case SchemeLet(bindings, body, pos) => SchemeLet(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body), pos)
        case SchemeLetStar(bindings, body, pos) => SchemeLetStar(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body), pos)
        case SchemeLetrec(bindings, body, pos) => SchemeLetrec(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body), pos)
        case SchemeSet(variable, value, pos) => SchemeSet(variable, undefine1(value), pos)
        case SchemeBegin(exps, pos) => SchemeBegin(undefineBody(exps), pos)
        case SchemeCond(clauses, pos) => SchemeCond(clauses.map({ case (cond, body) => (undefine1(cond), undefineBody(body)) }), pos)
        case SchemeCase(key, clauses, default, pos) => SchemeCase(undefine1(key), clauses.map({ case (vs, body) => (vs, undefineBody(body)) }), undefineBody(default), pos)
        case SchemeAnd(args, pos) => SchemeAnd(args.map(undefine1), pos)
        case SchemeOr(args, pos) => SchemeOr(args.map(undefine1), pos)
        case SchemeIdentifier(name, pos) => SchemeIdentifier(name, pos)
        case SchemeQuoted(quoted, pos) => SchemeQuoted(quoted, pos)
        case SchemeValue(value, pos) => SchemeValue(value, pos)
        case SchemeCas(variable, eold, enew, pos) => SchemeCas(variable, undefine1(eold), undefine1(enew), pos)
       // case SchemeCasVector(variable, index, eold, enew, pos) => SchemeCasVector(variable, undefine1(index), undefine1(eold), undefine1(enew), pos)
       // case SchemeAcquire(exp, pos) => SchemeAcquire(undefine1(exp), pos)
        //case SchemeRelease(exp, pos) => SchemeRelease(undefine1(exp), pos)
        case SchemeSpawn(exp, pos) => SchemeSpawn(undefine1(exp), pos)
        case SchemeJoin(exp, pos) => SchemeJoin(undefine1(exp), pos)
        /*
         * Object properties
         */
         case SchemeSink(name, pos) => SchemeSink(name, pos)
        case SchemeGetProperty(name, obj, pos) => SchemeGetProperty(name, obj, pos)
        case SchemeSetProperty(name, obj, value, pos) => SchemeSetProperty(name, obj, value, pos)
        case SchemeDeleteProperty(name, obj, pos) => SchemeDeleteProperty(name, obj, pos)
        case SchemeSetPrototype(name, obj, proto) => SchemeSetPrototype(name, obj, proto)
        case SchemeDefineDataProperty(name, obj, value, pos) => SchemeDefineDataProperty(name, obj, value, pos)
        case SchemeDefineAccessorProperty(name, obj, getter, setter, pos) => SchemeDefineAccessorProperty(name, obj, getter, setter, pos)
        /*
         *  Events
         */
        case SchemeAddEventListener(obj, name, handler, pos) => SchemeAddEventListener(obj, name, handler, pos)
        case SchemeRemoveEventListener(obj, name, handler, pos) => SchemeRemoveEventListener(obj, name, handler, pos)
        case SchemeDispatchEvent(obj, event, pos) => SchemeDispatchEvent(obj, event, pos) 
        case SchemeEmitEvent(obj, event, pos) => SchemeEmitEvent(obj, event, pos) 
        case SchemeEventLoop(pos) => SchemeEventLoop(pos) 
        case SchemeEventQueue(pos) => SchemeEventQueue(pos) 
      }
      exp2 :: undefineBody(rest)
    }
  }
}

object Scheme {
  /**
   * Compiles a s-expression into a scheme expression
   */
  def compile(exp: SExp): SchemeExp = SchemeCompiler.compile(exp)

  /**
   * Performs alpha-renaming to ensure that every variable has a unique name
   */
  def rename(exp: SchemeExp): SchemeExp = SchemeRenamer.rename(exp)

  /**
   * Replace defines in a program (a list of expressions) by a big letrec as a single expression
   */
  def undefine(exps: List[SchemeExp]): SchemeExp = SchemeUndefiner.undefine(exps)

  /**
   * Parse a string representing a Scheme program
   */
  def parse(s: String): SchemeExp = undefine(SExpParser.parse(s).map(compile _))
}
