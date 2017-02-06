/**
 * Implementation of a CESK machine following the AAM approach (Van Horn, David,
 * and Matthew Might. "Abstracting abstract machines." ACM Sigplan
 * Notices. Vol. 45. No. 9. ACM, 2010).
 *
 * A difference with the paper is that we separate the continuation store
 * (KontStore) from the value store (Store). That simplifies the implementation
 * of both stores, and the only change it induces is that we are not able to
 * support first-class continuation as easily (we don't support them at all, but
 * they could be added).
 *
 * Also, in the paper, a CESK state is made of 4 components: Control,
 * Environment, Store, and Kontinuation. Here, we include the environment in the
 * control component, and we distinguish "eval" states from "continuation"
 * states. An eval state has an attached environment, as an expression needs to
 * be evaluated within this environment, whereas a continuation state only
 * contains the value reached.
 */
import dk.brics.automaton._
//import scala.collection.convert.wrapAll._
import scala.collection.JavaConversions._

class AAM[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends EvalKontMachine[Exp, Abs, Addr, Time] {
  def name = "AAM"

  /**
   * The store used for continuations is a KontStore (defined in
   * Kontinuation.scala). It is parameterized by continuation addresses, that
   * are element of the KontAddress typeclass.
   */
  trait KontAddr
  case class NormalKontAddress(exp: Exp, addr: Addr) extends KontAddr {
    override def toString = s"NormalKontAddress($exp)"
  }
  object HaltKontAddress extends KontAddr {
    override def toString = "HaltKontAddress"
  }

  object KontAddr {
    implicit object KontAddrKontAddress extends KontAddress[KontAddr]
  }

  /** The primitives are defined in AbstractValue.scala and are available through the Primitives class */
  val primitives = new Primitives[Addr, Exp, Abs]()
    // 1: call site sensitive, 2: event sensitive, 3: listener sensitive
  var sensitivity = 2;

  /**
   * A machine state is made of a control component, a value store, a
   * continuation store, and an address representing where the current
   * continuation lives.
   */
  case class State(control: Control, σ: Store[Addr, Exp, Abs], kstore: KontStore[KontAddr], a: KontAddr, t: Time) {
    /**
     * Builds the state with the initial environment and stores
     */
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
      Store.initial[Addr, Exp, Abs](primitives.forStore),
      new KontStore[KontAddr](), HaltKontAddress, time.initial)
    override def toString() = control.toString(σ)
    /**
     * Checks whether a states subsumes another, i.e., if it is "bigger". This
     * is used to perform subsumption checking when exploring the state space,
     * in order to avoid exploring states for which another state that subsumes
     * them has already been explored.
     */
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && a == that.a && kstore.subsumes(that.kstore) && t == that.t

    /**
     * Integrates a set of actions (returned by the semantics, see
     * Semantics.scala), in order to generate a set of states that succeeds this
     * one.
     */
    private def integrate(a: KontAddr, actions: Set[Action[Exp, Abs, Addr]]): Set[State] =
    {
      actions.flatMap({
        /* When a value is reached, we go to a continuation state */
        case ActionReachedValue(v, σ, _, _) => Set(State(ControlKont(v), σ, kstore, a, t))
        
        /* When a event listener is added */
        case ActionReachedValueContext(v, σ, obj, name, handler, _, _) => 
          Set(State(ControlKont(v), σ, kstore, a, if(sensitivity == 3) time.tick(t, (obj, name, handler)) else t))

        /* When a value is reached, we go to a continuation state */
        case DispatchedActionReachedValue(v, obj, name, σ, _, _) => {
          //println("DispatchedActionReachedValue")
          //println(t)
          Set(State(ControlKont(v), σ, kstore, a, if(sensitivity == 2) time.tick(t, (obj, name)) else t)) 
          
        }        
        
        /* When a continuation needs to be pushed, push it in the continuation store 
         * used for event sensitivity in the event loop
         * */
        case ActionPushContext(e, obj, name, frame, ρ, σ, _, _) => {
          val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
          //println("ActionPushContext")
          Set(State(ControlEval(e, ρ), σ, kstore.extend(next, Kont(frame, a)), next, 
              if(sensitivity == 2) time.tick(t, (obj, name)) else t))
        }     
 
        /* When a continuation needs to be pushed, push it in the continuation store */
        case ActionPush(e, frame, ρ, σ, _, _) => {
          val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
          Set(State(ControlEval(e, ρ), σ, kstore.extend(next, Kont(frame, a)), next, t))
        }        

        //only used to get the event on the edges
        case ActionPushEvent(e, frame, event, ρ, σ, _, _) => {
          val next = NormalKontAddress(e, addr.variable("__kont__", t)) // Hack to get infinite number of addresses in concrete mode
          Set(State(ControlEvalEvent(e, ρ, event), σ, kstore.extend(next, Kont(frame, a)), next, t))
        }             
        
        /* When a value needs to be evaluated, we go to an eval state */
        case ActionEval(e, ρ, σ, _, _) => Set(State(ControlEval(e, ρ), σ, kstore, a, t))
        
        /* When a function is stepped in, we also go to an eval state */
        case ActionStepIn(fexp, _, e, ρ, σ, _, _, _) => {
          //println("stepin="+fexp)
          Set(State(ControlEval(e, ρ), σ, kstore, a, if(sensitivity == 1) time.tick(t, fexp) else t))
        }
        /* When a function is stepped in, we also go to an eval state */
        case ActionStepInEvent(fexp, _, e, ρ, σ, _, _, _) => {
          Set(State(ControlEval(e, ρ), σ, kstore, a, if(sensitivity == 2) time.tick(t, fexp) else t))
        }        
        
        /* When an error is reached, we go to an error state */
        case ActionError(err) => Set(State(ControlError(err), σ, kstore, a, t))
      })
    }
    
    /**
     * Computes the set of states that follow the current state
     */
    def step(sem: Semantics[Exp, Abs, Addr, Time]): Set[State] = control match {
      /* In a eval state, call the semantic's evaluation method */
      case ControlEval(e, ρ) => integrate(a, sem.stepEval(e, ρ, σ, t))
      /* In a continuation state, if the value reached is not an error, call the
       * semantic's continuation method */
      case ControlKont(v) if abs.isError(v) => Set()
      case ControlKont(v) => kstore.lookup(a).flatMap({
        case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, σ, t))
      })
      /* In an error state, the state is not able to make a step */
      case ControlError(_) => Set()
    }
    /**
     * Checks if the current state is a final state. It is the case if it
     * reached the end of the computation, or an error
     */
    def halted: Boolean = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => {
        //println("halte="+a)
        a == HaltKontAddress || abs.isError(v)
      }
      case ControlError(_) => true
    }
  }

  case class AAMOutput(halted: Set[State], count: Int, t: Double, graph: Option[Graph[State, String]], eventNodes: Set[State])
      extends Output[Abs] {

    /**
     * Returns the list of final values that can be reached
     */
    def finalValues = {
      println("size=+" +halted.size)
      halted.flatMap(st => st.control match {
    
      case ControlKont(v) => Set[Abs](v)
      case x => { println("err="+x); Set[Abs]()}
    })}

    /**
     * Checks if a halted state contains a value that subsumes @param v
     */
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))

    /**
     * Returns the number of visited states
     */
    def numberOfStates = graph match {
      case Some(g) => {
        println(g.nodes.size+ "=="+count)
        g.nodes.size
      }      
      case None => -1
    }
 
    def distinctFinalStates = 
              graph match {
      case Some(g) =>      g.nodes.filter { 
          node => node.control.toString().contains("IS_TAINTED") || node.control.toString().contains("MAYBE_TAINTED") }.map { x => x.toString() }

      case None => Set()
            }
    
    def shortestPaths = graph match {
      case Some(g) => {
        var finalGraph = g
        var annots = g.getAnnotations(); 
        val root = g.ids.filter(x => x match {
          case (node, 0) => true 
          case _ => false 
        }).head._1 // node
        
        var distinctFinalStates = g.nodes.filter { 
          node => node.control.toString().contains("IS_TAINTED") || node.control.toString().contains("MAYBE_TAINTED") }

//        println("distinctFinalStates="+distinctFinalStates.size)
//        println(distinctFinalStates)
        
        var shortestpaths = List[(String, Set[String])]()
        
        var startingTime = System.nanoTime
        
        var idx = 0;
        if(annots.size > 0) println(annots)
        if(annots.size > 0)
        for(dis <- distinctFinalStates) {
          var automaton = new Automaton()        
          var initial = new dk.brics.automaton.State()
          
         
          var startx = System.nanoTime();
          val mapping = annots.foldLeft(Map[String, Char]())(
              { case (map, annot) => if(annot.length()>0) map + (annot -> annot.charAt(0)) else map })
              
          val list = convert(Set((root, initial)), Set(), g, Set[StatePair](), mapping, Set(dis))
          val lt = new java.util.ArrayList[StatePair]()
          list.foreach { x => lt.add(x) }
          
          automaton.setInitialState(initial)
          println("Took "+((System.nanoTime - startx) / Math.pow(10, 9))+"s TIMEEEEEEEEEEEEEEEEE")
          startx = System.nanoTime();          
          
          automaton.addEpsilons(lt)
          val diff= ((System.nanoTime - startx) / Math.pow(10, 9))
          println("Took "+((System.nanoTime - startx) / Math.pow(10, 9))+"s TIMEEEEEEEEEEEEEEEEE")
          
         // startingTime = System.nanoTime
          Automaton.setMinimization(1) // brzozowski
          automaton.minimize()
         val shortestp = BasicOperations.getShortestExample(automaton, true)
         
         //dot out.dot -Tpng -o out.png
          
          println("shortest ="+BasicOperations.getShortestExample(automaton, true))        
          //println("lt size after minimizing="+lt.size)        
          //println("filter="+g.edges.values.flatten.filter( { case (annot, node) => annot.length() == 0 }).size)
          //println(automaton.toString()) 
          
          var grap = new Graph[Int, Char]
          automaton.getStates().map { st => {
            grap = grap.addNode(st.number)
            var lt = Set[Transition]()
            st.getTransitions().foreach { x => lt = lt + new Transition(x.getMin, x.getDest) }    
            assert(lt.size == st.getTransitions().size)
            lt.map({ s => grap = grap.addEdge(st.number, s.getMin(), s.getDest().number) })
            }
          }
          
          val states = grap.nodes.toArray;        
          val initialState = automaton.getInitialState().number
   
          var finals = Set[Int]()
          automaton.getAcceptStates().foreach { x => finals = finals + x.number }
  
//          println("initial="+initialState)
//          println("final="+finals)        
          
          var regex = new NFARegex2[Int](grap, initialState, states, finals.toList)
          var regexes = regex.compute2()
         
          val t = (shortestp, regexes)
          shortestpaths = shortestpaths :+ t

          idx = idx +1
        }
        
        ((System.nanoTime - startingTime) / Math.pow(10, 9), shortestpaths)
      }
      case None => (0, List())
    }

    def numberOfEdges = graph match {
      case Some(g) => g.transitions
      case None => -1
    }
    def numberOfMays = halted.filter { x => x.control match {
      case ControlError(v) => v.contains("MAYBE_TAINTED")
      case y => false // ko(Prod(String,Tainted)), but we only want the ones flowing to a sink
      }
    }.size
    def numberOfMusts = halted.filter { x => x.control match {
      case ControlError(v) => v.contains("IS_TAINTED")
      case y => false
      }
    }.size

    /**
     * Returns the time taken to evaluate the expression
     */
    def time = t

    /**
     * Outputs the graph in a dot file
     */
    def toDotFile(path: String) = graph match {
      case Some(g) => {
             
        var finalGraph = g
        var annots = g.getAnnotations(); 
        val root = g.ids.filter(x => x match {
          case (node, 0) => true 
          case _ => false 
        }).head._1
        
        var distinctFinalStates = g.nodes.filter { 
          node => node.control.toString().contains("IS_TAINTED") || node.control.toString().contains("MAYBE_TAINTED") }

        println("distinctFinalStates="+distinctFinalStates.size)
        println(distinctFinalStates)
        
        var shortestpaths = Set[String]()
        
        var idx = 0;
        if(annots.size > 0) println(annots)
        //if(annots.size > 0)
        //for(dis <- distinctFinalStates) {
          var automaton = new Automaton()
          var initial = new dk.brics.automaton.State()

          val mapping = annots.foldLeft(Map[String, Char]())(
              { case (map, annot) => if(annot.length()>0) map + (annot -> annot.charAt(0)) else map })

          val list = convert(Set((root, initial)), Set(), g, Set[StatePair](), mapping, distinctFinalStates)
          val lt = new java.util.ArrayList[StatePair]()
          list.foreach { x => lt.add(x) }
          println(s"states in automaton=${lt.size}, state="+automaton.getStates())
          automaton.setInitialState(initial)
          automaton.addEpsilons(lt)
          Automaton.setMinimization(1) // brzozowski
          automaton.minimize()
          shortestpaths = shortestpaths ++ Set(BasicOperations.getShortestExample(automaton, true))

          println("shortest ="+BasicOperations.getShortestExample(automaton, true))
          //println("lt size after minimizing="+lt.size)
          //println("filter="+g.edges.values.flatten.filter( { case (annot, node) => annot.length() == 0 }).size)
          //println(automaton.toString())

          // convert back to .dot graph
          var grap = new Graph[Int, Char]
          automaton.getStates().map { st => {
            grap = grap.addNode(st.number)
            var lt = Set[Transition]()
            st.getTransitions().foreach { x => lt = lt + new Transition(x.getMin, x.getDest) }
            assert(lt.size == st.getTransitions().size)
            lt.map({ s => grap = grap.addEdge(st.number, s.getMin(), s.getDest().number) })
            }
          }

          val states = grap.nodes.toArray;
          val initialState = automaton.getInitialState().number

          var finals = Set[Int]()
          automaton.getAcceptStates().foreach { x => finals = finals + x.number }

          println("initial="+initialState)
          println("final="+finals)

          var regex = new NFARegex2[Int](grap, initialState, states, finals.toList)
          var strings:Set[String] = regex.compute2()

          for(s <- strings)
          {
            println(s)
          }

          grap.toDotFile("out/"+path+"_"+idx+".dot", _.toString.take(40),
          (s) => if(s == initialState) "#DDFFDD" else if(finals.contains(s)) "#FFDDDD" else "#FFFFDD"
          , x => x.toString())

          idx = idx +1
       // }

//    val f = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File("bm/"+path+"_"+sens+".txt"), true))
//    val sens = sensitivity match {
//      case 1 => "KCFA"
//      case 2 => "EVT"
//      case 3 => "LST"
//    }

//    val set = shortestpaths
//    f.write(amount+""+sens+" => "+res2 + ", results="+set+"\n")
//    f.close()  
//        
        
        
          g.toDotFile(path, x => if (x.toString.contains("err")) x.toString else x.toString.take(40),
          (s) => if (halted.contains(s)) { "#FFDDDD" } else { s.control match { //redis
            case ControlEval(_, _) => "#DDFFDD" /// groen
            case ControlKont(_) => "#FFFFDD" //yellow
            case ControlError(_) => "#FF0000" // full red
            //case _ => "#FF00FF"
          }}, x => x)        
        
      }
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  //@scala.annotation.tailrec
  final def convert(todo: Set[(State, dk.brics.automaton.State)],
      visited: Set[(State,dk.brics.automaton.State)], graph:Graph[State, String], epsilons:Set[StatePair],mapping:Map[String,Char], finalState:Set[State]): Set[StatePair]
  = {
    todo.headOption match {
        // S = state
      case Some((s, ast)) => {
        if(!visited.contains((s, ast)))
        {
          var newEps = Set[StatePair]()
          var newStates = Set[(State,dk.brics.automaton.State)]()

          // Set[(Annotation, State)] ,node == state
          graph.edges.getOrElse(s, Set()).map({ case (trans, node) => { 

              val exists = visited.find({ case(x, y) => x == node })
              // new state for node
              val newState = if(exists.size == 0) new dk.brics.automaton.State() else exists.head._2
              
              if(node.control.toString().contains("IS_TAINTED") 
                  || node.control.toString().contains("MAYBE_TAINTED"))
              {
                val id = graph.ids.getOrElse(node, -1)
                if(finalState.contains(node)){
                  println("accepting")
                newState.setAccept(true)}
              }
                          
              if(trans.length() == 0)
              {
                // ast = bricbk state
                newEps = newEps + new StatePair(ast, newState)        
              }
              else
              {
                ast.addTransition(new Transition(mapping.getOrElse(trans, 'g'), newState))
              }

            // node = our state
              newStates = newStates + ((node, newState))

          }})
          
          convert(todo.tail ++ newStates, visited ++ Set((s,ast)), graph, epsilons ++ newEps, mapping, finalState)
        }
        else
        {
          convert(todo.tail, visited, graph, epsilons, mapping, finalState)
        }
      }
      case None => epsilons
    }
  }

  /**
   * Explores the state graph generated by State's step function.
   * @param todo is the set of states that needs to be visited
   * @param visited is the set of states already visited, they won't be visited again
   * @param halted is the set of final states reached
   * @param graph is the graph in its current form
   * @return the final states as well as the computed graph
   */
  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, graph: Option[Graph[State, String]],
    sem: Semantics[Exp, Abs, Addr, Time], eventNodes: Set[State]): AAMOutput =
    todo.headOption match {
      case Some(s) =>
        if(false)//visited.size == 400)//
        {
          AAMOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph, Set())
        
        }
        else{
        
        //TODO: disabled subsumption
        if (visited.contains(s)/**/) {
          /* If we already visited the state, or if it is subsumed by another already
           * visited state, we ignore it. The subsumption part reduces the
           * number of visited states but leads to non-determinism due to the
           * non-determinism of Scala's headOption (it seems so at least). */
          loop(todo.tail, visited, halted, startingTime, graph, sem, eventNodes)
        } 
        else if(visited.exists(s2 => s2.subsumes(s)))
        {
          // succs are the states that subsume s
          val succs = visited.filter(s2 => s2.subsumes(s))
          
          val newGraph = graph match {
            case Some(g) => {
              var graph = g;
              
              for(sc <- succs)
              {
                graph = graph.edges.get(sc) match {
                  case Some(es) => {
                    //println("edges"+es.size)
                    graph.addEdges(es.map( { case (e, y) => (s, e, y) }))
                  }
                  case None => {
                    //println("no edges")
                    graph
                  }
                }                
              }
              
              Some(graph)
            }
            case None => graph
          }
          
          //val newGraph = graph.map(_.addEdges(succs.map(s2 => (s, (), s2))))
          loop(todo.tail, visited , halted, startingTime, newGraph, sem, eventNodes)    
          
          //loop(todo.tail, visited, halted, startingTime, graph, sem)
        } 
        else if (s.halted) {
          /* If the state is a final state, add it to the list of final states and
           * continue exploring the graph */
          //println("halted="+s)
          loop(todo.tail, visited + s, halted + s, startingTime, graph, sem, eventNodes)
        } else {
          /* Otherwise, compute the successors of this state, update the graph, and push
           * the new successors on the todo list */
         
          var succs = s.step(sem)
          
          val eventNode = succs.filter({ st => st.control match {
            case ControlEvalEvent(_,_,_) => true
            case _ => false
          }}).size > 0
          
          if(eventNode)
          {
           //println("Eventnode detected=>"+s)
          }
          // println("stepping="+s+" ->"+succs)
          // println("store="+s.σ)
           
          // if we have an controlevalevent then we executed an event handler
          // s is the eventloop node or a repetition of it
          val newGraph = graph.map(_.addEdges(succs.map(s2 => {
            s2.control match {
              case ControlEvalEvent(_,_,event) => {
                 val nw = s2 match {
                  case State(ControlEvalEvent(e,ρ, _), σ, kstore, a, t) => {
                    State(ControlEval(e, ρ), σ, kstore, a, t)
                  }
                }
                (s, event, nw)
              }
              case _ => (s, "", s2)
            }            
          })))
          
           succs = succs.map(st => {
             st.control match {
              case ControlEvalEvent(e, ρ, event) => st match {
                case State(_, σ, kstore, a, t) => {
                  State(ControlEval(e, ρ), σ, kstore, a, t)
                }
              }
              case _ => st
            }            
          })
          
          
          loop(todo.tail ++ succs, visited + s, halted, startingTime, newGraph, sem, if(eventNode) eventNodes + s else eventNodes)
        }}
        
      case None => {
//        graph match {
//          case Some(g) => println(s"transitions ${g.transitions}")
//          case None => println("no graph")
//        }

        AAMOutput(halted, visited.size,(System.nanoTime - startingTime) / Math.pow(10, 9), graph, eventNodes)
      }
        
    }

  /**
   * Performs the evaluation of an expression, possibly writing the output graph
   * in a file, and returns the set of final states reached
   */
  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr, Time], graph: Boolean): Output[Abs] =
    loop(Set(new State(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some(new Graph[State, String]()) } else { None },
      sem, Set())
}
