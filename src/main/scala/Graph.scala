case class Graph[Node, Annotation]
(ids: Map[Node, Int], next: Int, nodes: Set[Node], 
    edges: Map[Node, Set[(Annotation, Node)]]) 
{
  def this() = this(Map[Node, Int](), 0, Set[Node](), Map[Node, Set[(Annotation, Node)]]())
  def this(node: Node) = this(Map[Node, Int]() + (node -> 0), 1, Set[Node](node), Map[Node, Set[(Annotation, Node)]]())
  def addNode(node: Node): Graph[Node, Annotation] =
    if (nodes.contains(node)) { this } else {
      Graph(ids + (node -> next), next + 1, nodes + node, edges)
    }

  // n in, one out
  def removeNodeNO(node: Node, parents:Set[Node], succ:Node): Graph[Node, Annotation] =
    ids.get(node) match { 
      case None => this
      case Some(id) => {
        // edge from node -> succ, is only one, should be removed
        val pedges = edges.getOrElse(succ, Set());
        
        // for each parent
        val nwedges = parents.map( p =>
        {
            val pedges = edges.getOrElse(p, Set());
          
            // the parent might have multiple edges, get the right and single one
            val in = pedges.filter({ case(an, next) => { next == node }})   

            // in.head should point to succ, instead of node
            val nw = pedges.map({ case(an, next) => { if(next == node) {
              (an, succ)
            } else { (an, next) } }})  
            
            (p -> nw)
        })
        
        // multiple
        val nedges = edges.getOrElse(node, Set());
         
        // remove node, remove edge node -> succ, 
        Graph(ids - node, next, nodes - node, edges - node ++ nwedges.toMap)
      }
  }  
  
  // one in, n out
  def removeNodeON(node: Node, parent:Node, succ:Set[Node]): Graph[Node, Annotation] =
    ids.get(node) match { 
      case None => this
      case Some(id) => {
        val pedges = edges.getOrElse(parent, Set());
        val nedges = edges.getOrElse(node, Set());
        
        // the parent might have multiple edges, get the right and single one
        val in = pedges.filter({ case(an, next) => { next == node }})
        
        // add the new set edges of the parent, i.e. the newly added, and the old ones
        // minus the old edge frm parent -> node
        val set = succ.map(x => (in.head._1, x)) ++ 
          edges.getOrElse(parent, Set()).filter({ case(an, next) => { next != node }})
          
        Graph(ids - node, next, nodes - node, edges - node + (parent -> set))
      }
  }

  def replaceEdge(parent: Node, node: Node, succ:Node, annotLabel: (Annotation, Annotation) => Annotation): Graph[Node, Annotation] = {
    
      require(this.edges.getOrElse(node, Set()) != Set(), "test")
    
      val pedges = this.edges.getOrElse(parent, Set());
      val inEdge = pedges.filter({ case(an, next) => { next == node }})
      val outEdge = this.edges.getOrElse(node, Set()).filter({ case(an, next) => { next == succ }})
      
      val loops = this.edges.getOrElse(node, Set()).filter({ case(an, next) => { next == node }})
      
      if(loops.size > 0)
      {
        println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@loop detected")
        require(true, "loop")
      }
      
      //if(this.ids.getOrElse(parent, -1) == this.ids.getOrElse(parent, -1)
//      if(outEdge.size != 1)
//      {
//        for(o <- outEdge)
//        {
//          println(o)
//        }
//      }
      
      //require(inEdge.size > 1 && outEdge == 1 || outEdge.size > 1 && inEdge == 1, "lel ..")
      
      //require(inEdge.size == 1, "There should only be 1 in edge, size="+inEdge.size+",par="+parent+"; id="+this.ids.get(parent)+", node="+node+"; id="+this.ids.get(node)+", succ="+succ+"; id="+this.ids.get(succ)+", in="+inEdge+", out="+outEdge.head._1.toString())
      //require(outEdge.size == 1, "There should only be 1 out edge, size="+outEdge.size+",par="+parent+"; id="+this.ids.get(parent)+", node="+node+"; id="+this.ids.get(node)+", succ="+succ+"; id="+this.ids.get(succ)+", in="+inEdge.head._1+", out="+outEdge)
      
      //println("id="+this.ids.get(inEdge.head._2))
      
//      require(inEdge.head._1.toString().length() > 0, 
//          "inedge should have label, par="+parent+"; id="+this.ids.get(parent)+", node="+node+"; id="+this.ids.get(node)+", succ="+succ+"; id="+this.ids.get(succ)+", in="+inEdge.head._1+", out="+outEdge.head._1.toString()+", inedge="+inEdge+", outedge="+outEdge)
//      require(outEdge.head._1.toString().length() == 0, 
//          "outedge should NOT have label, par="+parent+"; id="+this.ids.get(parent)+", node="+node+"; id="+this.ids.get(node)+", succ="+succ+"; id="+this.ids.get(succ)+", in="+inEdge.head._1+", out="+outEdge.head._1.toString())

      // p -> succ
//      val inSet = this.edges.getOrElse(parent, Set()).map({ case(an, next) => { if(next == node) {
//        println(an +","+succ)
//        println("id="+this.ids.get(succ))
//        (an, succ)
//      } else { (an, next) } }})  

      // remove node -> succ
      //val outSet = this.edges.getOrElse(node, Set()).filter({ case(an, next) => { next != succ }})        
      // + (parent -> inSet)
          //(inEdge.head._1 + "" + outEdge.head._1, succ)
     
      val n = inEdge.flatMap({ case (ian, _) => {
            outEdge.map({ case (oan, _) => {
              (annotLabel(ian, oan), succ)
            }})
      }})
      
        //  val nw = (annotLabel(inEdge.head._1, outEdge.head._1), succ)
      Graph(ids, next, nodes, edges + (parent -> (pedges ++ n)))
  }  
  
  def removeNodeNN(node: Node, parents: Set[Node]): Graph[Node, Annotation] = {
//      val inEdge = parents.flatMap(
//          p => { 
//            this.edges.getOrElse(p, Set()).filter({ case(an, next) => { next == node }})
//          }
//      )
//      val outEdge = this.edges.getOrElse(node, Set())

      // for each parent
      val nwedges = parents.map( p =>
      {
          val pedges = edges.getOrElse(p, Set());
        
          // the parent might have multiple edges, get the right and single one
          val in = pedges.filter({ case(an, next) => { next != node }})   

//          // in.head should point to succ, instead of node
//          val nw = pedges.map({ case(an, next) => { if(next == node) {
//            (an, succ)
//          } else { (an, next) } }})  
          
          (p -> in)
      })      
      
      //require(inEdge.size == 0, "There should be no in edges when removing node")
      //require(outEdge.size == 0, "There should be no out edges when removing node")
      
      Graph(ids - node, next, nodes - node, edges - node ++ nwedges.toMap)
  }      
  
  //, inEdge:(Annotation, Node), outEdge:(Annotation, Node)
  def removeNodeOO(node: Node, parent:Node, succ:Set[Node]): Graph[Node, Annotation] =
    ids.get(node) match { 
      case None => this
      case Some(id) => {
        val pedges = edges.getOrElse(parent, Set());
        val nedges = edges.getOrElse(node, Set());
        val in = pedges.filter({ case(an, next) => { next == node }})
        
        //require(in.size > 0, "tle");
        // remove old edge parent -> nodeµ
        
//        if(in.head._1.toString().length() == 0) 
//        {          
//          return this;
//        }
        
        // all edges from node should have no annot, == nedges.size
        if(nedges.filter({ case(an, next) => { an.toString().length() > 0 }}).size > 0)
        {
          require(2 == 1, "did not met requirement when removing node, has annotation")
          println("FAILED="+in.head._1+", size="+nedges.size)
          println(node);
          println(nedges.filter({ case(an, next) => { println(an.toString()); an.toString().length() > 0 }}).size+" != "+nedges.size)
          return this;
        }
        
        require(succ.size == 1, "did not met requirement when removing node")
        
        //println(in.head._1+"succ="+succ.size)
        val set = succ.map(x => (in.head._1, x)) ++ edges.getOrElse(parent, Set()).filter({ case(an, next) => { next != node }})

        //println((ids - node).size+","+ids.size)
        Graph(ids - node, next, nodes - node, edges - node + (parent -> set))
        //g.addEdge(parent, annot, out.head._2)
      }
    }  
  
  def getConcat(node: Node, parent:Node): String =
    ids.get(node) match { 
      case None => "n/a"
      case Some(id) => {
        val in = edges.getOrElse(parent, Set())
        val out = edges.getOrElse(node, Set())
        
        in.head._1 + "" + out.head._1
      }
    } 
  
  def parent(node: Node): Set[Node] = {
    nodes.filter(ne => { edges.filter({ case (n, set) => set.filter(e => e._2 == node).size > 0 && n == ne}).size > 0 })
  }
  
  def remove(node: Node): Graph[Node, Annotation] = Graph(ids - node, next, nodes - node, edges - node)
  
  def replaceEdges(replace: Annotation => Annotation): Graph[Node, Annotation] = {
    val edgs = this.edges.map({ case (node, set) => {
      (node -> set.map({ case (an, n) => { (replace(an), n) }} ))
    }});
    
    Graph(ids, next, nodes, edgs)
  }
  
  def hasEdge(node1: Node, annot: Annotation, node2: Node):Boolean = {
    //TODO, what if there are more than 1 edges, not possible?
    this.edges.getOrElse(node1, Set()).find({
      case (annot2, nodeTo) => {
        //println(annot+","+annot2+"="+(annot == annot2)); 
        annot == annot2 && node2 == nodeTo 
       
       }}) match {
      case Some(_) => true
      case None => false
    }
  }
  
  def getAnnotations():Set[Annotation] = {
    this.edges.values.toSet.flatMap( (x:Set[(Annotation, Node)]) => x.map(y => y._1)) 
  }
  
  def addEdge(node1: Node, annot: Annotation, node2: Node): Graph[Node, Annotation] =
    addNode(node1).addNode(node2).addEdgeNoCheck(node1, annot, node2)
  def addEdges(l: Traversable[(Node, Annotation, Node)]): Graph[Node, Annotation] =
    l.foldLeft(this)({ case (g, (n1, annot, n2)) => g.addEdge(n1, annot, n2) })
  def addEdgeNoCheck(node1: Node, annot: Annotation, node2: Node): Graph[Node, Annotation] =
    if (edges.contains(node1) && edges(node1).contains((annot, node2))) { this } else {
      val existing: Set[(Annotation, Node)] = edges.getOrElse(node1, Set[(Annotation, Node)]())
      Graph(ids, next, nodes, edges + (node1 -> (existing ++ Set((annot, node2)))))
    }
  def size: Integer = nodes.size
  def transitions: Integer = edges.size
  def foldNodes[B](init: B)(f: (B, Node) => B) = nodes.foldLeft(init)(f)
  def getNode(id: Int): Option[Node] = ids.find({ case (_, v) => id == v }).map(_._1)
//  def getParent(node: Node): Any = edges.filter({ case (n, edgs) => {
//    edgs.filter({ case (ann, child) => child == node}).size > 0
//  }})
    
  
  def toDot(label: Node => String, color: Node => String, annotLabel: Annotation => String): String = {
      val sb = new StringBuilder("digraph G {\n")
      nodes.foreach((n) =>
        sb.append("node_" + ids(n) + "[label=\"" /* + ids(n).toString + " " */ + label(n).replaceAll("\"", "\\\\\"") + "\", fillcolor=\"" + color(n) + "\" style=\"filled\"];\n")
      )
      edges.foreach({ case (n1, ns) => ns.foreach({ case (annot, n2) => {
        
        //if(nodes.contains(n2) && nodes.contains(n1))
          sb.append("node_" 
              + ids(n1) 
              + " -> node_" 
              + ids(n2) 
              + " [label=\"" + annotLabel(annot).replaceAll("\"", "\\\\\"") 
              + "\"]")
        
      }})})
      sb.append("}")
      return sb.toString
    }
  def toDotFile(path: String, label: Node => String, color: Node => String, annotLabel: Annotation => String): Unit = {
    val f = new java.io.File(path)
    val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
    bw.write(toDot(label, color, annotLabel))
    bw.close()
  }
}
