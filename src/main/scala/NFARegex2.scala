import scalaz.Scalaz._
import scala.collection.mutable.ArrayBuffer
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import RegexLang._


 class NFARegex2[State](graph : Graph[Int, Char], initialState:Int, states : Array[Int], finalStates : List[Int])
{
 
  //Transitive closure method
  def compute2():Set[String] =
 {
   var size = states.size
   var A = Array.ofDim[Regex](size + 1, size + 1, size + 1)
   val annotations = graph.getAnnotations().toList;
   
//   println("size="+size, "annots="+annotations.size)
//   
//   for(a <- annotations)
//   {
//     println("=>"+a)
//   }
   
   if(annotations.contains(""))
   {
     println("should never happen")
     return Set();
   }

   for(i <- 1 to size)
   {
     for(j <- 1 to size)
     {
        if(i == j)
        {
          A(i)(j)(0) = EmptyWord()
        }
        else
        {
          A(i)(j)(0) = EmptySet()
        }
        
        for(a <- annotations)
        { 
           if(graph.hasEdge(states(i-1), a, states(j-1)))
           {           
             A(i)(j)(0) = simplifyr(Or(A(i)(j)(0), Event(a.toString())))
           }
        }       
     }
   }
   
   println("starting transitive closure")

   var start = System.nanoTime();
   for(k <- 1 to size)
   {   
     for(i <- 1 to size)
     {
       for(j <- 1 to size)
       {
         // R[i,j,k] := R[i,j,k-1] + R[i,k,k-1] . star(R[k,k,k-1]) . R(k,j,k-1)
//         A(i)(j)(k) = simplify(Or(simplify(A(i)(j)(k-1)), 
//                         simplify(Concat(simplify(A(i)(k)(k-1)), 
//                             simplify(Concat(simplify(Star(simplify(A(k)(k)(k-1)))), 
//                                 simplify(A(k)(j)(k-1)))))))) 
         //A(i)(j)(k) = simplifyr(Or(A(i)(j)(k-1), Concat(A(i)(k)(k-1), Concat(Star(A(k)(k)(k-1)), A(k)(j)(k-1))))) 
        A(i)(j)(k) = simplifyr(Or(A(i)(j)(k-1), Concat(A(i)(k)(k-1), Concat(Star(A(k)(k)(k-1)), A(k)(j)(k-1))))) 

       }
     }
     //println("Took "+((System.nanoTime - start) / Math.pow(10, 9))+"s to do iteration #"+k)
     //start = System.nanoTime();
     //System.gc()
   }

   //Main.printMemory()
   
   println("Took "+((System.nanoTime - start) / Math.pow(10, 9))+"s to do iterations")
   

   val initial = states.indexOf(initialState) + 1
 
   for(i <- 1 to size)
   {
     if(i != initial) A(i) = null
     
     if(!finalStates.contains(states(i-1)))
     {
       //A(initial)(i) = null
     }
   }
   
   //System.gc(); 
   //Main.printMemory();
   
   var regexes = Set[Regex]()

   for(i <- 1 to size)
   {     
     if(finalStates.contains(states(i-1)))
     {
        println("writing final state "+(i-1))
        
//        val f = new java.io.File("./regex/res"+(i-1)+".txt")
//        val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
//        bw.write(A(initial)(i)(size).toString())
//        bw.close()
//        
//      val fout = new FileOutputStream("./regex/res_"+(i-1)+".dat");
//      val oos = new ObjectOutputStream(fout);
//      oos.writeObject(A(initial)(i)(size));
//      oos.close();
        
//        println("written")
//        println(A(initial)(i)(size).toString());
        start = System.nanoTime();
        val r = sum(A(initial)(i)(size))
        println("Took "+((System.nanoTime - start) / Math.pow(10, 9))+"s")
        
        println("size="+r.size)
        regexes = regexes ++ r
        A(initial)(i) = null
        //System.gc();
     }       
   }
   
   regexes.map { x => x.toString() }
   
//   for(r <- regexes)
//   {
//     println(r)
//   }

//   val f = new java.io.File("./regex/final3.txt")
//   val bw = new java.io.BufferedWriter(new java.io.FileWriter(f))
//   var concrete = 0;
//   
//   for(r <- regexes)
//   {
//     val stars = r.toString().length() - r.toString().replace("*", "").length();
//     r match {
//       case Event(e) => {
//         concrete = concrete + 1;
//         println("stars="+stars+", Concrete seq="+e)
//       }
//       case Or(Event(a), Event(b)) => {
//         concrete = concrete + 2;
//         println("stars="+stars+", Concrete seq="+a) 
//         println("stars="+stars+", Concrete seq="+b)
//       }
//       case _ => {}//println("stars="+stars+", seq="+r)
//     }
//     
//     bw.write(r.toString())
//     bw.newLine()
//   }
//   
//   bw.write("concrete="+concrete)
//   bw.close()
 }
  
 def listRegexes(regex : Regex):Set[Regex] = regex match {
   case Or(x, y) => listRegexes(x) ++ listRegexes(y)
   //And(x, y)
   case x => Set(x)
 }
 
 
 def sum(regex : Regex) :  Set[Regex] = {
  def sumAcc(trees : List[Regex], acc : Set[Regex]) :  Set[Regex] = trees match {
    case Nil => acc
    case Or(x, y) :: rs => sumAcc(x :: y :: rs, acc)
    case x :: rs => sumAcc(rs, Set(x) ++ acc)

  }
  sumAcc(List(regex), Set())
}

 
}