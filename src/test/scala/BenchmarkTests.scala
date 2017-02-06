import org.scalatest._
import org.scalatest.prop._
import Timestamps._


//    


abstract class Benchmarks2[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends FlatSpec with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val sem: Semantics[Exp, Abs, Addr, Time]
  val machine: AbstractMachine[Exp, Abs, Addr, Time]
var sensitivity = 1;
  var amount= 1;
  def checkResult(file: String, expected: List[Abs]): Unit = {
    var result = machine.eval(sem.parse(Main.fileContent(s"test/evaluation/$file")), sem, true)
    println(result.finalValues)

    println("writing")
    val set = result.distinctFinalStates;
    val (t, sp) = result.shortestPaths
    
    val res = s"Visited ${result.numberOfStates} states in ${result.time} seconds, ${result.finalValues.size} possible results: ${result.finalValues}"
    val res2 = s"states=${result.numberOfStates} edges=${result.numberOfEdges} mays=${result.numberOfMays} musts=${result.numberOfMusts} time=${result.time} regextime=${t} results=${result.finalValues.size}"
    //f.write(res + "\n")
    val sens = sensitivity match {
      case 1 => "KCFA"
      case 2 => "EVT"
      case 3 => "LST"
    }

    var shot:Set[String] = Set()      
    for((a, b) <- sp)
    {
      shot = shot ++ b
      
    }
    val f = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File("bm/"+amount+""+sens+""+file+".txt"), true))
    
    val af1 = BigDecimal(result.time).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    val af2 = BigDecimal(t).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
    val reshh = if(shot.mkString(",").length() == 0) "/" else shot.mkString("|")
    
    f.write(s"${af1}	${af2}")
    
    //f.write(amount+""+sens+s" => ${result.numberOfMays} / ${result.numberOfMusts}  &	${result.numberOfStates} / ${result.numberOfEdges}  & 	${af1} / ${af2}  &	${reshh}")
    //f.write(amount+""+sens+" => "+res2 + ", errors="+set+"\n")
        f.write("\n")
//    for((a, b) <- sp)
//    {
//      f.write("shortest="+a+", regexes="+b+"\n")
//    }
    
    f.close()      
    
    assert(1 == 1)
    //println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")    
  }

  def checkn(file: String, a:Int, expected: List[Abs]): Unit =
    file should s"eval to $a $expected" in { checkResult(file, expected) }
  
  
  //TODO for loop 30 times?
  // events
  //LOC emits listeners
  for( a <- 1 to 30){
  //checkn("objlis.scm", a, List(abs.inject(false))) // 21 2 2
  checkn("eventcallfunction.scm", a, List(abs.inject(false))) // 9 2 2
  checkn("eventsamehandler.scm", a, List(abs.inject(false))) // 22 2 2
  checkn("listenersens.scm", a,List(abs.inject(false))) // 14 2 2
  checkn("objjoin.scm", a,List(abs.inject(false))) // 28 4 4
  //
  checkn("objneg.scm", a,List(abs.inject(false))) // 11 2 2
  checkn("objrem.scm", a,List(abs.inject(false)))  // 26 4 4
  checkn("objsamelis.scm", a,List(abs.inject(false)))  // 16 2 3
  }
  
  
  //  //STUPID checkn("multiplelisteners.scm", List(abs.inject(false)))
//  
//  // only objs
//  checkn("obj.scm", List(abs.inject(false)))
//  checkn("obj2.scm", List(abs.inject(false)))
//  checkn("protojoin.scm", List(abs.inject(false)))    
//  checkn("evs10.scm", List(abs.inject(false)))



}


object TypeSetTaintLattice extends ProductLattice[AbstractTypeSet,AbstractMaybeTaint]
class AAMKCFA0 extends BenchmarkKCFA0[TypeSetTaintLattice.Product, ClassicalAddress, ZeroCFA]
class AAMKCFA1 extends BenchmarkKCFA1[TypeSetTaintLattice.Product, ClassicalAddress, OneCFA]
class AAMKCFA2 extends BenchmarkKCFA2[TypeSetTaintLattice.Product, ClassicalAddress, TwoCFA]
//class AAMKCFA3 extends BenchmarkKCFA3[TypeSetTaintLattice.Product, ClassicalAddress, ThreeCFA]

class AAMEVT1 extends BenchmarkEVT1[TypeSetTaintLattice.Product, ClassicalAddress, OneCFA]
class AAMEVT2 extends BenchmarkEVT2[TypeSetTaintLattice.Product, ClassicalAddress, TwoCFA]
//class AAMEVT3 extends BenchmarkEVT3[TypeSetTaintLattice.Product, ClassicalAddress, ThreeCFA]

class AAMLST1 extends BenchmarkLST1[TypeSetTaintLattice.Product, ClassicalAddress, OneCFA]
class AAMLST2 extends BenchmarkLST2[TypeSetTaintLattice.Product, ClassicalAddress, TwoCFA]
//class AAMLST3 extends BenchmarkLST3[TypeSetTaintLattice.Product, ClassicalAddress, ThreeCFA]

abstract class BenchmarkKCFA0[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 1;
  amount = 0 
  machine.sensitivity = 1;
}

abstract class BenchmarkKCFA1[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 1;
  amount = 1 
  machine.sensitivity = 1;
}

abstract class BenchmarkKCFA2[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 1;
  amount = 2 
  machine.sensitivity = 1;
}

abstract class BenchmarkKCFA3[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 1;
  amount = 3 
  machine.sensitivity = 1;
}

abstract class BenchmarkEVT0[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 2;
  amount = 0 
  machine.sensitivity = 2;
}

abstract class BenchmarkEVT1[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 2;
  amount = 1 
  machine.sensitivity = 2;
}

abstract class BenchmarkEVT2[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 2;
  amount = 2 
  machine.sensitivity = 2;
}

abstract class BenchmarkEVT3[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 2;
  amount = 3 
  machine.sensitivity = 2;
}

abstract class BenchmarkLST0[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 3;
  amount = 0 
  machine.sensitivity = 3;
}

abstract class BenchmarkLST1[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 3;
  amount = 1 
  machine.sensitivity = 3;
}

abstract class BenchmarkLST2[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 3;
  amount = 2 
  machine.sensitivity = 3;
}

abstract class BenchmarkLST3[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks2[SchemeExp, Abs, Addr, Time] {

  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  sensitivity = 3;
  amount = 3 
  machine.sensitivity = 3;
}




