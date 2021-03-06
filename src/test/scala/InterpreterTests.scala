import org.scalatest._
import org.scalatest.prop._
import Timestamps._

abstract class Benchmarks[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends FlatSpec with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val sem: Semantics[Exp, Abs, Addr, Time]
  val machine: AbstractMachine[Exp, Abs, Addr, Time]

  def checkResult(file: String, expected: Abs): Unit = {
    val result = machine.eval(sem.parse(Main.fileContent(s"test/suite/$file")), sem, false)
    assert(result.containsFinalValue(expected))
    println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")
  }
  def checkResult(file: String, expected: List[Abs]): Unit = {
    val result = machine.eval(sem.parse(Main.fileContent(s"test/suite/$file")), sem, false)
    println(result.finalValues)
    assert(expected.map(abs => { result.containsFinalValue(abs) }).foldLeft(true)((res, nxt) => res && nxt) 
        && result.finalValues.size == expected.size)
    println(s"${machine.name}, $file: ${result.numberOfStates}, ${result.time}")    
  }
  def check(file: String, expected: Abs): Unit =
    file should s"eval to $expected" in { checkResult(file, expected) }

  def checkn(file: String, expected: List[Abs]): Unit =
    file should s"eval to $expected" in { checkResult(file, expected) }
  
  check("deletedataprop.scm", abs.inject(false))
  check("deletedataprop2.scm", abs.inject(1))
  check("getdataprop.scm", abs.inject(1))
  check("getdatapropdyn.scm", abs.inject(1))
  checkn("getdatapropdyn2.scm", List(abs.inject(1)))
  check("getaccprop.scm", abs.inject(1))
  check("getaccprop2.scm", abs.inject(1))
  check("setdataprop.scm", abs.inject("ok"))
  check("setaccprop.scm", abs.inject("ok"))
  check("getdatapropproto.scm", abs.inject("ok"))
  check("getdatapropprotoshadow.scm", abs.inject(1)) 
  check("eventadd.scm", abs.inject("ok"))
  checkn("eventadd2.scm", List(abs.injectSymbol("ev2"), abs.inject(false), abs.inject(1))) // without abstract counting obj store += abs.inject(1), 
  checkn("eventadd3.scm", List(abs.inject(true))) // without count += abs.inject(1), 
  check("eventremove.scm", abs.inject(1))
  check("dispatcheventdyn.scm", abs.inject("ok"))
  checkn("getpropertyfromnobjects.scm", List(abs.inject("ok"), abs.inject(false)))
  checkn("protojoin.scm", List(abs.inject(1), abs.inject(true)))
  checkn("propjoin.scm", List(abs.inject(1), abs.inject(false)))
  
//  check("eventadd.scm", abs.inject(1))
  //  check("blur.scm", abs.inject(true))
//  check("count.scm", abs.inject("done"))
//  check("cpstak.scm", abs.inject(6))
//  check("fib.scm", abs.inject(3))
//  check("eta.scm", abs.inject(false))
//  check("fact.scm", abs.inject(120))
//  check("gcipd.scm", abs.inject(36))
//  check("inc.scm", abs.inject(4))
//  check("kcfa2.scm", abs.inject(false))
//  check("kcfa3.scm", abs.inject(false))
//  check("loop2.scm", abs.inject(550))
//  check("mj09.scm", abs.inject(2))
//  check("mut-rec.scm", abs.inject(true))
//  check("rotate.scm", abs.inject("hallo"))
//  check("sq.scm", abs.inject(9))
//  check("sym.scm", abs.injectSymbol("foo"))
//  check("ack.scm", abs.inject(4))
//  check("collatz.scm", abs.inject(5))
//  check("widen.scm", abs.inject(10))
  /* The following tests are disabled because they can take a long time to evaluate */
  // check("rsa.scm", abs.inject(true))
  // check("sat.scm", abs.inject(true))
  // check("primtest.scm", abs.inject(1))
  // check("nqueens.scm", abs.inject(92))
  // check("church.scm", abs.inject(true))
  // check("boyer.scm", abs.inject(true))
  // check("dderiv.scm", abs.inject(true))
  // check("takl.scm", abs.inject(true))
}

abstract class OneResultTests[Exp : Expression, Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends FlatSpec with Matchers {
  val abs = implicitly[AbstractValue[Abs]]
  val sem: Semantics[Exp, Abs, Addr, Time]
  val machine: AbstractMachine[Exp, Abs, Addr, Time]

  def check(file: String, expected: Abs): Unit =
    file should s"have only one final state in concrete mode and return $expected" in {
      val result = machine.eval(sem.parse(Main.fileContent(s"test/$file")), sem, false)
      println("test" +result)
      result.finalValues.size should equal (1)
      result.finalValues.head should equal (expected)
    }

  check("blur.scm", abs.inject(true))
  check("count.scm", abs.inject("done"))
  // check("cpstak.scm", abs.inject(6)) disabled due to the time it takes
  check("fib.scm", abs.inject(3))
  check("eta.scm", abs.inject(false))
  check("fact.scm", abs.inject(120))
  check("gcipd.scm", abs.inject(36))
  check("inc.scm", abs.inject(4))
  check("kcfa2.scm", abs.inject(false))
  check("kcfa3.scm", abs.inject(false))
  check("loop2.scm", abs.inject(550))
  check("mj09.scm", abs.inject(2))
  check("mut-rec.scm", abs.inject(true))
  check("rotate.scm", abs.inject("hallo"))
  check("sq.scm", abs.inject(9))
  check("sym.scm", abs.injectSymbol("foo"))
  check("ack.scm", abs.inject(4))
  check("collatz.scm", abs.inject(5))
  check("widen.scm", abs.inject(10))
  /* Disabled for slower tests */
  // check("rsa.scm", abs.inject(true))
  // check("sat.scm", abs.inject(true))
  // check("primtest.scm", abs.inject(1))
  // check("nqueens.scm", abs.inject(92))
  // check("church.scm", abs.inject(true))
  // check("boyer.scm", abs.inject(true))
  // check("dderiv.scm", abs.inject(true))
  // check("takl.scm", abs.inject(true))
}

//abstract class AACBenchmarks[Abs : AbstractValue, Addr : Address, Time : Timestamp]
//    extends Benchmarks[SchemeExp, Abs, Addr, Time] {
//  val sem = new SchemeSemantics[Abs, Addr, Time]
//  val machine = new AAC[SchemeExp, Abs, Addr, Time]
//}

abstract class AAMBenchmarks[Abs : AbstractValue, Addr : Address, Time : Timestamp]
    extends Benchmarks[SchemeExp, Abs, Addr, Time] {
  val sem = new SchemeSemantics[Abs, Addr, Time]
  val machine = new AAM[SchemeExp, Abs, Addr, Time]
  
}

//abstract class FreeBenchmarks[Abs : AbstractValue, Addr : Address, Time : Timestamp]
//    extends Benchmarks[SchemeExp, Abs, Addr, Time] {
//  val sem = new SchemeSemantics[Abs, Addr, Time]
//  val machine = new Free[SchemeExp, Abs, Addr, Time]
//}
//
//abstract class ConcurrentAAMBenchmarks[Abs : AbstractValue, Addr : Address, Time : Timestamp, TID : ThreadIdentifier]
//    extends Benchmarks[SchemeExp, Abs, Addr, Time] {
//  val sem = new SchemeSemantics[Abs, Addr, Time]
//  val machine = new ConcurrentAAM[SchemeExp, Abs, Addr, Time, TID]
//}

/* Concrete tests are disabled because of cpstak takes too much time to compute since it requires more than 75k recursive calls */
/* Type tests are disabled because they fail due to their inability to support a join between a closure and other abstract values */
//class AACConcreteBenchmarks extends AACBenchmarks[AbstractConcrete, ConcreteAddress]
//class AACTypeBenchmarks extends AACBenchmarks[AbstractType, ClassicalAddress]
//class AACTypeSetBenchmarks extends AACBenchmarks[AbstractTypeSet, ClassicalAddress, ZeroCFA]

//class AAMConcreteBenchmarks extends AAMBenchmarks[AbstractConcrete, ConcreteAddress, ZeroCFA]
////class AAMTypeBenchmarks extends AAMBenchmarks[AbstractType, ClassicalAddress]
//class AAMTypeSetBenchmarks extends AAMBenchmarks[AbstractTypeSet, ClassicalAddress, ZeroCFA]
//class AAMTypeDrepSetBenchmarks extends AAMBenchmarks[AbstractType, ClassicalAddress, ZeroCFA]
//class FreeConcreteBenchmarks extends FreeBenchmarks[AbstractConcrete, ConcreteAddress]
//class FreeTypeBenchmarks extends FreeBenchmarks[AbstractType, ClassicalAddress]
//class FreeTypeSetBenchmarks extends FreeBenchmarks[AbstractTypeSet, ClassicalAddress, ZeroCFA]

//class ConcurrentAAMTypeSetBenchmarks extends ConcurrentAAMBenchmarks[AbstractTypeSet, ClassicalAddress, ZeroCFA, ContextSensitiveTID]

//class AACOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA] {
//  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]
//  val machine = new AAC[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA]
//}
//
//class AAMOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA] {
//  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]
//  val machine = new AAM[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA]
//}
//
//class FreeOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA] {
//  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]
//  val machine = new Free[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA]
//}
//
//class ConcurrentAAMOneResultTests extends OneResultTests[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA] {
//  val sem = new SchemeSemantics[AbstractConcrete, ConcreteAddress, ZeroCFA]
//  val machine = new ConcurrentAAM[SchemeExp, AbstractConcrete, ConcreteAddress, ZeroCFA, ContextSensitiveTID]
//}
