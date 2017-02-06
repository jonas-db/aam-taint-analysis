trait Timestamp[T] {
  def name: String
  def initial: T
  def tick[Exp](t: T, e: Exp): T
}

case class KCFA(k: Int) {
  trait KCFATimestamp
  object KCFATimestamp {
    case class Time[Exp](history: List[Exp]) extends KCFATimestamp
    implicit object KCFATimestampTimestamp extends Timestamp[KCFATimestamp] {
      def name = "$k-CFA"
      def initial = Time(List())
      def tick[Exp](t: KCFATimestamp, e: Exp) = t match {
        case t : Time[Exp] => {
          Time[Exp]((e :: t.history).take(k))
        }
      }
    }
  }
}

//    case class Time[Exp](history: Set[Exp]) extends KCFATimestamp
//    implicit object KCFATimestampTimestamp extends Timestamp[KCFATimestamp] {
//      def name = "$k-CFA"
//      def initial = Time(Set())
//      def tick[Exp](t: KCFATimestamp, e: Exp) = t match {
//        case t : Time[Exp] => {
//          Time[Exp]((Set(e) ++ t.history).take(k))
//        }
//      }
//    }

object Timestamps {
  val ZeroCFA = KCFA(0)
  type ZeroCFA = ZeroCFA.KCFATimestamp
  val OneCFA = KCFA(1)
  type OneCFA = OneCFA.KCFATimestamp
  val TwoCFA = KCFA(2)
  type TwoCFA = TwoCFA.KCFATimestamp
  val ThreeCFA = KCFA(3)
  type ThreeCFA = ThreeCFA.KCFATimestamp
  val FourCFA = KCFA(4)
  type FourCFA = FourCFA.KCFATimestamp
  val FiveCFA = KCFA(5)
  type FiveCFA = FiveCFA.KCFATimestamp  
}
