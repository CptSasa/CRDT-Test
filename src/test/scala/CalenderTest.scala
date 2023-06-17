import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.datatypes.AddWinsSet
import kofre.syntax.ReplicaId
import kofre.base.Uid.asId
import kofre.dotted.{Dotted}


import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

class CalenderTest {}

object CalenderSpecification extends Properties("Calender") {
  /*
  def addCal(calender: Calender, value: Int): Calender = {
    val cal = calender.copy()
    var tmp = cal.calList
    if (calender.sum() + value <= 30) {
      tmp = cal.calList.add(using ("" + System.currentTimeMillis()).asId)(value)
    }
    return Calender(tmp)
  }
  def addRemainingDays(calender: Calender,value: Int): Calender ={
    val cal = calender.copy()
    var tmp = cal.calList
    if (calender.sum() + value <= 30) {
      val remainingDays = 30-cal.sum()
      tmp = cal.calList.add(using ("" + System.currentTimeMillis()).asId)(remainingDays)
    }
    return Calender(tmp)
  }
  def removeCal(calender: Calender, value: Int): Calender = {
    val cal = calender.copy()
    val tmp = cal.calList.remove(value)
    return Calender(tmp)
  }
  def mergeCal(calender: Calender, calender2: Calender): Calender = {
    val a = calender.calList merge calender2.calList
    return Calender(a)
  }
  def functionList(): List[(Calender, Int) => Calender] = {
    val list = List(removeCal(_, _), addCal(_, _),addRemainingDays(_,_))
    return list
  }
  */
  def generateCalender(): Calender ={
    var cal = Calender(Dotted.empty)
    var trace = List(Int)
    for(n <- 0 until generateSizedInt().sample.get){
      if(n%3 == 0){
        cal = cal.addCal(cal,generateSizedInt().sample.get)
      }
      else{
        cal = cal.removeCal(cal,4)
      }
    }
    return cal
  }
  def generateWithHigherOrder(): Calender ={
    var cal = Calender(Dotted.empty)
    for(n<-0 until generateSizedInt().sample.get){
      cal = cal.functionList()(n%3)(cal,generateSizedInt().sample.get)
    }
    return cal
  }
  def generateFunctionCalls(): Calender ={
    var cal = Calender(Dotted.empty)
    //cal = cal.convertFunction()(0)(cal)
    cal = cal.generateRandomFunctionCall()(cal)
    return cal
  }
  def generateSizedInt(): Gen[Int] = {
    val gener = Gen.choose(0, 30)
    gener
  }
  val holdsRes = forAll(generateCalender(), generateCalender()) { (x: Calender, y: Calender) =>
    var cal = Calender(Dotted.empty)
    cal = generateFunctionCalls()
    System.out.println(cal.toString)
    val tmp = x.mergeCal(x,y)
    tmp.holdsRestriction() && tmp.sum()<=30
  }
  val holdsResWithList = forAll(generateWithHigherOrder(), generateWithHigherOrder()) { (x: Calender, y: Calender) =>
    val tmp = x.mergeCal(x, y)
    //System.out.println(x.toString)
    //System.out.println(y.toString)
    tmp.holdsRestriction() && tmp.sum()<=30
  }
  property("holdsResWork") = holdsRes
  property("holdsRes") = holdsResWithList



}

