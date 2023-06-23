import CalenderSpecification.generateSizedInt
import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.datatypes.AddWinsSet
import kofre.syntax.ReplicaId
import kofre.base.Uid.asId
import kofre.dotted.Dotted
import org.scalacheck.Gen


trait toTest{
  def functionList(): List[(Calender) => Calender]
  def holdsRestriction(): Boolean
  def generate(): Calender
}
/*
class TestGenerator extends toTest {
  override def holdsRestriction(): Boolean = false
  override def generate(): Calender ={
    var cal = Calender (Dotted.empty)
    for (n <- 0 until generateSizedInt ().sample.get) {
    cal = functionList () (n % 3) (cal, generateSizedInt ().sample.get)
  }
    return cal
  }
  override def functionList(): List[(Calender, Int) => Calender] = List()
}
*/


case class Calender(calList: Dotted[AddWinsSet[Int]])  {
  def sum(): Int = {
    calList.elements.sum
  }


  def convertFunction() : (List[Calender=>Calender])={
    def convertedAddCal(calender : Calender) = addCal(calender,generateSizedInt().sample.get)
    def convertRemoveCal(calender : Calender) = removeCal(calender, generateSizedInt().sample.get)
    return List(convertedAddCal(_),convertRemoveCal(_))
  }
  def generateRandomFunctionCall(): Calender=>Calender = {
    val functionToCall = Gen.choose(0,convertFunction().size-1)
    return convertFunction()(functionToCall.sample.get)
  }
  /*
    def generateFunctionWithParameters(calender: Calender,list: List[parameterType]): (Calender=>Calender) ={
  
    }
  */
  
  /*
    def convertCalenderFunctions[T](func: ((Calender, T) => Calender)) : Calender=>Calender = {
      return def convertedCal(calender: Calender,generateFittingParameter)
    }
*/
  def generateFittingParam[T](param: T): Int | String |Double = {
    val typ = getType (param)
    typ match {
      case ("Int") => return generateSizedInt().sample.get
      case ("String") => return "someString"
      case ("Double") => return Gen.choose(0.0, 30.0).sample.get
      case _ => return 0
    }
  }

  def getType[T](param: T): String = {
    val typeName = param.getClass.getTypeName
    val simplifiedTypeName = typeName.substring(typeName.lastIndexOf('.') + 1)
    simplifiedTypeName
  }
  
  def addCal(calender: Calender, value: Int): Calender = {
    val cal = calender.copy()
    var tmp = cal.calList
    if (calender.sum() + value <= 30) {
      tmp = cal.calList.add(using ("" + System.currentTimeMillis()).asId)(value)
    }
    return Calender(tmp)
  }

  def addRemainingDays(calender: Calender, value: Int): Calender = {
    val cal = calender.copy()
    var tmp = cal.calList
    if (calender.sum() + value <= 30) {
      val remainingDays = 30 - cal.sum()
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

  def generateSizedInt(): Gen[Int] = {
    val gener = Gen.choose(0, 30)
    gener
  }
  def functionList(): List[(Calender, Int) => Calender] = List(removeCal(_,_),addCal(_,_),addRemainingDays(_,_))

  def holdsRestriction(): Boolean = sum()<=30
  /*
    override def generate(): Calender ={
      var cal = Calender(Dotted.empty)
      for (n <- 0 until generateSizedInt().sample.get) {
        cal = functionList()(n % 3)(cal, generateSizedInt().sample.get)
      }
      return cal
    }
  */
  override def toString: String = calList.elements.toString()

}
