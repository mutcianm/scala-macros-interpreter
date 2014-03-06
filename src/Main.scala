import interpreter.{ASTInterpreter, VMInterpreter}

import scala.reflect.runtime.{universe => ru}
import scala.math._


object Main extends App{
  class C { def foo(i: Int) = i * 2 }
  override def main(args: Array[String]) = {

    val tmp = new ASTInterpreter()
    tmp.run(ru.reify {
//     val a = List.range(1,4)

      val a = 5
//      val b = a match {
//        case 1 => "one"
//        case 5 => "five"
//        case _ => "other"
//      }
      val asd: Option[Int] = Some(1)
      asd match {
        case Some(value: Int) => value
        case asd: Option[Int] => 3
        case None => 2
        case _ => 0
      }
      val c: Any = 123
      c match {
        case _:Int =>
        case j:Int =>
        case _ =>
      }
    }.tree)
//    tmp.run()
  }
}
