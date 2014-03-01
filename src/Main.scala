import interpreter.{ASTInterpreter, VMInterpreter}

import scala.reflect.runtime.{universe => ru}
import scala.math._


object Main extends App{
  class C { def foo(i: Int) = i * 2 }
  override def main(args: Array[String]) = {

    val tmp = new ASTInterpreter()
    tmp.run(ru.reify {
     val a = List.range(1,4)

    }.tree)
//    tmp.run()
  }
}
