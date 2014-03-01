package interpreter

import scala.reflect.runtime.universe._
import scala.collection.mutable


class VMInterpreter {
  val symbolTable: mutable.HashMap[String,Any] = mutable.HashMap()
  val irStack: mutable.Stack[VMOp] = mutable.Stack()
  val vmStack: mutable.Stack[VMOp] = mutable.Stack()
  var depth: Int = 0

  def test(expr: Tree): Unit = {
    expr match {
      case Apply(fun, args) =>
        irStack.push(new VMSymbolRef(getMethodName(fun)))
        args.foreach(processFunctionArgs)
        irStack.push(new VMCallOp(args.size))
      case ValDef(mods, name, tpt, rhs) =>
        symbolTable.put(name.decoded, getInitializer(rhs))
      case Block(stats, exprs) =>
        depth += 1
        stats.foreach(test)
        test(exprs)
        depth -= 1
      case wtf =>
        println(wtf)
        println(wtf.getClass)
    }
  }

  private def processExpr(expr: Tree) = {
    expr match {
      case Apply(fun, args) =>
        irStack.push(new VMCallOp(args.size))
        irStack.push(new VMSymbolRef(getMethodName(fun)))
        args.foreach(processFunctionArgs)
      case ValDef(mods, name, tpt, rhs) =>
        symbolTable.put(name.decoded, getInitializer(rhs))
      case wtf =>
        println(wtf)
        println(wtf.getClass)
    }
  }

  private def getMethodName(expr: Tree): String = {
    expr match {
      case Select(qualifier, name) =>
        qualifier match {
          case Ident(id_name) =>
            if (symbolTable.contains(id_name.decoded))
              irStack.push(new VMObjRef(id_name.decoded))
            else
              println("$name is not defined!")
          case other =>
            test(qualifier)
        }
        name.encoded
    }
  }

  private def processFunctionArgs(expr: Tree) = {
    expr match {
      case Ident(name) => irStack.push(new VMObjRef(name.decoded))
      case other => test(other)
    }
  }

  private def visitBlock(expr: Block) = {
    for (it <- expr.children) {
      test(it)
    }
  }

  private def getInitializer(expr: Tree): Any = {
    expr match {
      case Literal(value) => value.value
    }
  }

  def run() = {
    while (!irStack.isEmpty) {
      irStack.pop() match {
        case VMCallOp(i) =>
          val name = irStack.pop().asInstanceOf[VMObjRef]
          val symb = irStack.pop().asInstanceOf[VMSymbolRef]
          val v = symbolTable.getOrElse(name.v, null).getClass.getMethod(symb.name)
        case other =>
          vmStack.push(other)
      }
    }
  }
}


