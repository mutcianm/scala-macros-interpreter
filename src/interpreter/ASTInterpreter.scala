package interpreter

import scala.collection.mutable
import scala.reflect.runtime.universe._

class ASTInterpreter {
  val functionDefs: mutable.HashMap[Symbol, FunctionDef] = mutable.HashMap()
  var currentFunction: FunctionDef = null
  val rm = runtimeMirror(getClass.getClassLoader)

  def run(expr: Tree): Any = {
    currentFunction = new FunctionDef(mutable.HashMap(), List(), expr)
    functionDefs.put(expr.symbol, currentFunction)
    processExpr(currentFunction.body)
  }

  def processExpr(expr: Tree): Any = {
    expr match {
      case it@Apply(fun, args) =>
        processFunCall(fun, args)
      case ValDef(mods, name, tpt, rhs) =>
        currentFunction.symbolTable.put(name.decoded, getInitializer(rhs))
      case Assign(lhs, rhs) =>
        processAssign(lhs, rhs)
      case Literal(value) =>
        value.value
      case Ident(name) =>
        getBoundObject(name.decoded)
      case ifExpr: If => processIf(ifExpr)
      case label: LabelDef => processLabelDef(label)
      case Block(stats, exprs) =>
        stats.foreach(processExpr)
        processExpr(exprs)
      case defDef: DefDef =>
        processDefDef(defDef)
      case wtf =>
        println(wtf)
        println(wtf.getClass)
    }
  }


  private def processFunctionArg(expr: Tree): Any = {
    expr match {
      case Ident(name) => currentFunction.symbolTable.getOrElse(name.decoded, null)
      case it@Function(vparamss, body) =>
        val args = vparamss.map({it => (it.name.decoded, null)})
        val f = new FunctionDef(mutable.HashMap(), args, body)
        functionDefs.put(it.symbol, f)
        val oldFunc = currentFunction
        vparamss.size match {
          case 1 => a: Any => currentFunction = f; updateArgs(List(a)); val ret = processExpr(body); currentFunction = oldFunc; ret
          case 2 => (a: Any,b: Any) => currentFunction = f; updateArgs(List(a,b)); val ret = processExpr(body); currentFunction = oldFunc; ret
        }

      case other => processExpr(other)
    }
  }

  private def processFunCall(fun: Tree, args: List[Tree]): Any = {
    fun match {
      case Select(qualifier, name) =>
        //direct call
        qualifier match {
          case it@Ident(id_name) =>
            it.symbol match {
              case ident: ModuleSymbol => emulateCall(ident, name.encoded, args.map(processFunctionArg))
              case _ => emulateCall(id_name.decoded, name.encoded, args.map(processFunctionArg))
            }
          case other =>
            val tmpVal = processExpr(other)
            val tmpname = "*tmpval"+currentFunction.symbolTable.size.toString
            currentFunction.symbolTable.put(tmpname, tmpVal)
            val ret = emulateCall(tmpname, name.encoded, args.map(processFunctionArg))
            currentFunction.symbolTable.remove(tmpname)
            ret
        }
      case id: Ident =>
        val f = functionDefs.getOrElse(id.symbol,null)
        f.symbolTable ++= args.zip(f.args).map( it => it._2._1 -> processFunctionArg(it._1))
        val oldFunc = currentFunction
        currentFunction = f
        val ret = processExpr(f.body)
        currentFunction = oldFunc
        ret
      case Apply(f, a) =>
        val tmp = processFunCall(f, a)
      case other =>
        println(other)
        println(other.getClass)
    }
  }

  private def getBoundObject(id: String): Any = {
    currentFunction.symbolTable.get(id) match {
      case Some(value) => value
      case None =>
        null
    }
  }

  private def processAssign(lhs: Tree, rhs: Tree): Any = {
    val rhsVal = processExpr(rhs)
    lhs match {
      case Ident(name) =>
        currentFunction.symbolTable.update(name.toString, rhsVal)
      case other => other
    }
  }

  private def processIf(expr: If): Any = {
    val res = processExpr(expr.cond)
    if(res.asInstanceOf[Boolean])
      processExpr(expr.thenp)
    else
      processExpr(expr.elsep)
  }

  private def processLabelDef(label: LabelDef): Any = {
    functionDefs.put(label.symbol, new FunctionDef(mutable.HashMap(), List(), label))
    val ret = processExpr(label.rhs)
    functionDefs.remove(label.symbol)
    ret
  }

  private def processDefDef(d: DefDef): Any = {
    val locals = mutable.HashMap[String, Any]()
    val args = d.vparamss.map(l => l.map({it => (it.name.decoded, null)})).flatten
    functionDefs.put(d.symbol, new FunctionDef(locals, args, d.rhs))
  }

  private def updateArgs(values: List[Any]) = {
    currentFunction.args.zip(values).foreach(it => currentFunction.symbolTable(it._1._1) = it._2)
  }

  private def emulateCall(ident: String, methodName: String, args: List[Any]): Any = {
    new Caller(getBoundObject(ident)).call(methodName, args)
  }

  private def emulateCall(ident: ModuleSymbol, methodName: String, args: List[Any]): Any = {
    new Caller(rm.reflectModule(ident).instance).call(methodName, args)
  }

  private def visitBlock(expr: Block) = {
    for (it <- expr.children) {
      processExpr(it)
    }
  }

  private def getInitializer(expr: Tree): Any = {
    expr match {
      case Literal(value) => value.value
      case other => processExpr(other)
    }
  }
}

class FunctionDef(val symbolTable: mutable.HashMap[String,Any], val args: List[(String, Any)], val body: Tree)

case class Caller(var klass:Any) {
  var calleeType: Type = classMap(klass)
  val mirror = klass match {
    case _:java.lang.Integer => runtimeMirror(calleeType.getClass.getClassLoader).reflect(klass.asInstanceOf[Int])
    case _:java.lang.String => runtimeMirror(calleeType.getClass.getClassLoader).reflect(klass.asInstanceOf[String])
    case _:java.lang.Double => runtimeMirror(calleeType.getClass.getClassLoader).reflect(klass.asInstanceOf[Double])
    case _ => runtimeMirror(calleeType.getClass.getClassLoader).reflect(klass)
  }


  def classMap(v: Any): Type = {
    v match {
      case _:java.lang.Integer => typeOf[Int]
      case _:java.lang.String => typeOf[String]
      case _:java.lang.Double => typeOf[Double]
      case other => runtimeMirror(getClass.getClassLoader).classSymbol(other.getClass).toType
    }
  }

  def getType[T: TypeTag](obj: T) = typeOf[T]

  implicit def toInt(in:Integer) = in.intValue()

  def typesConform(v1: List[Type], v2: List[Type]) = {
    v1.zip(v2).forall(it => it._2 <:< it._1)
  }

  def what(a:PolyType, b: List[Type]) = {
    val d = a.erasure.asInstanceOf[MethodType]
    true
  }

  def call(methodName:String,args:List[Any]):Any = {
//    def argtypes = args.map(classMap)

    val argTypes = args.map(classMap)
    try {
      val tmp = calleeType.members.collect {
        case m: MethodSymbol if !m.isPrivate && m.name.encoded == methodName => m -> m.typeSignature
      }
      val methods = calleeType.members.collect {
        case m: MethodSymbol if !m.isPrivate && m.name.encoded == methodName => m -> m.typeSignature
      }. collect  {
        case (m, mt @ MethodType(qwe, _)) if typesConform(qwe.map(it => it.typeSignature), argTypes)  => m -> mt
        case (m, mt @ PolyType(qwe, _)) if what(mt, argTypes) => m -> mt
      }
      val method = methods.head._1.asInstanceOf[MethodSymbol]
      val reflm = mirror.reflectMethod(method)
      val ret = if (args.isEmpty)
        reflm.apply()
        //dirty hack for seq arguments
      else if (method.paramss.head.size != args.size)
        reflm.apply(args)
      else
          reflm.apply(args: _*)
      println(s"$klass.$methodName($args) => $ret")
      ret

    } catch {
      case e: NoSuchMethodException =>
        //auto unboxing?
        throw e

        println(e.getMessage)

        null
    }
  }
}