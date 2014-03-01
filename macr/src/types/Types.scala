package types

import language.experimental.macros
import scala.reflect.runtime.{universe => ru}
import scala.reflect.macros.Context

object Types {

  def methodNames[A]: List[String] = macro methodNames_impl[A]
  def debug(param: Any): Unit = macro debug_impl
  def methods(param: Any): Unit = macro methods_impl

  def methodNames_impl[A : c.WeakTypeTag](c: Context): c.Expr[List[String]] = {
    import c.universe._

    val methods: List[String] = c.weakTypeOf[A].typeSymbol.typeSignature.
      declarations.toList.filter(_.isMethod).map(_.name.toString)

    val listApply = Select(reify(List).tree, TermName("apply"))

    c.Expr[List[String]](Apply(listApply, List(methods.map(x => Literal(Constant(x))):_*)))
  }

  def debug_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    val paramRep = show(param.tree)
    val paramRepTree = Literal(Constant(paramRep))
    val paramRepExpr = c.Expr[String](paramRepTree)
    reify { println(paramRepExpr.splice + " = " + param.splice) }
  }

  def extractClassDef(c: Context)(cls: c.universe.ClassSymbol): ru.ClassDef = {
    import c.universe._
    c.enclosingRun.units.foreach { it =>
      it.body match {
        case PackageDef(pid, stats) =>
          stats.foreach {
            case it@ClassDef(mods, name, tparams, impl) =>
              if (name == cls.name) return it.asInstanceOf[ru.ClassDef]
            case _ =>
          }
      }
    }
    null
  }

  def interpr(c: Context)(expr: ru.Tree, args: List[ru.Tree]): c.Expr[Unit] = {
    import c.universe._
    expr match {
      case ru.Apply(f, arg) =>
        println(f)
        println(arg)
    }
    reify({})
  }

  def methods_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    param.tree match {
      case Apply(fun, args) =>
        if (fun.symbol.isMethod) {
          val a = fun.symbol.asMethod
          val boundClass = a.owner.asClass
          val cls = extractClassDef(c)(boundClass)
          for( ru.DefDef(mods, name, tparams, varparams, tpt, rhs) <- cls.impl.body) {
            if (name == fun.symbol.name)
              interpr(c)(rhs.asInstanceOf[ru.Tree], args.asInstanceOf[List[ru.Tree]])
          }
//          c.enclosingRun.units
//          val m = ru.runtimeMirror(ru.getClass.getClassLoader)
//          val v = m.reflectClass(boundClass.asInstanceOf[ru.ClassSymbol])
//          val cons = boundClass.toType.members.find { it => it.isMethod && it.asMethod.isConstructor}
//          val ctor = v.reflectConstructor(cons.get.asMethod.asInstanceOf[ru.MethodSymbol])
//          val inst = ctor()
        }
      case other =>
    }
    reify { println(param.splice) }
  }

}

