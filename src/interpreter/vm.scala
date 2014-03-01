package interpreter

trait VMOp

class VMObjRef(val v: String) extends VMOp
class VMSymbolRef(val name: String) extends VMOp

case class VMCallOp(val argNum: Int) extends VMOp
