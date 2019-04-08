// LIU-OO-F-X-TY-1709
// programming Language,
//             Interpreted,
//             Untyped,
//             Object-Oriented,
//             Functional,
//             X (no trace of imports, includes, packaging or module system)
// @author tyukiand

package liuoofxty

/******************************************************************************\
|                Values and Runtime Datastructures                             |
\******************************************************************************/
sealed trait Value
case class IntValue(i: Int) extends Value {
  override def toString = i.toString
}
case class BooleanValue(b: Boolean) extends Value {
  override def toString = b.toString
}
case object UnitValue extends Value {
  override def toString = "()"
}
case object Undefined extends Value {
  override def toString = "<undefined>"
}
case class CharValue(c: Char) extends Value {
  override def toString = c.toString
}
case class Closure(args: List[String], body: Expr, ctx: Context) extends Value {
  override def toString = "<closure>"
}
case class BuiltInFunction(f: List[Value] => Value) extends Value {
  override def toString = "<builtInFunction>"
}

/* Method signature consists of name and arity. */
case class MethSig(name: String, arity: Int)

/* Constructor signature consists of arity only */
case class ConsSig(arity: Int)

sealed trait Constructor {
  def invoke(args: List[Value]): Value
}
sealed trait Method[O] {
  def invoke(obj: O, args: List[Value]): Value
}
sealed trait Clazz[O] {
  def name: String
  def getConstructor(sig: ConsSig): Constructor
  def getMethod(sig: MethSig): Method[O]
  def set(obj: O, name: String, value: Value): Unit
  def get(obj: O, name: String): Value
}

sealed trait Obj extends Value {
  def invokeMethod(name: String, args: List[Value]): Value
  def set(name: String, value: Value): Unit
  def get(name: String): Value
}

// case class JvmObject(clazz: JvmClass, data: ) extends Obj

/******************************************************************************\
|                User-defined classes (can be written as code)                 |
\******************************************************************************/
case class UserObject(userClass: UserClazz, data: Array[Value]) extends Obj {
  def invokeMethod(name: String, args: List[Value]): Value =
    userClass.getMethod(MethSig(name, args.size)).invoke(this, args)
  def set(name: String, value: Value): Unit = userClass.set(this, name, value)
  def get(name: String): Value = userClass.get(this, name)
  override def hashCode(): Int = {
    (userClass.hashCode + " " +
    (data map { _.hashCode }).mkString(",")).hashCode
  }
  override def equals(other: Any): Boolean = {
    other match {
      case UserObject(c, d) => (c == userClass) && d.sameElements(data)
      case _ => false
    }
  }
}

case class UserClazz(
  name: String,
  constructorDefs: Map[ConsSig, ConstructorDefinition],
  methods: Map[MethSig, MethodDefinition],
  dataOffsets: Map[String, Int],
  ctx: Context
) extends Clazz[UserObject] {
  def getMethod(sig: MethSig): UserMethod = {
    (for (mDef <- methods.get(sig)) yield {
      UserMethod(mDef.argNames, mDef.body, this.ctx)
    }).getOrElse{
      throw new Error(
        "Clazz " + name + " does not have a method with signature " + sig)
    }
  }

  def getConstructor(sig: ConsSig): UserConstructor = {
    (for (cDef <- constructorDefs.get(sig)) yield {
      UserConstructor(this, cDef.argNames, cDef.body, this.ctx)
    }).getOrElse{
      throw new Error(
        "Clazz " + name + " does not have a constuctor with arity " + sig)
    }
  }

  def set(obj: UserObject, memberName: String, value: Value): Unit = {
    obj.data(getDataOffset(memberName)) = value
  }

  def get(obj: UserObject, memberName: String): Value = {
    obj.data(getDataOffset(memberName))
  }

  private def getDataOffset(memberName: String): Int = {
    dataOffsets.get(memberName).getOrElse {
      throw new Error(
        "Clazz " + name + " does not have a member called " + memberName)
    }
  }

  val numMemberVars = dataOffsets.size
}

case class UserConstructor(clazz: UserClazz, argNames: List[String], body: Expr,
  ctx: Context) extends Constructor {

  def invoke(args: List[Value]): Value = {
    val data = Array.fill[Value](clazz.numMemberVars)(Undefined)
    val obj = UserObject(clazz, data)
    val c = ctx.enterBlock
    c.defineAll(argNames zip args)
    c.defineVar("this", obj)
    // result thrown away, we need the object, not the last value
    Semantics.eval(body, c)
    obj
  }
}

case class UserMethod(argNames: List[String], body: Expr, ctx: Context)
  extends Method[UserObject] {
  def invoke(receiver: UserObject, args: List[Value]): Value = {
    val as = argNames zip args
    val ctxWithArgs = ctx.enterBlock
    ctxWithArgs.defineAll(as)
    ctxWithArgs.defineVar("this", receiver)
    Semantics.eval(body, ctxWithArgs)
  }
}

/******************************************************************************\
|    Built-in classes and methods (predefined or loaded from external jars)    |
\******************************************************************************/

// invocation of methods and constructors with Java reflection is a bloody mess.
// No sane way to do it with Java reflection alone, too much trouble with all
// the boxing-unboxing and subtype compatibility. Use Apache commons.
import org.apache.commons.lang3.reflect.{MethodUtils, ConstructorUtils,
  FieldUtils}

case class JvmObject(wrapped: AnyRef) extends Obj {
  private def jvmClass = JvmClazz(wrapped.getClass)
  def invokeMethod(name: String, args: List[Value]): Value =
    jvmClass.getMethod(MethSig(name, args.size)).invoke(this, args)
  def set(name: String, value: Value): Unit = jvmClass.set(this, name, value)
  def get(name: String): Value = jvmClass.get(this, name)
  override def toString = wrapped.toString
}

import scala.language.existentials
case class JvmClazz(clazz: Class[_]) extends Clazz[JvmObject] {
  import JvmClassInterop._
  def name: String = clazz.getName
  def getMethod(sig: MethSig) = new Method[JvmObject] {
    val methodName = sig.name
    def invoke(jObj: JvmObject, args: List[Value]): Value = {
      val unpackedArgs = (args map unpack).toArray
      // Here, we actually do use the classes of arguments for dispatch,
      // the name and arity alone is not enough here.
      pack(
        MethodUtils.invokeMethod(jObj.wrapped, methodName, unpackedArgs: _*).
        asInstanceOf[AnyRef]
      )
    }
  }

  def getConstructor(sig: ConsSig) = new Constructor {
    def invoke(args: List[Value]): Value = {
      val unpackedArgs = (args map unpack).toArray
      val argClasses = unpackedArgs.map{_.getClass}
      pack(ConstructorUtils.invokeConstructor(clazz, unpackedArgs: _*).
        asInstanceOf[AnyRef]
      )
    }
  }

  def set(obj: JvmObject, memberName: String, value: Value): Unit = {
    FieldUtils.writeDeclaredField(obj.wrapped, memberName, unpack(value), false)
  }

  def get(obj: JvmObject, memberName: String): Value = {
    pack(FieldUtils.readDeclaredField(obj.wrapped, memberName, false))
  }
}

object JvmClassInterop {
  def unpack(v: Value): AnyRef = v match {
    // unpacking POJOs, re-boxing premitive types into something java reflection
    // understands better.
    case JvmObject(o) => o
    case IntValue(i) => new java.lang.Integer(i)
    case BooleanValue(b) => new java.lang.Boolean(b)
    case CharValue(c) => new java.lang.Character(c)

    // no way to unpack it in anything closer to POJO
    case uo: UserObject => uo
    case UnitValue => UnitValue
    case Undefined => Undefined
    case o: JvmClazzObject => o
    case c: Closure => c
    case f: BuiltInFunction => f
  }

  def pack(o: AnyRef): Value = o match {
    // unbox-rebox
    case i: java.lang.Integer => IntValue(i)
    case b: java.lang.Boolean => BooleanValue(b)
    case c: java.lang.Character => CharValue(c)

    // pass values that exist only in our language without unboxing
    case UnitValue => UnitValue
    case Undefined => Undefined
    case uo: UserObject => uo
    case c: Closure => c
    case f: BuiltInFunction => f
    case o: JvmClazzObject => o

    // box all other POJOs into a `JvmObject` wrapper
    case pojo => JvmObject(pojo)
  }
}

/******************************************************************************\
|            JVM Class objects that allow calling Java's static methods        |
\******************************************************************************/

/** Special singleton objects created for used Java classes. Their methods
  * correspond to the static methods of the class.
  */
case class JvmClazzObject(c: Class[_]) extends Obj {
  import JvmClassInterop._
  def invokeMethod(name: String, args: List[Value]): Value = {
    pack(MethodUtils.invokeStaticMethod(c, name, args map unpack))
  }
  def set(name: String, value: Value): Unit = {
    FieldUtils.writeStaticField(c, name, unpack(value))
  }
  def get(name: String): Value = {
    pack(FieldUtils.readStaticField(c, name))
  }
}

/******************************************************************************\
|                              Scopes and Contexts                             |
\******************************************************************************/

import scala.collection.mutable.{HashMap => Hash}
case class Scope(
  variables: Hash[String, Value],
  classes: Hash[String, Clazz[_]]
) {
  def containsVar(s: String): Boolean = variables.contains(s)
  def lookupVar(s: String) = variables(s)
  def setVar(name: String, value: Value): Unit = variables(name) = value
  def containsClazz(c: String): Boolean = classes.contains(c)
  def lookupClazz(s: String) = classes(s)
  def defineClazz(name: String, c: Clazz[_]): Unit = classes(name) = c
}

object Scope {
  def empty = Scope(Hash.empty, Hash.empty)
}

case class Context(scopes: List[Scope]) {
  def lookupVar(varName: String): Value = scopes match {
    case Nil => throw new Error("Unknown variable: " + varName)
    case h :: t => if (h containsVar varName) h.lookupVar(varName) else {
      Context(t).lookupVar(varName)
    }
  }

  def isUnusedVar(varName: String): Boolean = !scopes.head.containsVar(varName)
  def isUnusedClazz(name: String): Boolean = !scopes.head.containsClazz(name)

  def defineVar(varName: String, value: Value): Unit = {
    if (isUnusedVar(varName)) {
      scopes.head.setVar(varName, value)
    } else {
      throw new Error(
        "Variable with name '" + varName + "' already defined in this scope")
    }
  }

  def defineAll(namesValues: List[(String, Value)]): Unit = {
    for ((n, v) <- namesValues) {
      defineVar(n, v)
    }
  }

  def defineClazz(clazzName: String, clazzDef: Clazz[_]): Unit = {
    if (isUnusedClazz(clazzName)) {
      scopes.head.defineClazz(clazzName, clazzDef)
    } else {
      throw new Error(
        "Clazz with name '" + clazzName + "' already defined in this scope")
    }
  }

  def setVar(varName: String, value: Value): Unit = scopes match {
    case Nil => throw new Error(
      "variable with name '" + varName + "' not in scope")
    case h :: t => if (h containsVar varName) {
      h.setVar(varName, value)
    } else {
      Context(t).setVar(varName, value) //TODO: code smell...
    }
  }

  def lookupClazz(clazzName: String): Clazz[_] = scopes match {
    case Nil => throw new Error("Clazz " + clazzName + " not found")
    case h :: t => if (h.containsClazz(clazzName)) {
      h.lookupClazz(clazzName)
    } else {
      Context(t).lookupClazz(clazzName)
    }
  }

  def enterBlock: Context = Context(Scope.empty :: scopes)
  def exitBlock: Context = Context(scopes.tail)
}

object Context {
  def empty = Context(List(Scope.empty))
}

/******************************************************************************\
|                           Intermediate Representation                        |
\******************************************************************************/

sealed trait Expr

/* atomic variables, constants,
 * basic arithmetic and boolean operations form expressions */
case class IntConstant(i: Int) extends Expr
case class BooleanConstant(b: Boolean) extends Expr
case object UnitConstant extends Expr
case class StringConstant(s: String) extends Expr
case class CharConstant(c: Char) extends Expr
case class Var(varName: String) extends Expr
case class Binop(a: Expr, b: Expr, op: (Value, Value) => Value,
  symb: String) extends Expr {
  override def toString = "Binop(" + a + "," + symb + "," + b + ")"
}
case class Comparison(a: Expr, b: Expr, cmp: (Value, Value) => BooleanValue,
  symb: String) extends Expr
case class ShortCircuitOr(a: Expr, b: Expr) extends Expr
case class ShortCircuitAnd(a: Expr, b: Expr) extends Expr

/* Declarations are special, because they must be enclosed in a
 * block to delimit their scope.
 */
sealed trait Scoped
case class ScopedExpression(e: Expr) extends Scoped
case class VariableDeclaration(varName: String) extends Scoped
case class VariableDeclInit(varName: String, rhs: Expr) extends Scoped
/* Function declarations treated separately, because they might be recursive */
case class FunctionDefinition(
  name: String, args: List[String], body: Expr) extends Scoped
case class BuiltInClassUsage(fullClassName: String) extends Scoped
case class Block(body: List[Scoped]) extends Expr


/* Assignments that mutate variables are also expressions.
 * Treating assignments as expressions is not that uncommon.
 * In `C`, assignments return the value of the RHS. In Scala, they return Unit.
 */
case class Assignment(varName: String, rhs: Expr) extends Expr

/* Some basic control flow */
case class While(cond: Expr, body: Expr) extends Expr
case class If(cond: Expr, t: Expr, e: Expr) extends Expr

/* Function application */
case class Apply(f: Expr, args: List[Expr]) extends Expr

/* Members of a class definition */
sealed trait Member
case class MethodDefinition(name: String, argNames: List[String], body: Expr)
  extends Member
case class MemberVarDecl(name: String) extends Member
case class ConstructorDefinition(argNames: List[String], body: Expr)
  extends Member
case class ClazzDefinition(
  name: String,
  memberVars: List[MemberVarDecl],
  constructors: List[ConstructorDefinition],
  methods: List[MethodDefinition]
) extends Scoped

/* Object instantiation, method call, member access */
case class New(className: String, args: List[Expr]) extends Expr
case class MethodCall(receiver: Expr, methodName: String, args: List[Expr])
  extends Expr
case class MemberUpdate(receiver: Expr, memberName: String, rhs: Expr)
  extends Expr
case class MemberAccess(receiver: Expr, memberName: String) extends Expr

/******************************************************************************\
|                                   Evaluation                                 |
\******************************************************************************/

object Semantics {
  /** Evaluates expression in a context, mutates the context */
  def eval(expr: Expr, ctx: Context): Value = expr match {
    case Var(name) => ctx.lookupVar(name)
    case Assignment(name, rhs) => {
      val v = eval(rhs, ctx)
      ctx.setVar(name, v)
      v
    }
    case IntConstant(i) => IntValue(i)
    case BooleanConstant(b) => BooleanValue(b)
    case UnitConstant => UnitValue
    case CharConstant(c) => CharValue(c)
    case StringConstant(s) => JvmObject(s)
    case Binop(a, b, op, symb) => {
      val av = eval(a, ctx)
      val bv = eval(b, ctx)
      op(av, bv)
    }
    case Comparison(a, b, cmp, symb) => {
      val av = eval(a, ctx)
      val bv = eval(b, ctx)
      cmp(av, bv)
    }
    case ShortCircuitOr(a, b) => {
      val av = eval(a, ctx)
      av match {
        case BooleanValue(v) => if (v) BooleanValue(true) else eval(b, ctx)
        case sthElse => throw new Error("Operand of || not boolean")
      }
    }
    case ShortCircuitAnd(a, b) => {
      val av = eval(a, ctx)
      av match {
        case BooleanValue(v) => if (!v) BooleanValue(false) else eval(b, ctx)
        case sthElse => throw new Error("Operand of && not boolean")
      }
    }
    case Block(enclosed) => evalBlock(enclosed, ctx.enterBlock, UnitValue)

    case w @ While(cond, body) => eval(cond, ctx) match {
      case BooleanValue(runBody) => if (runBody) {
        eval(body, ctx)
        eval(w, ctx)
      } else {
        UnitValue
      }
      case sthElse => throw new Error(
        "Expected boolean value in condition of while-loop, but got " + sthElse)
    }

    case If(cond, t, e) => eval(cond, ctx) match {
      case BooleanValue(runTrue) => if (runTrue) {
        eval(t, ctx)
      } else {
        eval(e, ctx)
      }
      case sthElse => throw new Error(
        "Expected boolean value in condition of if-else, but got " + sthElse)
    }

    case Apply(f, xs) => {
      val vf = eval(f, ctx)
      val argValues = evalArgs(xs, ctx)
      vf match {
        case Closure(args, body, closureCtx) => {
          if (args.size == argValues.size) {
            val enteredClosureCtx = closureCtx.enterBlock
            enteredClosureCtx.defineAll(args zip argValues)
            eval(body, enteredClosureCtx)
          } else {
            throw new Error("Wrong number of arguments")
          }
        }
        case BuiltInFunction(f) => f(argValues)
        case sthElse => throw new Error(sthElse + " is not applicable")
      }
    }

    case MethodCall(rcv, methName, args) => {
      val rcvValue = eval(rcv, ctx)
      val argValues = evalArgs(args, ctx)
      rcvValue match {
        case obj: Obj => obj.invokeMethod(methName, argValues)
        case sthElse => throw new Error(
          "Cannot invoke method " + methName + " on a non-object value")
      }
    }

    case New(clazzName, args) => {
      val cls = ctx.lookupClazz(clazzName)
      val cons = cls.getConstructor(ConsSig(args.size))
      val argValues = evalArgs(args, ctx)
      cons.invoke(argValues)
    }

    case MemberUpdate(rcv, memName, rhs) => {
      val obj = eval(rcv, ctx)
      val v = eval(rhs, ctx)
      obj match {
        case o: Obj => {
          o.set(memName, v)
          UnitValue
        }
        case sthElse =>
          throw new Error("Cannot update member of a non-object entity.")
      }
    }

    case MemberAccess(rcv, memName) => {
      val obj = eval(rcv, ctx)
      obj match {
        case o: Obj => o.get(memName)
        case sthElse =>
          throw new Error("Cannot access member of a non-object entity.")
      }
    }
  }

  def evalArgs(args: List[Expr], ctx: Context): List[Value] = {
    for (a <- args) yield eval(a, ctx)
  }

  // TODO: why processing `tail` every time? Unnecessary.
  def evalBlock(block: List[Scoped], ctx: Context, lastValue: Value)
  : Value = block match {
    case Nil => lastValue
    case VariableDeclaration(n) :: tail => if (ctx.isUnusedVar(n)) {
      ctx.defineVar(n, Undefined)
      evalBlock(tail, ctx, UnitValue)
    } else {
      throw new Error(
        "Variable '" + n + "' already defined in this scope: " + ctx)
    }
    case VariableDeclInit(n, rhs) :: tail => if (ctx.isUnusedVar(n)) {
      ctx.defineVar(n, Undefined)
      val r = eval(rhs, ctx)
      ctx.setVar(n, r)
      evalBlock(tail, ctx, UnitValue)
    } else {
      throw new Error(
        "Variable '" + n + "' already defined in this scope: " + ctx)
    }
    case ScopedExpression(e) :: tail => {
      val v = eval(e, ctx)
      evalBlock(tail, ctx, v)
    }
    case FunctionDefinition(n, args, body) :: tail => if (ctx.isUnusedVar(n)) {
      ctx.defineVar(n, Closure(args, body, ctx))
      evalBlock(tail, ctx, UnitValue)
    } else {
      throw new Error("The name '" + n +
        "' is already in use in current scope.")
    }
    case ClazzDefinition(n, membs, conss, meths) :: tail => {
      if (ctx.isUnusedClazz(n)) {
        val constructors = (for (c <- conss) yield {
          val consSig = ConsSig(c.argNames.size)
          (consSig, c)
        }).toMap
        val methods = (for (m <- meths) yield {
          val methSig = MethSig(m.name, m.argNames.size)
          (methSig, m)
        }).toMap
        val dataOffsets = (membs.map(_.name).sorted zip Stream.from(0)).toMap
        ctx.defineClazz(n,
          UserClazz(n, constructors, methods, dataOffsets, ctx))
        evalBlock(tail, ctx, UnitValue)
      } else {
        throw new Error(
          "Class with name " + n + " already defined in this scope")
      }
    }
    case BuiltInClassUsage(fullClassName) :: tail => {
      val simpleName = fullClassName.split("\\.").last
      if (ctx.isUnusedClazz(simpleName)) {
        val c = Class.forName(fullClassName)
        ctx.defineClazz(simpleName, JvmClazz(c))
        ctx.defineVar(simpleName, JvmClazzObject(c))
        evalBlock(tail, ctx, UnitValue)
      } else {
        throw new Error("Class with name " + simpleName + " already defined")
      }
    }
  }
}

/******************************************************************************\
|                                   Parser                                     |
\******************************************************************************/
import scala.util.parsing.combinator.{JavaTokenParsers}
import scala.util.{Either, Left, Right}

/* A purely syntactical thingie that can "come after a dot".
 * Either a method call ".methodName(arg1,...,argN)", or a
 * member variable access ".memberVar"
 *
 * On it's own, it's not an expression or declaration.
 */
sealed trait DotMethOrVar
case class DotMeth(name: String, args: List[Expr]) extends DotMethOrVar
case class DotVar(name: String) extends DotMethOrVar
case class DotChain(receiver: Expr, dotThingies: List[DotMethOrVar])

sealed trait LeftValue
case class LeftVar(name: String) extends LeftValue
case class LeftMemberVar(obj: Expr, varName: String) extends LeftValue
/**
 * Rough plan of precedence rules for this parser:
 *   assignment: =
 *   logical disjunctions: ||
 *   logical conjunctions: &&
 *   comparisons: == < > <= >=
 *   arithmetic sums: + -
 *   arithmetic products: * /
 *   dot-separated method-var-gizmos: blah.blup(x,y,z).foo(bar).baz.k.l.m
 *   function or method invocation: f(x,y,z)
 *   atoms (literals, identifiers, parenthesized expressions)
 */
object LanguageParser extends JavaTokenParsers {
  import java.lang.{Error => JError}

  /* Helper methods for building binary operations. Not really clean,
   * the binop's have some semantics inside...
   */
  def intBinopCons(op: (Int, Int) => Int, symb: String)(x: Expr, y: Expr)
  : Expr = {
    Binop(x, y, {
      case (IntValue(x), IntValue(y)) => IntValue(op(x, y))
      case sthElse => throw new JError(
        "binop '" + symb + "' not applicable to " + sthElse + "."
      )
    }, symb)
  }

  val Add = (x: Expr, y: Expr) => {
    Binop(x, y, {
      case (IntValue(x), IntValue(y)) => IntValue(x + y)
      case (JvmObject(x: String), y) => JvmObject(x + y)
      case (x, JvmObject(y: String)) => JvmObject(x + y)
      case sthElse => throw new JError("Cannot apply '+' to " + sthElse)
    }, "+")
  }
  val Sub = intBinopCons(_ - _, "-")(_, _)
  val Mul = intBinopCons(_ * _, "*")(_, _)
  val Div = intBinopCons(_ / _, "/")(_, _)

  def Eq(x: Expr, y: Expr): Expr =
    Comparison(x, y, (x, y) => BooleanValue(x == y), "==")
  def Neq(x: Expr, y: Expr): Expr =
    Comparison(x, y, (x, y) => BooleanValue(x != y), "!=")

  def intComparCons(op: (Int, Int) => Boolean, symb: String)(x: Expr, y: Expr) =
    Comparison(x, y, {
      case (IntValue(x), IntValue(y)) => BooleanValue(op(x, y))
      case sthElse => throw new JError(
        symb + " not applicable to " + x + " " + y)
    }, "<")

  val Le = intComparCons(_ < _, "<")(_, _)
  val Ge = intComparCons(_ > _, ">")(_, _)
  val Leq = intComparCons(_ <= _, "<=")(_, _)
  val Geq = intComparCons(_ >= _, ">=")(_, _)

  /** Transforms chains like "obj.f1(x1).m2.f3(x3).m4"
    * into expressions.
    *
    * Notice: "thingies after a dot" is a purely syntactical concept, which
    * does not have any meaning on it's own, whereas an expression can be
    * assigned a semantic meaning in a context.
    */
  def dotChainToExpr(rcv: Expr, dotChain: List[DotMethOrVar]): Expr = {
    dotChain match {
      case DotVar(v) :: tail => dotChainToExpr(MemberAccess(rcv, v), tail)
      case DotMeth(name, args) :: tail => dotChainToExpr(
        MethodCall(rcv, name, args), tail
      )
      case Nil => rcv
    }
  }

  def dotChainToExpr(dc: DotChain): Expr = {
    dotChainToExpr(dc.receiver, dc.dotThingies)
  }

  def applyChainToExpr(f: Expr, argLists: List[List[Expr]]): Expr = {
    argLists match {
      case Nil => f
      case h :: t => applyChainToExpr(Apply(f, h), t)
    }
  }

  def id: Parser[String] =
    "[_a-zA-Z][_a-zA-Z0-9]*".r | failure("expected id")
  def variable: Parser[Expr] = id ^^ { case v => Var(v) }
  def unitLiteral: Parser[Expr] = "()" ^^^ UnitConstant
  def intLiteral: Parser[Expr] = "([1-9][0-9]*)|0".r ^^ {
    case s => IntConstant(s.toInt)
  }
  def stringConstant: Parser[Expr] =
    """"([^"\\]|(\\")|(\\n)|(\\t)|(\\\\))*"""".r ^^ {
    case s => {
      val escaped = s.substring(1, s.size - 1)
      .replaceAll("\\\\\"", "\"")
      .replaceAll("\\\\n", "\n")
      .replaceAll("\\\\t", "\t")
      .replaceAll("\\\\\\\\", "\\\\")
      // in second part `\` must be escaped twice too!
      StringConstant(escaped)
    }
  }
  def boolLiteral: Parser[Expr] = "(true)|(false)".r ^^ {
    case "true" => BooleanConstant(true)
    case "false" => BooleanConstant(false)
  }
  def instantiation: Parser[New] =
    (("new" ~> id) ~ ("(" ~> repsep(expr, ",") <~ ")")) ^^ {
      case (className ~ args) => New(className, args)
    }
  def atomicExpr: Parser[Expr] =
    unitLiteral | intLiteral | boolLiteral | stringConstant |
    instantiation | variable | "(" ~> expr <~ ")" | block

  def applyFunc: Parser[Expr] = atomicExpr
  def applyArgs: Parser[List[Expr]] = "(" ~> repsep(expr, ",") <~ ")"
  def applyExpr: Parser[Expr] = (applyFunc ~ rep(applyArgs)) ^^ {
    case (f ~ xss) => applyChainToExpr(f, xss)
  }

  def dotReceiver: Parser[Expr] = applyExpr
  def dotMeth: Parser[DotMeth] = (id ~ ("(" ~> repsep(expr, ",") <~ ")")) ^^ {
    case (name ~ args) => DotMeth(name, args)
  }
  def dotVar: Parser[DotVar] = id ^^ { DotVar(_) }
  def dotMethOrVar: Parser[DotMethOrVar] = dotMeth | dotVar
  def dotChain: Parser[DotChain] = (dotReceiver ~ ("." ~> dotMethOrVar).*) ^^ {
    case (rcv ~ tail) => DotChain(rcv, tail)
  }
  def dotExpr: Parser[Expr] = dotChain ^^ { dotChainToExpr(_) }

  def arithAtom: Parser[Expr] = dotExpr
  def arithProd: Parser[Expr] =
    (arithAtom ~ (("*" ~ arithProd) | ("/" ~ arithAtom)).*).map{
      case t~list => {
        list.foldLeft(t) {
          case (x, (op~y)) => op match {
            case "*" => Mul(x, y)
            case "/" => Div(x, y)
          }
        }
      }
    }
  def arithSum: Parser[Expr] =
    (arithProd ~ (("+" ~ arithProd) | ("-" ~ arithProd)).*).map{
      case t~list => {
        list.foldLeft(t) {
          case (x, (op~y)) => op match {
            case "+" => Add(x, y)
            case "-" => Sub(x, y)
          }
        }
      }
    }
  def arithExpr: Parser[Expr] = arithSum

  def comparAtom: Parser[Expr] = arithExpr
  def comparison: Parser[Expr] =
    (comparAtom ~ "([<>]=?)|(==)|(!=)".r ~ comparAtom) ^^ {
      case (x ~ op ~ y) => op match {
        case "==" => Eq(x, y)
        case "!=" => Neq(x, y)
        case ">" => Ge(x, y)
        case "<" => Le(x, y)
        case ">=" => Geq(x, y)
        case "<=" => Geq(x, y)
      }
    }
  def comparExpr = comparison | comparAtom

  def logicAtom: Parser[Expr] = comparExpr
  def logicConj: Parser[Expr] = rep1sep(logicAtom, "&&") ^^ {
    case cs => cs.reduce{(x, y) => ShortCircuitAnd(x, y)}
  }
  def logicDisj: Parser[Expr] = rep1sep(logicConj, "||") ^^ {
    case as => as.reduce{(x, y) => ShortCircuitOr(x, y)}
  }
  def logicExpr: Parser[Expr] = logicDisj

  def varDeclInit: Parser[VariableDeclInit] =
    (("var" ~> id <~ "=") ~ (expr <~ ";")) ^^ {
      case (varName ~ rhs) => VariableDeclInit(varName, rhs)
    }

  def varDecl: Parser[VariableDeclaration] = ("var" ~> id <~ ";") ^^ {
    case varName => VariableDeclaration(varName)
  }

  def leftValue: Parser[LeftValue] = dotChain ^? {
    case DotChain(Var(n), Nil) => LeftVar(n)
    // all the nasty checks are necessary to ensure that the chain of dots
    // ends with an assignable member variable; We must keep format of
    // a partial function.
    case DotChain(rcv, tail) if !tail.isEmpty && (
      tail.last match {
        case DotVar(n) => true
        case _ => false
      }
    ) => {
      val DotVar(n) = tail.last
      LeftMemberVar(dotChainToExpr(rcv, tail.dropRight(1)), n)
    }
  }

  def assignment: Parser[Expr] = ((leftValue <~ "=") ~ expr) ^^ {
    case (lval ~ rhs) => lval match {
      case LeftVar(v) => Assignment(v, rhs)
      case LeftMemberVar(obj, name) => MemberUpdate(obj, name, rhs)
    }
  }

  def expr: Parser[Expr] = ifElse | whileLoop | assignment | logicExpr
  def scopedExpr: Parser[ScopedExpression] = (expr <~ ";") ^^ {
    case e => ScopedExpression(e)
  }
  def ifElse: Parser[If] =
    (("if" ~> "(" ~> expr <~ ")") ~ block ~ ("else" ~> block)) ^^ {
      case ((cond ~ t) ~ e) => If(cond, t, e)
    }
  def whileLoop: Parser[While] =
    (("while" ~> "(" ~> expr <~ ")") ~ block) ^^ {
      case (cond ~ body) => While(cond, body)
    }
  def funcDef: Parser[FunctionDefinition] =
    "def" ~> id ~ ("(" ~> repsep(id, ",") <~ ")") ~ block ^^ {
      case ((fname ~ args) ~ body) => FunctionDefinition(fname, args, body)
    }
  def classDef: Parser[ClazzDefinition] =
    ("class" ~> id ~ ("{" ~> rep(member) <~ "}")) ^^ {
      case (name ~ members) =>
      val declaredVars = for (m @ MemberVarDecl(n) <- members) yield m
      val constructors = for (m @ ConstructorDefinition(_,_) <- members) yield m
      val methods = for(m @ MethodDefinition(_,_,_) <- members) yield m
      ClazzDefinition(name, declaredVars, constructors, methods)
    }
  def builtInClassUsage: Parser[BuiltInClassUsage] =
    ("using" ~> repsep(id, ".") <~ ";") ^^ {
      case names => BuiltInClassUsage(names.mkString("."))
    }

  def memberVarDecl: Parser[MemberVarDecl] = ("var" ~> id <~ ";") ^^ {
    case name => MemberVarDecl(name)
  }
  def consDef: Parser[ConstructorDefinition] =
    (("this" ~> "(" ~> repsep(id, ",") <~ ")") ~ block) ^^ {
      case (args ~ body) => ConstructorDefinition(args, body)
    }
  def methDef: Parser[MethodDefinition] =
    (("def" ~> id) ~ ("(" ~> repsep(id, ",") <~ ")") ~ block) ^^ {
      case (name ~ args ~ body) => MethodDefinition(name, args, body)
    }
  def member = memberVarDecl | consDef | methDef
  def noSemicolonScopedExpr: Parser[Scoped] =
    ((ifElse | whileLoop) ^^ { case e => ScopedExpression(e) }) |
    funcDef | classDef | builtInClassUsage

  def scoped: Parser[Scoped] =
    noSemicolonScopedExpr | varDeclInit | varDecl | scopedExpr
  def block: Parser[Block] = ("{" ~> rep(scoped) <~ "}") ^^ {
    case bs => Block(bs)
  }

  def parseCode(codeWithComments: String): Either[String, Expr] = {
    val s = codeWithComments
      .split("\n")
      .filterNot(_.trim.startsWith("//"))
      .mkString("\n")

    parseAll(rep(scoped), s) match {
      case Success(t, _) => Right(Block(t))
      case f => Left(f.toString)
    }
  }
}

/******************************************************************************\
|              Defining predefined Built-Ins, running.                         |
\******************************************************************************/
object Main {
  val PredefContext = {
    val c = Context.empty
    c.defineVar("println", BuiltInFunction { args =>
      for (a <- args) {
        a match {
          case IntValue(i) => System.out.print(i)
          case CharValue(c) => System.out.print(c)
          case BooleanValue(b) => System.out.print(b)
          case UnitValue => System.out.print("()")
          case Undefined => System.out.print("undefined")
          case Closure(_, _, _) => System.out.print("<closure>")
          case BuiltInFunction(f) => System.out.print("<builtin-function>")
          case sthElse => System.out.print(sthElse)
        }
      }
      System.out.println()
      UnitValue
    })
    c
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage:")
      println("liuoofxty <sourceCodeFile>")
    } else {
      val p = scala.io.Source.fromFile(args(0)).getLines.mkString("\n")
      LanguageParser.parseCode(p) match {
        case Right(parsed) => Semantics.eval(parsed, PredefContext)
        case Left(f) => println(f)
      }
    }
  }

  /** Runs a single file script, redirects standard output into a
    * buffer, returns the result as string.
    *
    * This should simplify testing somewhat.
    */
  def runCaptureStdout(filePath: String): Either[String, String] = {
    val b = new java.io.ByteArrayOutputStream()
    val o = new java.io.PrintStream(b)
    val out = System.out
    val p = scala.io.Source.fromFile(filePath).getLines.mkString("\n")
    for {
      parsed <- LanguageParser.parseCode(p)
    } yield {
      System.setOut(o)
      Semantics.eval(parsed, PredefContext)
      System.setOut(out)
      b.toString
    }
  }
}
