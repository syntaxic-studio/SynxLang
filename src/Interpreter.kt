import java.io.File
import kotlin.math.pow

class ReturnException(val value: Value?): Throwable()
class ContinueException: Throwable()
class BreakException: Throwable()
sealed class Value {
	data class Num(var value: Double): Value()
	data class Str(val value: String): Value()
	data class Bool(val value: Boolean): Value()
	
	data class Array(val members: MutableList<Value>): Value()
	data class Dict(val members: MutableMap<Value, Value>): Value()
	
	object Nil: Value()
	class Function(val decl: Stmt.FunDecl, val closure: Environment): Value()
	class NativeFunction(val body: (List<Value>) -> Value): Value()
}

class Interpreter(var env: Environment = Environment().loadDefaultEnv()) {
	fun eval(expr: Expr): Value {
		return when (expr) {
			
			is Expr.Nil -> Value.Nil
			is Expr.Str -> Value.Str(expr.value)
			is Expr.Num -> Value.Num(expr.value)
			is Expr.Bool -> Value.Bool(expr.value)
			is Expr.Var -> env.get(expr.name)
			is Expr.Assign -> env.set(expr.name, eval(expr.value))
			is Expr.IndexAssign -> {
				val collection = eval(expr.collection)
				val index = eval(expr.index)
				val value = eval(expr.value)
				
				when (collection) {
					is Value.Array -> {
						val i = (index as? Value.Num)?.value?.toInt() ?: error("Invalid array index")
						collection.members[i] = value
						value
					}
					is Value.Dict -> {
						collection.members[eval(expr.index)] = value
						value
					}
					is Value.Str -> error("Cannot assign to string index")
					else -> error("Cannot index assign to ${collection.javaClass.simpleName}")
				}
			}
			is Expr.Unary -> {
				val subject = eval(expr.expr)
				val op = expr.op
				when (subject) {
					is Value.Num -> when (op) {
						UnaryOp.NEG -> Value.Num(-subject.value)
						UnaryOp.NOT -> Value.Bool(!env.isTruthy(subject))
					}
					is Value.Bool -> when (op) {
						UnaryOp.NOT -> Value.Bool(!env.isTruthy(subject))
						else -> Value.Nil
					}
					else -> Value.Nil
				}
			}
			is Expr.Binary -> {
				val op = expr.op
				if (op == BinaryOp.AND) {
					return if (!env.isTruthy(eval(expr.left))) Value.Bool(false)
					else Value.Bool(env.isTruthy(eval(expr.right)))
				}
				if (op == BinaryOp.OR) {
					return if (env.isTruthy(eval(expr.left))) Value.Bool(true)
					else Value.Bool(env.isTruthy(eval(expr.right)))
				}
				
				val left = eval(expr.left)
				val right = eval(expr.right)
				when (left) {
					is Value.Num if right is Value.Num -> when (op) {
						BinaryOp.ADD -> Value.Num(left.value + right.value)
						BinaryOp.SUB -> Value.Num(left.value - right.value)
						BinaryOp.MUL -> Value.Num(left.value * right.value)
						BinaryOp.DIV -> Value.Num(left.value / right.value)
						BinaryOp.MOD -> Value.Num(left.value % right.value)
						BinaryOp.EXP -> Value.Num(left.value.pow(right.value))
						BinaryOp.EQ -> Value.Bool(left.value == right.value)
						BinaryOp.NEQ -> Value.Bool(left.value != right.value)
						BinaryOp.LT -> Value.Bool(left.value < right.value)
						BinaryOp.LTE -> Value.Bool(left.value <= right.value)
						BinaryOp.GT -> Value.Bool(left.value > right.value)
						BinaryOp.GTE -> Value.Bool(left.value >= right.value)
                        else -> Value.Nil
                    }
					
					is Value.Str if right is Value.Num -> when (op) {
						BinaryOp.MUL -> Value.Str(left.value.repeat(right.value.toInt()))
						else -> Value.Nil
					}
					
					is Value.Str if right is Value.Str -> when (op) {
						BinaryOp.ADD -> Value.Str(left.value + right.value)
						else -> Value.Nil
					}
					
					else -> Value.Nil
				}
			}
			is Expr.Call -> {
				val callee = eval(expr.callee)
				val args = expr.args.map { eval(it) }
				when (callee) {
					is Value.NativeFunction -> {
						callee.body(args)
					}
					is Value.Function -> {
						val callEnv = Environment(callee.closure)
						callee.decl.params.forEachIndexed { index, name ->
							val value = args.getOrNull(index) ?: Value.Nil
							callEnv.define(name, value)
						}
						val previous = env
						env = callEnv
						try {
							callee.decl.body.stmts.forEach { execute(listOf(it)) }
							Value.Nil
						} catch (e: ReturnException) {
							e.value ?: Value.Nil
						} finally {
							env = previous
						}
					}
					else -> error("Cannot call ${callee.javaClass.canonicalName}")
				}
			}
			is Expr.Index -> {
				val collection = eval(expr.collection)
				val index = eval(expr.index)
				when (collection) {
					is Value.Array -> {
						val i = (index as? Value.Num)?.value?.toInt() ?: error("Invalid array index. Enter int")
						collection.members.getOrElse(i) { Value.Nil }
					}
					
					is Value.Str -> {
						val i = (index as? Value.Num)?.value?.toInt() ?: error("Invalid string index.")
						Value.Str(collection.value[i].toString())
					}
					
					is Value.Dict -> {
						collection.members[index] ?: Value.Nil
					}
					
					else -> error("Cannot index ${collection.javaClass.simpleName}")
				}
			}
		}
	}
	
	fun execute(ast: List<Stmt>) {
		for (stmt in ast) {
			when (stmt) {
				is Stmt.Expression -> eval(stmt.expr)
				is Stmt.VarDecl -> env.define(stmt.name, eval(stmt.value))
				is Stmt.FunDecl -> env.define(stmt.name, Value.Function(stmt, env))
				is Stmt.Return -> throw ReturnException(if (stmt.value != null) eval(stmt.value) else null)
				is Stmt.Break -> throw BreakException()
				is Stmt.Continue -> throw ContinueException()
				is Stmt.Block -> {
					val previous = env
					env = Environment(env)
					try {
						execute(stmt.stmts)
					} finally {
						env = previous
					}
				}
				is Stmt.If -> {
					if (env.isTruthy(eval(stmt.cond)))
						execute(stmt.then.stmts)
					else
						stmt.els?.let { execute(it.stmts) }
				}
				is Stmt.While -> {
					try {
						while (env.isTruthy(eval(stmt.cond))) {
							try {
								execute(stmt.body.stmts)
							} catch (_: ContinueException) {
								// ignore it since it continues loop
							}
						}
					} catch (_: BreakException) {
						// ignore it since it breaks loop
					}
				}
				
				is Stmt.For -> {
					val previous = env
					env = Environment(previous)
					execute(listOf(stmt.init))
					try {
						while (env.isTruthy(eval(stmt.cond))) {
							try {
								execute(stmt.body.stmts)
							} catch (_: ContinueException) {
								// ignore it continues loop
							}
							eval(stmt.step)
						}
					} catch (_: BreakException) {
						// ignore it since it exists loop
					} finally {
						env = previous
					}
				}
			}
		}
		
	}
}

fun main(args: Array<String>) {
	val env = Environment()
	env.loadDefaultEnv()
	val interpreter = Interpreter(env)

	if (args.isEmpty()) {
		println("Welcome to Synx REPL. Type 'exit' to quit.")
		while (true) {
			print("> ")
			val line = readln()
			if (line.lowercase() == "exit") break
			try {
				val lexer = Lexer(line)
				val tokens = lexer.tokenize()
				val parser = Parser(tokens)
				val ast = parser.parse()
				interpreter.execute(ast)
			} catch (e: Exception) {
				println("Error: ${e.message}")
			}
		}
	} else {
		val file = File(args[0]).readText()
		val lexer = Lexer(file)
		val tokens = lexer.tokenize()
		val parser = Parser(tokens)
		val ast = parser.parse()
		interpreter.execute(ast)
	}
}