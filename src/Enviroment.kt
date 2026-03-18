class Environment(val parent: Environment? = null) {
	private val vars = mutableMapOf<String, Value>()
	fun define(name: String, value: Value) { vars[name] = value }
	fun get(name: String): Value = vars[name] ?: parent?.get(name) ?: error("Undefined variable '$name'")
	fun set(name: String, value: Value): Value {
		if (vars.containsKey(name)) {
			vars[name] = value
			return value
		}
		return parent?.set(name, value) ?: error("Undefined variable '$name'")
	}
	
	fun toNative(value: Value): Any? = when (value) {
		is Value.Num -> value.value
		is Value.Str -> value.value
		is Value.Bool -> value.value
		is Value.Nil -> null
		is Value.Array -> value.members
		is Value.Dict -> value.members
		else -> "UNKNOWN"
	}
	
	fun isTruthy(value: Value?) = when (value) {
		is Value.Nil -> false
		is Value.Bool -> value.value
		is Value.Num -> value.value != 0.0
		else -> true
	}
	
	fun synxToString(value: Value): Value.Str {
		return Value.Str(when (value) {
			is Value.Num -> value.value.toString()
			is Value.Str -> value.value
			is Value.Bool -> value.value.toString()
			is Value.Nil -> "nil"
			is Value.Function -> "Function"
			is Value.NativeFunction -> "NativeFunction"
			is Value.Array -> {
				val elements = value.members.joinToString(", ") {
					synxToString(it).value
				}
				"[$elements]"
			}
			is Value.Dict -> {
				val entries = value.members.entries.joinToString(", ") { (k, v) ->
					"${synxToString(k).value}: ${synxToString(v).value}"
				}
				"{$entries}"
			}
		})
	}
	
	fun loadDefaultEnv(): Environment {
		define("toStr", Value.NativeFunction { args -> synxToString(args[0]) })
		
		define("error", Value.NativeFunction { args ->
			val msg = toNative(args[0]).toString()
			throw RuntimeException(msg)
		})
		
		define("array", Value.Dict(mutableMapOf(
			Value.Str("new") to Value.NativeFunction { args ->
				val arr: MutableList<Value> = mutableListOf()
				
				args.forEach { arr.add(it) }
				
				Value.Array(arr)
			},
			
			Value.Str("add") to Value.NativeFunction { args ->
				if (args.size < 3) {
					throw RuntimeException("array.add expects (array, index, value)")
				}
				
				val arr = args[0]
				if (arr !is Value.Array) {
					throw RuntimeException("First argument must be an array")
				}
				
				val index = args[1]
				val value = args[2]
				
				val i = (index as? Value.Num)?.value?.toInt()
					?: throw RuntimeException("Index must be a number")
				
				arr.members[i] = value
				Value.Nil
			},
			
			Value.Str("remove") to Value.NativeFunction { args ->
				if (args.size < 2) {
					throw RuntimeException("array.remove expects (array, index)")
				}
				
				val arr = args[0]
				if (arr !is Value.Array) {
					throw RuntimeException("First argument must be an array")
				}
				
				val index = args[1]
				
				val i = (index as? Value.Num)?.value?.toInt()
					?: throw RuntimeException("Index must be a number")

				val value = arr.members[i]
				arr.members.removeAt(i)
				value
			},
			
			Value.Str("push") to Value.NativeFunction { args ->
				if (args.size < 2) {
					throw RuntimeException("array.push expects (array, value)")
				}
				val arr = args[0]
				if (arr !is Value.Array) {
					throw RuntimeException("First argument must be an array")
				}
				args.drop(1).forEach { arr.members.add(it) }
				arr
			},
			
			Value.Str("pop") to Value.NativeFunction { args ->
				if (args.isEmpty()) {
					throw RuntimeException("array.pop expects (array)")
				}
				val arr = args[0]
				if (arr !is Value.Array) {
					throw RuntimeException("First argument must be an array")
				}
				arr.members.removeLast()
				arr
			}
		)))
		
		define("dict", Value.Dict(mutableMapOf(
			Value.Str("new") to Value.NativeFunction { args ->
				val dict: MutableMap<Value, Value> = mutableMapOf()
				
				for (i in 0..<args.size step 2) {
					if (i + 1 >= args.size) {
						throw RuntimeException("Dict: Odd number of arguments")
					}
					dict[args[i]] = args[i + 1]
				}
				
				Value.Dict(dict)
			},
			
			Value.Str("add") to Value.NativeFunction { args ->
				if (args.size < 3) {
					throw RuntimeException("dict.add expects (dict, key, value)")
				}
				
				val dictVal = args[0]
				if (dictVal !is Value.Dict) {
					throw RuntimeException("First argument must be a dict")
				}
				
				val key = args[1]
				val value = args[2]
				
				dictVal.members[key] = value
				Value.Nil
			},
			
			Value.Str("remove") to Value.NativeFunction { args ->
				if (args.size < 2) {
					throw RuntimeException("dict.remove expects (dict, key)")
				}
				
				val dictVal = args[0]
				if (dictVal !is Value.Dict) {
					throw RuntimeException("First argument must be a dict")
				}
				
				val key = args[1]
				val value = dictVal.members[key]
				dictVal.members.remove(key)
				value ?: Value.Nil
			}
			
		)))
		
		define("len", Value.NativeFunction {args ->
			when (val arg = toNative(args[0])) {
				is List<*> -> Value.Num(arg.size.toDouble())
				is Map<*, *> -> Value.Num(arg.size.toDouble())
				is String -> Value.Num(arg.length.toDouble())
				else -> Value.Nil
			}
		})
		
		define("io", Value.Dict(mutableMapOf(
			Value.Str("write") to Value.NativeFunction { args ->
				for (arg in args) {
					print("${toNative(synxToString(arg))} ")
				}
				Value.Nil
			},
			Value.Str("writeln") to Value.NativeFunction { args ->
				for (arg in args) {
					print("${toNative(synxToString(arg))} ")
				}
				println()
				Value.Nil
			},
			
			Value.Str("read") to Value.NativeFunction {
				Value.Str(readln())
			}
		)))
		
		define("math", Value.Dict(mutableMapOf(
			Value.Str("abs") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.abs expects a number")
				Value.Num(kotlin.math.abs(n.value))
			},
			Value.Str("sqrt") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.sqrt expects a number")
				Value.Num(kotlin.math.sqrt(n.value))
			},
			Value.Str("sin") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.sin expects a number")
				Value.Num(kotlin.math.sin(n.value))
			},
			Value.Str("cos") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.cos expects a number")
				Value.Num(kotlin.math.cos(n.value))
			},
			Value.Str("tan") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.tan expects a number")
				Value.Num(kotlin.math.tan(n.value))
			},
			Value.Str("log") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.log expects a number")
				Value.Num(kotlin.math.ln(n.value))
			},
			Value.Str("exp") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.exp expects a number")
				Value.Num(kotlin.math.exp(n.value))
			},
			Value.Str("floor") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.floor expects a number")
				Value.Num(kotlin.math.floor(n.value))
			},
			Value.Str("ceil") to Value.NativeFunction { args ->
				val n = args.getOrNull(0) as? Value.Num ?: throw RuntimeException("math.ceil expects a number")
				Value.Num(kotlin.math.ceil(n.value))
			},
			Value.Str("max") to Value.NativeFunction { args ->
				val nums = args.map { (it as? Value.Num)?.value ?: throw RuntimeException("math.max expects numbers") }
				Value.Num(nums.maxOrNull() ?: 0.0)
			},
			Value.Str("min") to Value.NativeFunction { args ->
				val nums = args.map { (it as? Value.Num)?.value ?: throw RuntimeException("math.min expects numbers") }
				Value.Num(nums.minOrNull() ?: 0.0)
			},
			Value.Str("pi") to Value.Num(kotlin.math.PI),
			Value.Str("e") to Value.Num(kotlin.math.E)
		)))
		return this
	}
}

fun main() {
	val env = Environment()
	env.loadDefaultEnv()
}