class ParserError(message: String): RuntimeException(message)

enum class UnaryOp { NEG, NOT }
enum class BinaryOp { ADD, SUB, MUL, DIV, EXP, EQ, NEQ, LT, LTE, GT, GTE, AND, OR, MOD }

sealed class Expr {
    data class Num(val value: Double): Expr()
    data class Str(val value: String): Expr()
    
    data class Bool(val value: Boolean): Expr()
    object Nil: Expr()
    
    data class Var(val name: String): Expr()
    data class Assign(val name: String, val value: Expr): Expr()
    data class IndexAssign(val collection: Expr, val index: Expr, val value: Expr): Expr()

    data class Unary(val op: UnaryOp, val expr: Expr): Expr()
    data class Binary(val op: BinaryOp, val left: Expr, val right: Expr): Expr()

    data class Call(val callee: Expr, val args: List<Expr>): Expr()
    data class Index(val collection: Expr, val index: Expr): Expr()
}

sealed class Stmt {
    data class Expression(val expr: Expr): Stmt()

    data class VarDecl(val name: String, val value: Expr): Stmt()
    data class FunDecl(val name: String, val params: List<String>, val body: Block): Stmt()

    data class If(val cond: Expr, val then: Block, val els: Block?): Stmt()
    data class While(val cond: Expr, val body: Block) : Stmt()
    data class For(val init: Stmt, val cond: Expr, val step: Expr, val body: Block): Stmt()

    data class Block(val stmts: List<Stmt>): Stmt()
    data class Return(val value: Expr?): Stmt()
    
    object Break: Stmt()
    object Continue: Stmt()
}

class Parser(val tokens: List<Token>) {
    private var pos = 0
	
	// helpers
    private fun peek(): Token = tokens[pos]
    private fun previous(): Token = tokens[pos - 1]
    private fun advance(): Token = tokens[pos++]
    private fun isAtEnd(): Boolean = peek() is Token.EOF
    private fun consume(expected: Token): Token {
        if (peek() == expected) return advance()
        throw ParserError("Expected $expected but found ${peek()}")
    }
    private inline fun <reified T : Token> expect(): T {
        val token = advance()
        if (token is T) return token
        throw ParserError("Expected ${T::class.simpleName} but found $token")
    }
    private fun check(type: Token): Boolean = !isAtEnd() && peek() == type
    private fun match(vararg types: Token): Boolean {
        if (types.any { check(it) }) {
            advance()
            return true
        }
        return false
    }

    fun parse(): List<Stmt> {
        return parseProgram()
    }

    private fun parseProgram(): List<Stmt> {
        val stmts = mutableListOf<Stmt>()
        while (!isAtEnd()) {
            stmts += parseStatement()
        }

        return stmts
    }

    private fun parseStatement(): Stmt {
        return when (peek()) {
            is Token.LBrace -> parseBlock()
            is Token.If -> parseIf()
            is Token.While -> parseWhile()
            is Token.For -> parseFor()
            is Token.Var -> parseVarDecl()
            is Token.Func -> parseFunDecl()
            is Token.Return -> parseReturn()
            is Token.Break -> {advance(); Stmt.Break}
            is Token.Continue -> {advance(); Stmt.Continue}
            else -> Stmt.Expression(parseExpression())
        }
    }

    private fun parseIf(): Stmt.If {
        consume(Token.If)
        val cond = parseExpression()
        val then = parseBlock()
        val els: Stmt.Block? = if (match(Token.Else)) {
            parseBlock()
        } else {
            null
        }
        return Stmt.If(cond, then, els)
    }
    private fun parseFor(): Stmt.For {
        consume(Token.For)
        val init = if (check(Token.Var)) parseVarDecl() else Stmt.Expression(parseExpression())
        
        consume(Token.Semicolon)
        val cond = parseExpression()
        val step = if (match(Token.Semicolon)) parseExpression() else Expr.Nil
        val body = parseBlock()
        
        return Stmt.For(init, cond, step, body)
    }
    private fun parseWhile(): Stmt.While {
        consume(Token.While)
        val cond = parseExpression()
        val block = parseBlock()
        return Stmt.While(cond, block)
    }

    private fun parseBlock(): Stmt.Block {
        consume(Token.LBrace)
        val stmts = mutableListOf<Stmt>()
        while (!check(Token.RBrace) && !isAtEnd()) {
            stmts += parseStatement()
        }
        consume(Token.RBrace)
        return Stmt.Block(stmts)
    }
    
    private fun parseVarDecl(): Stmt.VarDecl {
        consume(Token.Var)
        val name = expect<Token.Ident>()
        consume(Token.Eq)
        val value = parseExpression()
        return Stmt.VarDecl(name.name, value)
    }

    private fun parseReturn(): Stmt.Return {
        consume(Token.Return)
        val value = if (check(Token.RBrace) || isAtEnd()) null else parseExpression()
        return Stmt.Return(value)
    }

    private fun parseFunDecl(): Stmt.FunDecl {
        consume(Token.Func)
        val name = expect<Token.Ident>()
        consume(Token.LParen)
        val params = mutableListOf<String>()
        if (!check(Token.RParen)) {
            do {
                params += expect<Token.Ident>().name
            } while (match(Token.Comma))
        }
        consume(Token.RParen)
        val body = parseBlock()
        return Stmt.FunDecl(name.name, params, body)
    }

    private fun parseExpression(): Expr = parseAssign()

    private fun parseAssign(): Expr {
        val expr = parseLogical()
        when (expr) {
            is Expr.Var -> {
                when {
                    match(Token.Eq) -> return Expr.Assign(expr.name, parseAssign())
                    match(Token.PlusEq) -> return Expr.Assign(expr.name, Expr.Binary(BinaryOp.ADD, expr, parseAssign()))
                    match(Token.MinusEq) -> return Expr.Assign(expr.name, Expr.Binary(BinaryOp.SUB, expr, parseAssign()))
                    match(Token.StarEq) -> return Expr.Assign(expr.name, Expr.Binary(BinaryOp.MUL, expr, parseAssign()))
                    match(Token.SlashEq) -> return Expr.Assign(expr.name, Expr.Binary(BinaryOp.DIV, expr, parseAssign()))
                }
            }
            is Expr.Index -> {
                when {
                    match(Token.Eq) -> return Expr.IndexAssign(expr.collection, expr.index, parseAssign())
                    match(Token.PlusEq) -> return Expr.IndexAssign(expr.collection, expr.index, Expr.Binary(BinaryOp.ADD, expr, parseAssign()))
                    match(Token.MinusEq) -> return Expr.IndexAssign(expr.collection, expr.index, Expr.Binary(BinaryOp.SUB, expr, parseAssign()))
                    match(Token.StarEq) -> return Expr.IndexAssign(expr.collection, expr.index, Expr.Binary(BinaryOp.MUL, expr, parseAssign()))
                    match(Token.SlashEq) -> return Expr.IndexAssign(expr.collection, expr.index, Expr.Binary(BinaryOp.DIV, expr, parseAssign()))
                }
            }
            else -> return expr
        }
		return expr
	}
    
    private fun parseLogical(): Expr {
        var left = parseEquality()
        while (match(Token.And, Token.Or)) {
            val op = when (previous()) {
                Token.And -> BinaryOp.AND
                Token.Or -> BinaryOp.OR
                else -> throw ParserError("unreachable")
            }
            val right = parseEquality()
            left = Expr.Binary(op, left, right)
        }
        return left
    }

    private fun parseEquality(): Expr {
        var left = parseComparison()
        while (match(Token.EqEq, Token.BangEq)) {
            val op = when (previous()) {
                Token.EqEq -> BinaryOp.EQ
                Token.BangEq -> BinaryOp.NEQ
                else -> throw ParserError("unreachable")
            }
            val right = parseComparison()
            left = Expr.Binary(op, left, right)
        }
        return left
    }

    private fun parseComparison(): Expr {
        var left = parseTerm()
        while (match(Token.Less, Token.Greater, Token.LessEq, Token.GreaterEq)) {
            val op = when (previous()) {
                Token.Less -> BinaryOp.LT
                Token.Greater -> BinaryOp.GT
                Token.LessEq -> BinaryOp.LTE
                Token.GreaterEq -> BinaryOp.GTE
                else -> throw ParserError("unreachable")
            }
            val right = parseTerm()
            left = Expr.Binary(op, left, right)
        }
        return left
    }

    private fun parseTerm(): Expr {
        var left = parseFactor()
        while (match(Token.Plus, Token.Minus)) {
            val op = when (previous()) {
                Token.Plus -> BinaryOp.ADD
                Token.Minus -> BinaryOp.SUB
                else -> throw ParserError("unreachable")
            }
            val right = parseFactor()
            left = Expr.Binary(op, left, right)
        }
        return left
    }

    private fun parseFactor(): Expr {
        var left = parseExpo()

        while (match(Token.Star, Token.Slash, Token.Percent)) {
            val op = when (previous()) {
                Token.Star -> BinaryOp.MUL
                Token.Slash -> BinaryOp.DIV
                Token.Percent -> BinaryOp.MOD
                else -> throw ParserError("unreachable")
            }
            val right = parseExpo()
            left = Expr.Binary(op, left, right)
        }
        return left
    }
    
    private fun parseExpo(): Expr {
        var left = parseUnary()
        while (match(Token.Caret)) {
            val right = parseUnary()
            left = Expr.Binary(BinaryOp.EXP, left, right)
        }
        return left
    }

    private fun parseUnary(): Expr {
        if (match(Token.Bang)) return Expr.Unary(UnaryOp.NOT, parseUnary())
        if (match(Token.Minus)) return Expr.Unary(UnaryOp.NEG, parseUnary())
        if (match(Token.PlusPlus)) return Expr.Binary(BinaryOp.ADD, parseUnary(), Expr.Num(1.0))
        if (match(Token.MinusMinus)) return Expr.Binary(BinaryOp.SUB, parseUnary(), Expr.Num(1.0))
        return parsePostfix()
    }
    
    private fun parsePostfix(): Expr {
        var expr = parseCall()
        while (match(Token.PlusPlus, Token.MinusMinus)) {
            val op = when (previous()) {
                Token.PlusPlus -> BinaryOp.ADD
                Token.MinusMinus -> BinaryOp.SUB
                else -> error("unreachable")
            }
            expr = Expr.Binary(op, expr, Expr.Num(1.0))
        }
        return expr
    }
    
    private fun parseCall(): Expr {
        var expr = parsePrimary()
        while (true) {
            when {
                match(Token.LParen) -> {
                    val args = mutableListOf<Expr>()
                    if (!check(Token.RParen)) {
                        do {
                            args += parseExpression()
                        } while (match(Token.Comma))
                    }
                    consume(Token.RParen)
                    expr = Expr.Call(expr, args)
                }
                
                match(Token.LBracket) -> {
                    val index = parseExpression()
                    consume(Token.RBracket)
                    expr = Expr.Index(expr, index)
                }
                
                match(Token.Dot) -> {
                    val key = expect<Token.Ident>()
                    expr = Expr.Index(expr, Expr.Str(key.name))
                }
                
                else -> break
            }
        }
        return expr
    }

    private fun parsePrimary(): Expr {
        return when (val currentToken = advance()) {
            is Token.Num -> Expr.Num(currentToken.value)
            is Token.Str -> Expr.Str(currentToken.value)
            is Token.Ident -> when (currentToken.name) {
                "true" -> Expr.Bool(true)
                "false" -> Expr.Bool(false)
                "nil" -> Expr.Nil
                else -> Expr.Var(currentToken.name)
            }

            Token.LParen -> {
                val expr = parseExpression()
                consume(Token.RParen)
                expr
            }

            else -> throw ParserError("Unknown token $currentToken")
        }
    }
}