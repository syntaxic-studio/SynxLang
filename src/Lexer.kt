class LexerError(message: String): RuntimeException(message)

sealed class Token {
	data class Num(val value: Double): Token()
	data class Ident(val name: String): Token()
	data class Str(val value: String): Token()
	
	object Plus: Token() // +
	object Minus: Token() // -
	object Star: Token() // *
	object Slash: Token() // /
	object Caret: Token() // ^
	object Percent: Token() // %
	
	object PlusPlus: Token()
	object MinusMinus: Token()
	
	object Eq: Token() // =
	object PlusEq: Token() // +=
	object MinusEq: Token() // -=
	object StarEq: Token() // *=
	object SlashEq: Token() // /=-
	
	object Bang: Token() // !
	object EqEq: Token() // ==
	object BangEq: Token() // !=
	object Less: Token() // <
	object LessEq: Token() // <=
	object Greater: Token() // >
	object GreaterEq: Token() // >=
	
	object And: Token() // &&
	object Or: Token() // ||
	
	object LParen: Token() // (
	object RParen: Token() // )
	object LBracket: Token() // [
	object RBracket: Token() // ]
	object LBrace: Token() // {
	object RBrace: Token() // }
	
	object Comma: Token() // ,
	object Semicolon: Token() // ;
	object Dot: Token() // .
	
	object If: Token() // if
	object Else: Token() // else
	object While: Token() // while
	object For: Token() // for
	object Break: Token() // break
	object Continue: Token() // continue
	object Var: Token() // var
	object Return: Token() // return
	object Func: Token() // func
	
	object EOF: Token() // End Of File
}

class Lexer(val source: String) {
	private var pos = 0 // for the tokenizer
	
	private var line = 1
	private var col = 1 // for error handing
	
	private fun lexError(msg: String): Nothing = throw LexerError("line $line at column $col: $msg")
	
	private fun isAtEnd() = pos >= source.length
	private fun peek(): Char = if (isAtEnd()) '\u0000' else source[pos]
	
	private fun advance(): Char {
		val c = source[pos++]
		if (c == '\n') {
			line++; col = 1
		} else col++
		return c
	}
	
	private fun peekNext(): Char = if (pos + 1 >= source.length) '\u0000' else source[pos + 1]
	
	private fun scanNumber(first: Char): Token {
		val sb = StringBuilder(first.toString())
		while (!isAtEnd() && (peek().isDigit() || peek() == '.')) {
			sb.append(advance())
		}
		return Token.Num(sb.toString().toDouble())
	}
	
	private fun scanIdentOrKeyword(first: Char): Token {
		val sb = StringBuilder(first.toString())
		while (!isAtEnd() && (peek().isLetterOrDigit() || peek() == '_')) {
			sb.append(advance())
		}
		return when (sb.toString()) {
			"if" -> Token.If
			"else" -> Token.Else
			"var" -> Token.Var
			"while" -> Token.While
			"for" -> Token.For
			"break" -> Token.Break
			"continue" -> Token.Continue
			"return" -> Token.Return
			"function" -> Token.Func
			else -> Token.Ident(sb.toString())
		}
	}
	
	private fun scanString(): Token {
		val sb = StringBuilder()
		
		while (peek() != '"') {
			if (isAtEnd() || peek() == '\n') lexError("Unterminated string")
			
			val char = advance()
			
			if (char == '\\') {
				val escaped = when (val nextChar = advance()) {
					'n'  -> '\n'
					't'  -> '\t'
					'r'  -> '\r'
					'b'  -> '\b'
					'f'  -> '\u000C'
					'\\' -> '\\'
					'"'  -> '"'
					'\'' -> '\''
					'$'  -> '$'
					'u'  -> {
						val hex = buildString {
							repeat(4) { append(advance()) }
						}
						hex.toInt(16).toChar()
					}
					else -> lexError("Invalid escape sequence: \\$nextChar")
				}
				sb.append(escaped)
			} else {
				sb.append(char)
			}
		}
		
		advance() // closing quote
		return Token.Str(sb.toString())
	}
	
	private fun skipWhitespace() {
		while (!isAtEnd()) {
			when {
				peek().isWhitespace() -> advance()
				peek() == '/' && peekNext() == '/' -> {
					while (!isAtEnd() && peek() != '\n') advance()
				}
				else -> return
			}
		}
	}
	
	private fun nextToken(): Token {
		return when (val c = advance()) {
			'+' -> when (peek()) {
				'+' -> { advance(); Token.PlusPlus }
				'=' -> {advance(); Token.PlusEq}
				else -> Token.Plus
			}
			
			'-' -> when (peek()) {
				'*' -> { advance(); Token.MinusMinus }
				'=' -> {advance(); Token.MinusEq}
				else -> Token.Minus
			}
			'*' -> when {
				peek() == '=' -> {
					advance(); Token.StarEq
				}
				else -> Token.Star
			}
			
			'/' -> when {
				peek() == '=' -> {
					advance(); Token.SlashEq
				}
				else -> Token.Slash
			}
			'^' -> Token.Caret
			'=' -> when {
				peek() == '=' -> {
					advance(); Token.EqEq
				}
				else -> Token.Eq
			}
			
			'!' -> when {
				peek() == '=' -> {
					advance(); Token.BangEq
				}
				else -> Token.Bang
			}
			
			'<' -> when {
				peek() == '=' -> {
					advance(); Token.LessEq
				}
				else -> Token.Less
			}
			
			'>' -> when {
				peek() == '=' -> {
					advance(); Token.GreaterEq
				}
				else -> Token.Greater
			}
			
			'|' -> when {
				peek() == '|' -> {
					advance(); Token.Or
				}
				else -> lexError("Did you misspell || ?")
			}
			
			'&' -> when {
				peek() == '&' ->{
					advance(); Token.And
				}
				else -> lexError("Did you misspell && ?")
			}
			
			'%' -> Token.Percent
			'(' -> Token.LParen
			')' -> Token.RParen
			'{' -> Token.LBrace
			'}' -> Token.RBrace
			'[' -> Token.LBracket
			']' -> Token.RBracket
			',' -> Token.Comma
			';' -> Token.Semicolon
			'.' -> Token.Dot
			'"' -> scanString()
			
			else -> when {
				c.isDigit() -> scanNumber(c)
				c.isLetter() || c == '_' -> scanIdentOrKeyword(c)
				else -> lexError("unexpected character '$c'")
			}
		}
	}
	
	fun tokenize(): List<Token> {
		val tokens = mutableListOf<Token>()
		
		while (!isAtEnd()) {
			skipWhitespace()
			if (isAtEnd()) break
			tokens.add(nextToken())
		}
		tokens.add(Token.EOF)
		
		return tokens
	}
}