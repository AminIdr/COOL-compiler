package lexer

import (
	"strings"
	"testing"
)

func TestNextToken(t *testing.T) {
	tests := []struct {
		input             string
		expectedTokenType []TokenType
		expectedLiteral   []string
	}{
		{
			"class Main {};",
			[]TokenType{CLASS, TYPEID, LBRACE, RBRACE, SEMI, EOF},
			[]string{"class", "Main", "{", "}", ";", ""},
		},
		{
			"x <- true;-- One line comment\nx <- false;",
			[]TokenType{OBJECTID, ASSIGN, BOOL_CONST, SEMI, OBJECTID, ASSIGN, BOOL_CONST, SEMI, EOF},
			[]string{"x", "<-", "true", ";", "x", "<-", "false", ";", ""},
		},
		{
			"_a <- 0; b   <- _a <= \"1\\n\";",
			[]TokenType{OBJECTID, ASSIGN, INT_CONST, SEMI, OBJECTID, ASSIGN, OBJECTID, LE, STR_CONST, SEMI, EOF},
			[]string{"_a", "<-", "0", ";", "b", "<-", "_a", "<=", "1\n", ";", ""},
		},
		{
			"{true\n1\n\"some string\"\n}",
			[]TokenType{LBRACE, BOOL_CONST, INT_CONST, STR_CONST, RBRACE, EOF},
			[]string{"{", "true", "1", "some string", "}", ""},
		},
		{
			"{true\n1\n\"some string\"}",
			[]TokenType{LBRACE, BOOL_CONST, INT_CONST, STR_CONST, RBRACE, EOF},
			[]string{"{", "true", "1", "some string", "}", ""},
		},
		{
			"let a:A in true",
			[]TokenType{LET, OBJECTID, COLON, TYPEID, IN, BOOL_CONST, EOF},
			[]string{"let", "a", ":", "A", "in", "true", ""},
		},
		{
			"case a of b:B => false esac",
			[]TokenType{CASE, OBJECTID, OF, OBJECTID, COLON, TYPEID, DARROW, BOOL_CONST, ESAC, EOF},
			[]string{"case", "a", "of", "b", ":", "B", "=>", "false", "esac", ""},
		},
		// Additional test cases:
		{
			"if x < 10 then x else y fi",
			[]TokenType{IF, OBJECTID, LT, INT_CONST, THEN, OBJECTID, ELSE, OBJECTID, FI, EOF},
			[]string{"if", "x", "<", "10", "then", "x", "else", "y", "fi", ""},
		},
		{
			"while i <= length loop i <- i + 1 pool",
			[]TokenType{WHILE, OBJECTID, LE, OBJECTID, LOOP, OBJECTID, ASSIGN, OBJECTID, PLUS, INT_CONST, POOL, EOF},
			[]string{"while", "i", "<=", "length", "loop", "i", "<-", "i", "+", "1", "pool", ""},
		},
		{
			"class Counter inherits IO { x : Int; }",
			[]TokenType{CLASS, TYPEID, INHERITS, TYPEID, LBRACE, OBJECTID, COLON, TYPEID, SEMI, RBRACE, EOF},
			[]string{"class", "Counter", "inherits", "IO", "{", "x", ":", "Int", ";", "}", ""},
		},
		{
			"(* This is a\nmultiline comment *) class",
			[]TokenType{CLASS, EOF},
			[]string{"class", ""},
		},
		{
			"(* Nested (* comment *) still works *) x",
			[]TokenType{OBJECTID, EOF},
			[]string{"x", ""},
		},
		{
			"new Object",
			[]TokenType{NEW, TYPEID, EOF},
			[]string{"new", "Object", ""},
		},
		{
			"isvoid object",
			[]TokenType{ISVOID, OBJECTID, EOF},
			[]string{"isvoid", "object", ""},
		},
		{
			"not flag",
			[]TokenType{NOT, OBJECTID, EOF},
			[]string{"not", "flag", ""},
		},
		{
			"var1 + var2 * var3 / var4 - var5",
			[]TokenType{OBJECTID, PLUS, OBJECTID, TIMES, OBJECTID, DIVIDE, OBJECTID, MINUS, OBJECTID, EOF},
			[]string{"var1", "+", "var2", "*", "var3", "/", "var4", "-", "var5", ""},
		},
		{
			"object@Type.method(arg1, arg2)",
			[]TokenType{OBJECTID, AT, TYPEID, DOT, OBJECTID, LPAREN, OBJECTID, COMMA, OBJECTID, RPAREN, EOF},
			[]string{"object", "@", "Type", ".", "method", "(", "arg1", ",", "arg2", ")", ""},
		},
		{
			"~num",
			[]TokenType{NEG, OBJECTID, EOF},
			[]string{"~", "num", ""},
		},
		{
			"\"String with escape sequences: \\t \\n \\b \\f \\\\ \\\"\"",
			[]TokenType{STR_CONST, EOF},
			[]string{"String with escape sequences: \t \n \b \f \\ \"", ""},
		},
		{
			"SELF_TYPE method()",
			[]TokenType{SELF_TYPE, OBJECTID, LPAREN, RPAREN, EOF},
			[]string{"SELF_TYPE", "method", "(", ")", ""},
		},
		{
			"-- Single line comment\nclass",
			[]TokenType{CLASS, EOF},
			[]string{"class", ""},
		},
		{
			"let x:Int <- 5, y:Bool <- true in x",
			[]TokenType{LET, OBJECTID, COLON, TYPEID, ASSIGN, INT_CONST, COMMA, OBJECTID, COLON, TYPEID, ASSIGN, BOOL_CONST, IN, OBJECTID, EOF},
			[]string{"let", "x", ":", "Int", "<-", "5", ",", "y", ":", "Bool", "<-", "true", "in", "x", ""},
		},
		{
			"x <- (y + 2) * 3",
			[]TokenType{OBJECTID, ASSIGN, LPAREN, OBJECTID, PLUS, INT_CONST, RPAREN, TIMES, INT_CONST, EOF},
			[]string{"x", "<-", "(", "y", "+", "2", ")", "*", "3", ""},
		},
		{
			"class Main { main():Int { 1 }; };",
			[]TokenType{CLASS, TYPEID, LBRACE, OBJECTID, LPAREN, RPAREN, COLON, TYPEID, LBRACE, INT_CONST, RBRACE, SEMI, RBRACE, SEMI, EOF},
			[]string{"class", "Main", "{", "main", "(", ")", ":", "Int", "{", "1", "}", ";", "}", ";", ""},
		},
		{
			"if (x = 5) then { x <- x + 1; } else { x <- x - 1; } fi",
			[]TokenType{IF, LPAREN, OBJECTID, EQ, INT_CONST, RPAREN, THEN, LBRACE, OBJECTID, ASSIGN, OBJECTID, PLUS, INT_CONST, SEMI, RBRACE, ELSE, LBRACE, OBJECTID, ASSIGN, OBJECTID, MINUS, INT_CONST, SEMI, RBRACE, FI, EOF},
			[]string{"if", "(", "x", "=", "5", ")", "then", "{", "x", "<-", "x", "+", "1", ";", "}", "else", "{", "x", "<-", "x", "-", "1", ";", "}", "fi", ""},
		},
		{
			"case expr of x : Int => x; y : String => y.length(); esac",
			[]TokenType{CASE, OBJECTID, OF, OBJECTID, COLON, TYPEID, DARROW, OBJECTID, SEMI, OBJECTID, COLON, TYPEID, DARROW, OBJECTID, DOT, OBJECTID, LPAREN, RPAREN, SEMI, ESAC, EOF},
			[]string{"case", "expr", "of", "x", ":", "Int", "=>", "x", ";", "y", ":", "String", "=>", "y", ".", "length", "(", ")", ";", "esac", ""},
		},
		{
			"{ x <- 10; while x < 0 loop { out_string(\"x = \"); out_int(x); x <- x - 1; } pool; }",
			[]TokenType{LBRACE, OBJECTID, ASSIGN, INT_CONST, SEMI, WHILE, OBJECTID, LT, INT_CONST, LOOP, LBRACE, OBJECTID, LPAREN, STR_CONST, RPAREN, SEMI, OBJECTID, LPAREN, OBJECTID, RPAREN, SEMI, OBJECTID, ASSIGN, OBJECTID, MINUS, INT_CONST, SEMI, RBRACE, POOL, SEMI, RBRACE, EOF},
			[]string{"{", "x", "<-", "10", ";", "while", "x", "<", "0", "loop", "{", "out_string", "(", "x = ", ")", ";", "out_int", "(", "x", ")", ";", "x", "<-", "x", "-", "1", ";", "}", "pool", ";", "}", ""},
		},
		// More complex string literals
		{
			"\"String with all escape sequences: \\t\\n\\b\\f\\\\\\\"\"",
			[]TokenType{STR_CONST, EOF},
			[]string{"String with all escape sequences: \t\n\b\f\\\"", ""},
		},
		{
			"\"String with Unicode: Привет мир!\"",
			[]TokenType{STR_CONST, EOF},
			[]string{"String with Unicode: Привет мир!", ""},
		},

		// Complex number combinations
		{
			"1234567890 0 42",
			[]TokenType{INT_CONST, INT_CONST, INT_CONST, EOF},
			[]string{"1234567890", "0", "42", ""},
		},

		// Identifiers and keywords with different cases
		{
			"class CLASS Class clASS",
			[]TokenType{CLASS, CLASS, CLASS, CLASS, EOF},
			[]string{"class", "CLASS", "Class", "clASS", ""},
		},
		{
			"if IF iF If",
			[]TokenType{IF, IF, IF, IF, EOF},
			[]string{"if", "IF", "iF", "If", ""},
		},
		{
			"true TRUE True tRuE false FALSE False",
			[]TokenType{BOOL_CONST, BOOL_CONST, BOOL_CONST, BOOL_CONST, BOOL_CONST, BOOL_CONST, BOOL_CONST, EOF},
			[]string{"true", "TRUE", "True", "tRuE", "false", "FALSE", "False", ""},
		},

		// Object identifiers
		{
			"variable _var var123 _123",
			[]TokenType{OBJECTID, OBJECTID, OBJECTID, OBJECTID, EOF},
			[]string{"variable", "_var", "var123", "_123", ""},
		},

		// Type identifiers
		{
			"MyClass YourClass MYCLASS My_Class",
			[]TokenType{TYPEID, TYPEID, TYPEID, TYPEID, EOF},
			[]string{"MyClass", "YourClass", "MYCLASS", "My_Class", ""},
		},

		// Nested comments
		{
			"(* comment (* nested comment *) still comment *) code",
			[]TokenType{OBJECTID, EOF},
			[]string{"code", ""},
		},
		{
			"(* (* (* deeply *) nested *) comment *) code",
			[]TokenType{OBJECTID, EOF},
			[]string{"code", ""},
		},

		// Operators with whitespace
		{
			"a + b",
			[]TokenType{OBJECTID, PLUS, OBJECTID, EOF},
			[]string{"a", "+", "b", ""},
		},
		{
			"a+b",
			[]TokenType{OBJECTID, PLUS, OBJECTID, EOF},
			[]string{"a", "+", "b", ""},
		},

		// Sequences of operators
		{
			"+-*/=<.",
			[]TokenType{PLUS, MINUS, TIMES, DIVIDE, EQ, LT, DOT, EOF},
			[]string{"+", "-", "*", "/", "=", "<", ".", ""},
		},

		// Complex method invocations
		{
			"object.method(arg1, arg2).another_method()",
			[]TokenType{OBJECTID, DOT, OBJECTID, LPAREN, OBJECTID, COMMA, OBJECTID, RPAREN, DOT, OBJECTID, LPAREN, RPAREN, EOF},
			[]string{"object", ".", "method", "(", "arg1", ",", "arg2", ")", ".", "another_method", "(", ")", ""},
		},

		// Mixed tokens in complex expressions
		{
			"if (x = 5) then out_string(\"x = 5\") else out_string(\"not equal\") fi",
			[]TokenType{IF, LPAREN, OBJECTID, EQ, INT_CONST, RPAREN, THEN, OBJECTID, LPAREN, STR_CONST, RPAREN, ELSE, OBJECTID, LPAREN, STR_CONST, RPAREN, FI, EOF},
			[]string{"if", "(", "x", "=", "5", ")", "then", "out_string", "(", "x = 5", ")", "else", "out_string", "(", "not equal", ")", "fi", ""},
		},

		// Comments followed by code on same line
		{
			"-- this is a comment\nclass Main {}; -- another comment",
			[]TokenType{CLASS, TYPEID, LBRACE, RBRACE, SEMI, EOF},
			[]string{"class", "Main", "{", "}", ";", ""},
		},

		// Multiple adjacent operators
		{
			"a<=b+c*d/e-f",
			[]TokenType{OBJECTID, LE, OBJECTID, PLUS, OBJECTID, TIMES, OBJECTID, DIVIDE, OBJECTID, MINUS, OBJECTID, EOF},
			[]string{"a", "<=", "b", "+", "c", "*", "d", "/", "e", "-", "f", ""},
		},

		// All COOL keyword test
		{
			"class inherits if then else fi while loop pool let in case of esac new isvoid not true false",
			[]TokenType{CLASS, INHERITS, IF, THEN, ELSE, FI, WHILE, LOOP, POOL, LET, IN, CASE, OF, ESAC, NEW, ISVOID, NOT, BOOL_CONST, BOOL_CONST, EOF},
			[]string{"class", "inherits", "if", "then", "else", "fi", "while", "loop", "pool", "let", "in", "case", "of", "esac", "new", "isvoid", "not", "true", "false", ""},
		},

		// SELF_TYPE test
		{
			"SELF_TYPE self_type Self_Type",
			[]TokenType{SELF_TYPE, SELF_TYPE, TYPEID, EOF},
			[]string{"SELF_TYPE", "self_type", "Self_Type", ""},
		},

		// Empty class definition
		{
			"class Empty { };",
			[]TokenType{CLASS, TYPEID, LBRACE, RBRACE, SEMI, EOF},
			[]string{"class", "Empty", "{", "}", ";", ""},
		},

		// Multi-line input
		{
			"class Main {\n  main(): Object {\n    out_string(\"Hello, World!\\n\")\n  };\n};",
			[]TokenType{CLASS, TYPEID, LBRACE, OBJECTID, LPAREN, RPAREN, COLON, TYPEID, LBRACE, OBJECTID, LPAREN, STR_CONST, RPAREN, RBRACE, SEMI, RBRACE, SEMI, EOF},
			[]string{"class", "Main", "{", "main", "(", ")", ":", "Object", "{", "out_string", "(", "Hello, World!\n", ")", "}", ";", "}", ";", ""},
		},

		// Combination of assignments
		{
			"x <- 5; y <- \"string\"; z <- true;",
			[]TokenType{OBJECTID, ASSIGN, INT_CONST, SEMI, OBJECTID, ASSIGN, STR_CONST, SEMI, OBJECTID, ASSIGN, BOOL_CONST, SEMI, EOF},
			[]string{"x", "<-", "5", ";", "y", "<-", "string", ";", "z", "<-", "true", ";", ""},
		},
	}

	for _, tt := range tests {
		l := NewLexer(strings.NewReader(tt.input))
		for i, expTType := range tt.expectedTokenType {
			tok := l.NextToken()

			if tok.Type != expTType {
				t.Fatalf("[%q]: Wrong token type %d-th Token. expected=%s, got %s", tt.input, i, expTType, tok.Type)
			}

			if tok.Literal != tt.expectedLiteral[i] {
				t.Fatalf("[%q]: Wrong literal at test %d-it Token. expected=%q, got %q", tt.input, i, tt.expectedLiteral[i], tok.Literal)
			}
		}
	}
}

func TestLineColumnTracking(t *testing.T) {
	input := `class Main {
    method() : Int {
        1
    };
};`

	expected := []struct {
		tokenType TokenType
		line      int
		column    int
	}{
		{CLASS, 1, 1},
		{TYPEID, 1, 7},
		{LBRACE, 1, 12},
		{OBJECTID, 2, 5},
		{LPAREN, 2, 11},
		{RPAREN, 2, 12},
		{COLON, 2, 14},
		{TYPEID, 2, 16},
		{LBRACE, 2, 20},
		{INT_CONST, 3, 9},
		{RBRACE, 4, 5},
		{SEMI, 4, 6},
		{RBRACE, 5, 1},
		{SEMI, 5, 2},
		{EOF, 5, 3},
	}

	l := NewLexer(strings.NewReader(input))

	for i, exp := range expected {
		tok := l.NextToken()

		if tok.Type != exp.tokenType {
			t.Fatalf("Test[%d] - Wrong token type. expected=%s, got=%s", i, exp.tokenType, tok.Type)
		}

		if tok.Line != exp.line {
			t.Fatalf("Test[%d] - Wrong line number. expected=%d, got=%d", i, exp.line, tok.Line)
		}

		if tok.Column != exp.column {
			t.Fatalf("Test[%d] - Wrong column number. expected=%d, got=%d", i, exp.column, tok.Column)
		}
	}
}

func TestErrorCases(t *testing.T) {
	tests := []struct {
		input           string
		expectedType    TokenType
		expectedMessage string
	}{
		{
			"\"Unterminated string\n",
			ERROR,
			"Unterminitaed string constant",
		},
		{
			"(* Unterminated comment",
			ERROR,
			"EOF in comment",
		},
		{
			"@#$%^", // Invalid characters
			ERROR,
			"Unexpected character",
		},
		{
			"\"String with invalid escape \\z\"",
			STR_CONST,
			"String with invalid escape z",
		},
	}

	for _, tt := range tests {
		l := NewLexer(strings.NewReader(tt.input))
		var tok Token

		// Keep reading tokens until we find an error or EOF
		for {
			tok = l.NextToken()
			if tok.Type == ERROR || (tok.Type == EOF && tt.expectedType == ERROR) {
				break
			}
			if tok.Type == tt.expectedType {
				break
			}
		}

		if tok.Type != tt.expectedType {
			t.Fatalf("[%q]: Expected token type %s, got %s", tt.input, tt.expectedType, tok.Type)
		}

		if tt.expectedType == ERROR && !strings.Contains(tok.Literal, tt.expectedMessage) {
			t.Fatalf("[%q]: Expected error message containing %q, got %q", tt.input, tt.expectedMessage, tok.Literal)
		}
	}
}

func TestComplexProgram(t *testing.T) {
	input := `class Main inherits IO {
    main() : Object {
        let count : Int <- 0,
            message : String <- "Hello, World!"
        in {
            out_string(message);
            while count < 5 loop {
                out_int(count);
                count <- count + 1;
            } pool;
        }
    };
};`

	expected := []TokenType{
		CLASS, TYPEID, INHERITS, TYPEID, LBRACE,
		OBJECTID, LPAREN, RPAREN, COLON, TYPEID, LBRACE,
		LET, OBJECTID, COLON, TYPEID, ASSIGN, INT_CONST, COMMA,
		OBJECTID, COLON, TYPEID, ASSIGN, STR_CONST,
		IN, LBRACE,
		OBJECTID, LPAREN, OBJECTID, RPAREN, SEMI,
		WHILE, OBJECTID, LT, INT_CONST, LOOP, LBRACE,
		OBJECTID, LPAREN, OBJECTID, RPAREN, SEMI,
		OBJECTID, ASSIGN, OBJECTID, PLUS, INT_CONST, SEMI,
		RBRACE, POOL, SEMI,
		RBRACE,
		RBRACE, SEMI,
		RBRACE, SEMI,
		EOF,
	}

	l := NewLexer(strings.NewReader(input))

	for i, expType := range expected {
		tok := l.NextToken()
		if tok.Type != expType {
			t.Fatalf("Test[%d] - Wrong token type. expected=%s, got=%s", i, expType, tok.Type)
		}
	}
}
