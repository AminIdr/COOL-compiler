package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strconv"
)

type Parser struct {
	l         *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
	errors    []string

	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // myFunction(X)
	DOT         // obj.method
)

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.prefixParseFns = make(map[lexer.TokenType]prefixParseFn)
	p.infixParseFns = make(map[lexer.TokenType]infixParseFn)

	// Register prefix parsers
	p.registerPrefix(lexer.OBJECTID, p.parseIdentifierOrAssignment)
	p.registerPrefix(lexer.INT_CONST, p.parseIntegerLiteral)
	p.registerPrefix(lexer.STR_CONST, p.parseStringLiteral)
	p.registerPrefix(lexer.BOOL_CONST, p.parseBooleanLiteral)
	p.registerPrefix(lexer.IF, p.parseIfExpression)
	p.registerPrefix(lexer.WHILE, p.parseWhileExpression)
	p.registerPrefix(lexer.NEW, p.parseNewExpression)
	p.registerPrefix(lexer.ISVOID, p.parseIsVoidExpression)
	p.registerPrefix(lexer.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(lexer.NOT, p.parsePrefixExpression)
	// IDR
	p.registerPrefix(lexer.NEG, p.parsePrefixExpression)
	p.registerPrefix(lexer.LBRACE, p.parseBlockExpression)
	p.registerPrefix(lexer.LET, p.parseLetExpression)
	p.registerPrefix(lexer.CASE, p.parseCaseExpression)

	// Register infix parsers
	p.registerInfix(lexer.PLUS, p.parseInfixExpression)
	p.registerInfix(lexer.MINUS, p.parseInfixExpression)
	p.registerInfix(lexer.DIVIDE, p.parseInfixExpression)
	p.registerInfix(lexer.TIMES, p.parseInfixExpression)
	p.registerInfix(lexer.EQ, p.parseInfixExpression)
	p.registerInfix(lexer.LT, p.parseInfixExpression)
	p.registerInfix(lexer.LE, p.parseInfixExpression)

	// IDR
	p.registerInfix(lexer.LPAREN, p.parseCallExpression)
	p.registerInfix(lexer.AT, p.parseStaticDispatchExpression)
	p.registerInfix(lexer.DOT, p.parseDispatchExpression)

	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()

	// Debug
	// fmt.Printf("Token advance: current={%v:%v} at line %d col %d, peek={%v:%v}\n",
	// 	p.curToken.Type, p.curToken.Literal, p.curToken.Line, p.curToken.Column,
	// 	p.peekToken.Type, p.peekToken.Literal)
}

func (p *Parser) curTokenIs(t lexer.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t lexer.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectAndPeek(t lexer.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.peekError(t)
	return false
}

func (p *Parser) expectCurrent(t lexer.TokenType) bool {
	if p.curTokenIs(t) {
		p.nextToken()
		return true
	}
	p.currentError(t)
	return false
}

func (p *Parser) peekError(t lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("Expected next token to be %v, got %v line %d col %d", t, p.peekToken.Type, p.peekToken.Line, p.peekToken.Column))
}

func (p *Parser) currentError(t lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("Expected current token to be %v, got %v line %d col %d", t, p.curToken.Type, p.peekToken.Line, p.peekToken.Column))
}
func (p *Parser) ParseProgram() *ast.Program {
	prog := &ast.Program{}

	for !p.curTokenIs(lexer.EOF) {
		// fmt.Printf("Parsing class at token: {%v:%v}\n", p.curToken.Type, p.curToken.Literal)
		c := p.parseClass()
		if c == nil {
			// fmt.Printf("Failed to parse class\n")
			return prog
		}
		prog.Classes = append(prog.Classes, c)

		// After a class definition, expect a semicolon
		if !p.curTokenIs(lexer.SEMI) {
			p.currentError(lexer.SEMI)
			return prog
		}
		p.nextToken() // Move past SEMI
	}

	return prog
}

// Helper method
func (p *Parser) skipToNextClass() {
	for !p.curTokenIs(lexer.CLASS) && !p.curTokenIs(lexer.EOF) {
		p.nextToken()
	}
}

func (p *Parser) parseClass() *ast.Class {
	c := &ast.Class{Token: p.curToken}

	if !p.curTokenIs(lexer.CLASS) {
		p.currentError(lexer.CLASS)
		return nil
	}
	p.nextToken() // move past CLASS

	if !p.curTokenIs(lexer.TYPEID) {
		p.currentError(lexer.TYPEID)
		return nil
	}
	c.Name = &ast.TypeIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
	p.nextToken() // move past class name

	if p.curTokenIs(lexer.INHERITS) {
		p.nextToken() // move past INHERITS
		if !p.curTokenIs(lexer.TYPEID) {
			p.currentError(lexer.TYPEID)
			return nil
		}
		c.Parent = &ast.TypeIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}
		p.nextToken() // move past parent class name
	}

	if !p.curTokenIs(lexer.LBRACE) {
		p.currentError(lexer.LBRACE)
		return nil
	}
	p.nextToken() // move past LBRACE

	for !p.curTokenIs(lexer.RBRACE) && !p.curTokenIs(lexer.EOF) {
		feature := p.parseFeature()
		if feature == nil {
			return nil
		}
		c.Features = append(c.Features, feature)

		if !p.curTokenIs(lexer.SEMI) {
			p.currentError(lexer.SEMI)
			return nil
		}
		p.nextToken() // move past SEMI
	}

	if !p.curTokenIs(lexer.RBRACE) {
		p.currentError(lexer.RBRACE)
		return nil
	}
	p.nextToken() // move past RBRACE

	return c
}

func (p *Parser) parseFeature() ast.Feature {
	if p.peekTokenIs(lexer.LPAREN) {
		return p.parseMethod()
	}
	return p.parseAttribute()
}

func (p *Parser) parseMethod() *ast.Method {

	method := &ast.Method{
		Name: &ast.ObjectIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		},
	}

	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}

	if !p.peekTokenIs(lexer.RPAREN) {
		p.nextToken()
		method.Formals = append(method.Formals, p.parseFormals()...)
	}

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	if p.peekTokenIs(lexer.TYPEID) || p.peekTokenIs(lexer.SELF_TYPE) {
		p.nextToken() // Advance to the type token
	} else {
		p.peekError(lexer.TYPEID) // Report an error expecting TYPEID
		return nil
	}
	method.Type = &ast.TypeIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	p.nextToken() // move past LBRACE
	method.Expression = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

	p.nextToken()
	return method
}

func (p *Parser) parseFormal() *ast.Formal {
	formal := &ast.Formal{
		Name: &ast.ObjectIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		},
	}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}

	if !p.expectCurrent(lexer.COLON) {
		return nil
	}

	formal.Type = &ast.TypeIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
	if !p.curTokenIs(lexer.TYPEID) {
		p.currentError(lexer.TYPEID)
		return nil
	}

	return formal
}

func (p *Parser) parseFormals() []*ast.Formal {
	formals := []*ast.Formal{p.parseFormal()}
	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		formals = append(formals, p.parseFormal())
	}
	return formals
}

func (p *Parser) parseAttribute() *ast.Attribute {
	attribute := &ast.Attribute{
		Name: &ast.ObjectIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		},
	}
	if !p.expectCurrent(lexer.OBJECTID) {
		return nil
	}
	if !p.expectCurrent(lexer.COLON) {
		return nil
	}
	attribute.Type = &ast.TypeIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	if !p.curTokenIs(lexer.TYPEID) {
		p.currentError(lexer.TYPEID)
		return nil
	}

	// IDR
	p.nextToken()

	// Handle initialization if present
	if p.curTokenIs(lexer.ASSIGN) {
		p.nextToken()
		attribute.Init = p.parseExpression(LOWEST)
		p.nextToken()
	}
	return attribute
}

func (p *Parser) registerPrefix(tokenType lexer.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType lexer.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		// p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix() // This will call parseBlockExpression when curToken is LBRACE

	for !p.peekTokenIs(lexer.SEMI) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	num, err := strconv.ParseInt(p.curToken.Literal, 10, 64)
	if err != nil {
		return nil
	}
	return &ast.IntegerLiteral{Token: p.curToken, Value: num}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.BooleanLiteral{Token: p.curToken, Value: p.curToken.Literal == "true"}
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	exp := &ast.UnaryExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.nextToken()
	exp.Right = p.parseExpression(PREFIX)
	return exp
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	exp := &ast.BinaryExpression{
		Token:    p.curToken,
		Left:     left,
		Operator: p.curToken.Literal,
	}
	precedence := p.curPrecedence()
	p.nextToken()
	exp.Right = p.parseExpression(precedence)
	return exp
}

// func (p *Parser) parseIfExpression() ast.Expression {
// 	exp := &ast.IfExpression{Token: p.curToken}

// 	if !p.expectCurrent(lexer.IF) {
// 		return nil
// 	}

// 	exp.Condition = p.parseExpression(LOWEST)

// 	if !p.expectCurrent(lexer.THEN) {
// 		return nil
// 	}

// 	exp.Consequence = p.parseExpression(LOWEST)

// 	if !p.expectCurrent(lexer.ELSE) {
// 		return nil
// 	}

// 	exp.Alternative = p.parseExpression(LOWEST)

// 	if !p.expectCurrent(lexer.FI) {
// 		return nil
// 	}

// 	return exp
// }

// IDR
func (p *Parser) parseIfExpression() ast.Expression {
	exp := &ast.IfExpression{Token: p.curToken}

	p.nextToken() // Advance past IF

	exp.Condition = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.THEN) {
		return nil
	}

	p.nextToken() // Advance past THEN
	exp.Consequence = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.ELSE) {
		return nil
	}

	p.nextToken() // Advance past ELSE
	exp.Alternative = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.FI) {
		return nil
	}

	return exp
}

// func (p *Parser) parseWhileExpression() ast.Expression {
// 	exp := &ast.WhileExpression{Token: p.curToken}

// 	if !p.expectCurrent(lexer.WHILE) {
// 		return nil
// 	}

// 	exp.Condition = p.parseExpression(LOWEST)

// 	if !p.expectCurrent(lexer.LOOP) {
// 		return nil
// 	}

// 	exp.Body = p.parseExpression(LOWEST)

// 	if !p.expectCurrent(lexer.POOL) {
// 		return nil
// 	}

// 	return exp
// }

// IDR
func (p *Parser) parseWhileExpression() ast.Expression {
	exp := &ast.WhileExpression{Token: p.curToken}

	p.nextToken() // Advance past WHILE

	exp.Condition = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.LOOP) {
		return nil
	}

	p.nextToken() // Advance past LOOP
	exp.Body = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.POOL) {
		return nil
	}

	return exp
}

func (p *Parser) parseNewExpression() ast.Expression {
	exp := &ast.NewExpression{Token: p.curToken}

	if !p.expectCurrent(lexer.NEW) {
		return nil
	}

	exp.Type = &ast.TypeIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	return exp
}

func (p *Parser) parseIsVoidExpression() ast.Expression {
	exp := &ast.IsVoidExpression{Token: p.curToken}

	p.nextToken()

	exp.Expression = p.parseExpression(LOWEST)

	return exp
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

var precedences = map[lexer.TokenType]int{
	lexer.EQ:     EQUALS,
	lexer.LT:     LESSGREATER,
	lexer.LE:     LESSGREATER,
	lexer.PLUS:   SUM,
	lexer.MINUS:  SUM,
	lexer.DIVIDE: PRODUCT,
	lexer.TIMES:  PRODUCT,
	lexer.LPAREN: CALL,
	lexer.DOT:    DOT,
	lexer.AT:     DOT, // Same precedence as DOT since they're both dispatch operators
	lexer.ASSIGN: EQUALS,

	lexer.CASE: LOWEST,
	lexer.OF:   LOWEST,
}

func (p *Parser) parseBlockExpression() ast.Expression {
	block := &ast.BlockExpression{Token: p.curToken}
	block.Expressions = []ast.Expression{}

	p.nextToken() // move past LBRACE

	for !p.curTokenIs(lexer.RBRACE) && !p.curTokenIs(lexer.EOF) {
		// fmt.Println("Current Token is", p.curToken.Type)
		expr := p.parseExpression(LOWEST)
		if expr != nil {
			block.Expressions = append(block.Expressions, expr)
		}

		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}
		p.nextToken() // move past SEMI
	}
	if !p.curTokenIs(lexer.RBRACE) {
		p.currentError(lexer.RBRACE)
		return nil
	}
	return block
}

// Add the function call parser
func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{
		Token:     p.curToken,
		Function:  function,
		Arguments: []ast.Expression{},
	}

	// Move past '('
	p.nextToken()

	// Handle empty argument list
	if p.curTokenIs(lexer.RPAREN) {
		return exp
	}

	// Parse first argument
	exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))

	// Parse additional arguments
	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken() // move to comma
		p.nextToken() // move past comma
		exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))
	}

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) parseLetExpression() ast.Expression {
	exp := &ast.LetExpression{Token: p.curToken}
	exp.Bindings = []*ast.LetBinding{}

	for {
		p.nextToken() // move past 'let' or ','

		// Parse identifier
		if !p.curTokenIs(lexer.OBJECTID) {
			p.currentError(lexer.OBJECTID)
			return nil
		}

		binding := &ast.LetBinding{
			Identifier: &ast.ObjectIdentifier{
				Token: p.curToken,
				Value: p.curToken.Literal,
			},
		}

		if !p.expectAndPeek(lexer.COLON) {
			return nil
		}

		if !p.expectAndPeek(lexer.TYPEID) {
			return nil
		}

		binding.Type = &ast.TypeIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}

		// Check for initialization with <-
		if p.peekTokenIs(lexer.ASSIGN) {
			p.nextToken() // move to ASSIGN
			p.nextToken() // move past ASSIGN
			binding.Init = p.parseExpression(LOWEST)
		}

		exp.Bindings = append(exp.Bindings, binding)

		if !p.peekTokenIs(lexer.COMMA) {
			break
		}
		p.nextToken() // consume the comma
	}

	if !p.expectAndPeek(lexer.IN) {
		return nil
	}

	p.nextToken() // move past IN

	// Here's the main change - handle any expression after 'in'
	exp.In = p.parseExpression(LOWEST)

	// We don't need special handling for block expressions here
	// as they will be handled by parseExpression through parseBlockExpression

	return exp
}

func (p *Parser) parseIdentifierOrAssignment() ast.Expression {
	identifier := &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// If next token is not ASSIGN, just return the identifier
	if !p.peekTokenIs(lexer.ASSIGN) {
		return identifier
	}

	// Otherwise, create an assignment
	assignment := &ast.Assignment{
		Token: p.curToken,
		Name:  p.curToken.Literal,
	}

	// Skip over the ASSIGN token
	p.nextToken()
	p.nextToken()

	// Parse the expression being assigned
	assignment.Expression = p.parseExpression(LOWEST)

	return assignment
}

// Add new function for parsing static dispatch
func (p *Parser) parseDispatchExpression(object ast.Expression) ast.Expression {
	exp := &ast.DispatchExpression{
		Token:  p.curToken,
		Object: object,
	}

	// Expect method name (OBJECTID)
	if !p.expectAndPeek(lexer.OBJECTID) {
		return nil
	}

	exp.Method = &ast.ObjectIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// Expect opening parenthesis
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}

	// Move past '('
	p.nextToken()

	exp.Arguments = []ast.Expression{}

	// Handle empty argument list
	if p.curTokenIs(lexer.RPAREN) {
		return exp
	}

	// Parse first argument
	exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))

	// Parse additional arguments
	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken() // move to comma
		p.nextToken() // move past comma
		exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))
	}

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) parseCaseExpression() ast.Expression {
	expr := &ast.CaseExpression{
		Token: p.curToken,
	}

	p.nextToken() // move past 'case'

	// Parse the expression being cased on
	expr.Expression = p.parseExpression(LOWEST)

	// Check for OF token
	if !p.expectAndPeek(lexer.OF) {
		return nil
	}

	p.nextToken() // move past OF token

	// Parse case branches
	expr.Cases = []*ast.Case{}

	// We must have at least one branch
	for !p.curTokenIs(lexer.ESAC) && !p.curTokenIs(lexer.EOF) {
		caseNode := &ast.Case{}

		// Parse pattern (identifier:type)
		if !p.curTokenIs(lexer.OBJECTID) {
			p.currentError(lexer.OBJECTID)
			return nil
		}

		// Store the identifier
		caseNode.ObjectIdentifier = &ast.ObjectIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}

		// Parse colon
		if !p.expectAndPeek(lexer.COLON) {
			return nil
		}

		// Parse type
		if !p.expectAndPeek(lexer.TYPEID) {
			return nil
		}

		// Store the type identifier
		caseNode.TypeIdentifier = &ast.TypeIdentifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		}

		// Parse =>
		if !p.expectAndPeek(lexer.DARROW) {
			return nil
		}

		p.nextToken() // move past '=>'

		// Parse the body expression
		caseNode.Body = p.parseExpression(LOWEST)

		// Each branch must end with a semicolon
		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}

		expr.Cases = append(expr.Cases, caseNode)
		p.nextToken() // move past semicolon
	}

	if !p.curTokenIs(lexer.ESAC) {
		p.currentError(lexer.ESAC)
		return nil
	}
	// p.nextToken() // move past 'esac'

	return expr
}

func (p *Parser) parseStaticDispatchExpression(object ast.Expression) ast.Expression {
	exp := &ast.StaticDispatchExpression{
		Token:  p.curToken, // AT token
		Object: object,
	}

	// After @, expect a type name
	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}

	exp.StaticType = &ast.TypeIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// After type, expect a dot
	if !p.expectAndPeek(lexer.DOT) {
		return nil
	}

	// After dot, expect method name
	if !p.expectAndPeek(lexer.OBJECTID) {
		return nil
	}

	exp.Method = &ast.ObjectIdentifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// After method name, expect opening parenthesis
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}

	// Move past '('
	p.nextToken()

	exp.Arguments = []ast.Expression{}

	// Handle empty argument list
	if p.curTokenIs(lexer.RPAREN) {
		return exp
	}

	// Parse first argument
	exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))

	// Parse additional arguments
	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken() // move to comma
		p.nextToken() // move past comma
		exp.Arguments = append(exp.Arguments, p.parseExpression(LOWEST))
	}

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	return exp
}

// void + self + modules + linked list + string methods + type_name of int and string
//fct + valeur
//       0;  -- Default return value
// SELF_TYPE

// New store
// attribute <- 20
