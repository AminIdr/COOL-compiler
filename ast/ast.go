package ast

import (
	"cool-compiler/lexer"
	"fmt"
)

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Feature interface {
	Node
	featureNode()
}

type TypeIdentifier struct {
	Token lexer.Token
	Value string
}

func (ti *TypeIdentifier) TokenLiteral() string { return ti.Token.Literal }

type ObjectIdentifier struct {
	Token lexer.Token
	Value string
}

func (oi *ObjectIdentifier) TokenLiteral() string { return oi.Token.Literal }
func (oi *ObjectIdentifier) expressionNode()      {}

type Program struct {
	Classes []*Class
	// IDR
	Modules []*ModuleDeclaration
	Imports []*ImportStatement
}

func (p *Program) TokenLiteral() string { return "" }

type Class struct {
	Token    lexer.Token
	Name     *TypeIdentifier
	Parent   *TypeIdentifier
	Features []Feature
}

func (c *Class) TokenLiteral() string { return c.Token.Literal }

type Attribute struct {
	Token lexer.Token
	Name  *ObjectIdentifier
	Type  *TypeIdentifier
	// IDR semant
	Expression Expression
	TypeDecl   TypeIdentifier
}

func (a *Attribute) TokenLiteral() string { return a.Name.Value }
func (a *Attribute) featureNode()         {}

type Method struct {
	Name    *ObjectIdentifier
	Type    *TypeIdentifier
	Formals []*Formal
	// IDR semant
	Expression Expression
	TypeDecl   TypeIdentifier
}

func (m *Method) TokenLiteral() string { return m.Name.Value }
func (m *Method) featureNode()         {}

type Formal struct {
	// IDR semant
	Token lexer.Token

	Name *ObjectIdentifier
	Type *TypeIdentifier

	TypeDecl TypeIdentifier
}

func (f *Formal) TokenLiteral() string { return f.Name.Value }

// Implement Expression interface
func (f *Formal) expressionNode() {}

// IntegerLiteral represents an integer literal in the AST.
type IntegerLiteral struct {
	Token lexer.Token // The token representing the integer literal.
	Value int64       // The actual value of the integer literal.
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }

// StringLiteral represents a string literal in the AST.
type StringLiteral struct {
	Token lexer.Token // The token representing the string literal.
	Value string      // The actual value of the string literal.
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }

// BooleanLiteral represents a boolean literal in the AST.
type BooleanLiteral struct {
	Token lexer.Token // The token representing the boolean literal.
	Value bool        // The actual value of the boolean literal.
}

func (bl *BooleanLiteral) expressionNode()      {}
func (bl *BooleanLiteral) TokenLiteral() string { return bl.Token.Literal }

// UnaryExpression represents a unary operation in the AST.
type UnaryExpression struct {
	Token    lexer.Token // The operator token, e.g., 'not', '~', 'isvoid'.
	Operator string      // The operator as a string.
	Right    Expression  // The right-hand side expression.
}

func (ue *UnaryExpression) expressionNode()      {}
func (ue *UnaryExpression) TokenLiteral() string { return ue.Token.Literal }

// BinaryExpression represents a binary operation in the AST.
type BinaryExpression struct {
	Token    lexer.Token // The operator token, e.g., '+', '-', '*', '/'.
	Operator string      // The operator as a string.
	Left     Expression  // The left-hand side expression.
	Right    Expression  // The right-hand side expression.
}

func (be *BinaryExpression) expressionNode()      {}
func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }

// IfExpression represents an if-else expression in the AST.
type IfExpression struct {
	Token       lexer.Token // The 'if' token.
	Condition   Expression  // The condition expression.
	Consequence Expression  // The consequence expression (then branch).
	Alternative Expression  // The alternative expression (else branch).
}

func (ie *IfExpression) expressionNode()      {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }

// WhileExpression represents a while loop in the AST.
type WhileExpression struct {
	Token     lexer.Token // The 'while' token.
	Condition Expression  // The condition expression.
	Body      Expression  // The body expression.
}

func (we *WhileExpression) expressionNode()      {}
func (we *WhileExpression) TokenLiteral() string { return we.Token.Literal }

// BlockExpression represents a block of expressions in the AST.
type BlockExpression struct {
	Token       lexer.Token  // The '{' token.
	Expressions []Expression // The list of expressions within the block.
}

func (be *BlockExpression) expressionNode()      {}
func (be *BlockExpression) TokenLiteral() string { return be.Token.Literal }

// LetExpression represents a let expression in the AST.
type LetExpression struct {
	Token    lexer.Token   // The 'let' token.
	Bindings []*LetBinding // The list of bindings (variable declarations).
	In       Expression    // The expression that follows the bindings.
}

func (le *LetExpression) expressionNode()      {}
func (le *LetExpression) TokenLiteral() string { return le.Token.Literal }

// LetBinding represents a single binding in a let expression.
type LetBinding struct {
	Identifier *ObjectIdentifier // The identifier of the binding.
	Type       *TypeIdentifier   // The type of the binding.
	Init       Expression        // The initialization expression, if any.
}

// NewExpression represents the 'new' type expression in the AST.
type NewExpression struct {
	Token lexer.Token     // The 'new' token.
	Type  *TypeIdentifier // The type to be instantiated.
}

func (ne *NewExpression) expressionNode()      {}
func (ne *NewExpression) TokenLiteral() string { return ne.Token.Literal }

// IsVoidExpression represents an 'isvoid' expression in the AST.
type IsVoidExpression struct {
	Token      lexer.Token // The 'isvoid' token.
	Expression Expression  // The expression to check for being void.
}

func (ive *IsVoidExpression) expressionNode()      {}
func (ive *IsVoidExpression) TokenLiteral() string { return ive.Token.Literal }

// IDR

type CaseExpression struct {
	Token      lexer.Token
	Expression Expression
	Cases      []*Case
}

func (ce *CaseExpression) expressionNode()      {}
func (ce *CaseExpression) TokenLiteral() string { return ce.Token.Literal }

// CaseBranch represents a single branch in a case expression
type CaseBranch struct {
	Token lexer.Token       // The branch token
	Name  *ObjectIdentifier // The identifier for the matched value
	Type  *TypeIdentifier   // The type to match against
	Body  Expression        // The expression to evaluate if matched
}

func (cb *CaseBranch) TokenLiteral() string { return cb.Token.Literal }

type Case struct {
	ObjectIdentifier *ObjectIdentifier // The variable name
	TypeIdentifier   *TypeIdentifier   // The type to match
	Body             Expression        // The expression to evaluate if matched
}

type Binding struct {
	Token      lexer.Token
	Name       string
	Expression Expression
}

func (b *Binding) expressionNode()      {}
func (b *Binding) TokenLiteral() string { return b.Token.Literal }

type Assignment struct {
	Token      lexer.Token
	Name       string
	Expression Expression
}

func (a *Assignment) expressionNode()      {}
func (a *Assignment) TokenLiteral() string { return a.Token.Literal }

// Add this method to the Assignment struct
func (a *Assignment) String() string {
	return fmt.Sprintf("Assignment{Name: %s}", a.Name)
}

// CallExpression represents a method call in the AST
type CallExpression struct {
	Token     lexer.Token  // The '(' token
	Function  Expression   // The function being called (usually an ObjectIdentifier)
	Arguments []Expression // The arguments passed to the function
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }

// DispatchExpression represents a method dispatch in the AST
type DispatchExpression struct {
	Token     lexer.Token       // The '.' token
	Object    Expression        // The object on which the method is called
	Method    *ObjectIdentifier // The method being called
	Arguments []Expression      // The arguments passed to the method
}

func (de *DispatchExpression) expressionNode()      {}
func (de *DispatchExpression) TokenLiteral() string { return de.Token.Literal }

// StaticDispatchExpression represents a static method dispatch in the AST
type StaticDispatchExpression struct {
	Token      lexer.Token       // The '@' token
	Object     Expression        // The object on which the method is called
	StaticType *TypeIdentifier   // The static type specified after @
	Method     *ObjectIdentifier // The method being called
	Arguments  []Expression      // The arguments passed to the method
}

func (sde *StaticDispatchExpression) expressionNode()      {}
func (sde *StaticDispatchExpression) TokenLiteral() string { return sde.Token.Literal }

// ModuleDeclaration represents a module declaration
type ModuleDeclaration struct {
	Token lexer.Token     // The 'module' token
	Name  *TypeIdentifier // Module name
	Body  []*Class        // Classes defined in the module
}

func (md *ModuleDeclaration) TokenLiteral() string { return md.Token.Literal }

// ImportStatement represents an import statement
type ImportStatement struct {
	Token  lexer.Token     // The 'import' token
	Module *TypeIdentifier // The module being imported
}

func (is *ImportStatement) TokenLiteral() string { return is.Token.Literal }
