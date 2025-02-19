package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"strings"
	"testing"
)

func setupSemanticAnalyser() *SemanticAnalyser {
	sa := NewSemanticAnalyser()
	// Add basic types to global symbol table
	sa.buildClassesSymboltables(&ast.Program{})
	return sa
}

func TestGetExpressionType(t *testing.T) {
	tests := []struct {
		name          string
		expr          ast.Expression
		expectedType  string
		expectedError string
	}{
		{
			name:         "Integer Literal",
			expr:         &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "5"}, Value: 5},
			expectedType: "Int",
		},
		{
			name:         "String Literal",
			expr:         &ast.StringLiteral{Token: lexer.Token{Type: lexer.STR_CONST, Literal: "hello"}, Value: "hello"},
			expectedType: "String",
		},
		{
			name:         "Boolean Literal",
			expr:         &ast.BooleanLiteral{Token: lexer.Token{Type: lexer.BOOL_CONST, Literal: "true"}, Value: true},
			expectedType: "Bool",
		},
		{
			name: "Block Expression",
			expr: &ast.BlockExpression{
				Token: lexer.Token{Type: lexer.LBRACE, Literal: "{"},
				Expressions: []ast.Expression{
					&ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "1"}, Value: 1},
					&ast.StringLiteral{Token: lexer.Token{Type: lexer.STR_CONST, Literal: "hello"}, Value: "hello"},
				},
			},
			expectedType: "String",
		},
		{
			name: "If Expression Valid",
			expr: &ast.IfExpression{
				Token: lexer.Token{Type: lexer.IF, Literal: "if"},
				Condition: &ast.BooleanLiteral{
					Token: lexer.Token{Type: lexer.BOOL_CONST, Literal: "true"},
					Value: true,
				},
				Consequence: &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "1"}, Value: 1},
				Alternative: &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "2"}, Value: 2},
			},
			expectedType: "Int",
		},
		{
			name: "If Expression Invalid Condition",
			expr: &ast.IfExpression{
				Token: lexer.Token{Type: lexer.IF, Literal: "if"},
				Condition: &ast.IntegerLiteral{
					Token: lexer.Token{Type: lexer.INT_CONST, Literal: "1"},
					Value: 1,
				},
				Consequence: &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "1"}, Value: 1},
				Alternative: &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "2"}, Value: 2},
			},
			expectedType:  "Int",
			expectedError: "condition of if statement is of type Int, expected Bool",
		},
		{
			name: "Unary Expression Valid",
			expr: &ast.UnaryExpression{
				Token:    lexer.Token{Type: lexer.NEG, Literal: "~"},
				Operator: "~",
				Right:    &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "1"}, Value: 1},
			},
			expectedType: "Int",
		},
		{
			name: "Binary Expression Valid",
			expr: &ast.BinaryExpression{
				Token:    lexer.Token{Type: lexer.PLUS, Literal: "+"},
				Operator: "+",
				Left:     &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "1"}, Value: 1},
				Right:    &ast.IntegerLiteral{Token: lexer.Token{Type: lexer.INT_CONST, Literal: "2"}, Value: 2},
			},
			expectedType: "Int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			st := NewSymbolTable(nil)

			actualType := sa.getExpressionType(tt.expr, st)

			if actualType != tt.expectedType {
				t.Errorf("expected type %s, got %s", tt.expectedType, actualType)
			}

			if tt.expectedError != "" {
				found := false
				for _, err := range sa.errors {
					if strings.Contains(err, tt.expectedError) {
						found = true
						break
					}
				}
				if !found {
					t.Errorf("expected error containing %q, got %v", tt.expectedError, sa.errors)
				}
			} else if len(sa.errors) > 0 {
				t.Errorf("unexpected errors: %v", sa.errors)
			}
		})
	}
}

func TestMainClassRequirement(t *testing.T) {
	tests := []struct {
		name          string
		input         *ast.Program
		expectError   bool
		errorContains string
	}{
		{
			name: "No Main class",
			input: &ast.Program{
				Classes: []*ast.Class{
					{
						Name: &ast.TypeIdentifier{Value: "Sum"},
					},
				},
			},
			expectError:   true,
			errorContains: "Program does not contain a Main class",
		},
		{
			name: "Has Main class",
			input: &ast.Program{
				Classes: []*ast.Class{
					{
						Name: &ast.TypeIdentifier{Value: "Main"},
					},
					{
						Name: &ast.TypeIdentifier{Value: "Sum"},
					},
				},
			},
			expectError: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := NewSemanticAnalyser()
			sa.Analyze(tt.input)

			hasError := len(sa.Errors()) > 0
			if hasError != tt.expectError {
				t.Errorf("expected error: %v, got: %v", tt.expectError, hasError)
			}

			if tt.expectError {
				foundExpectedError := false
				for _, err := range sa.Errors() {
					if strings.Contains(err, tt.errorContains) {
						foundExpectedError = true
						break
					}
				}
				if !foundExpectedError {
					t.Errorf("expected error containing %q, got %v", tt.errorContains, sa.Errors())
				}
			}
		})
	}
}
