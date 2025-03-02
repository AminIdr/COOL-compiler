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

func TestTypeConformance(t *testing.T) {
	tests := []struct {
		name     string
		type1    string
		type2    string
		expected bool
	}{
		{"Same type", "Int", "Int", true},
		{"Base Object", "Int", "Object", true},
		{"Invalid inheritance", "Object", "Int", false},
	}

	sa := setupSemanticAnalyser()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := sa.isTypeConformant(tt.type1, tt.type2)
			if result != tt.expected {
				t.Errorf("isTypeConformant(%s, %s) = %v; want %v",
					tt.type1, tt.type2, result, tt.expected)
			}
		})
	}
}

func TestLetExpressionTypeChecking(t *testing.T) {
	tests := []struct {
		name          string
		letExpr       *ast.LetExpression
		expectedType  string
		expectedError string
	}{
		{
			name: "Valid Let with Int binding",
			letExpr: &ast.LetExpression{
				Token: lexer.Token{Type: lexer.LET, Literal: "let"},
				Bindings: []*ast.LetBinding{
					{
						Identifier: &ast.ObjectIdentifier{Value: "x"},
						Type:       &ast.TypeIdentifier{Value: "Int"},
						Init:       &ast.IntegerLiteral{Value: 42},
					},
				},
				In: &ast.IntegerLiteral{Value: 42},
			},
			expectedType: "Int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			st := NewSymbolTable(nil)

			actualType := sa.GetLetExpressionType(tt.letExpr, st)

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
			}
		})
	}
}

func TestCaseExpressionTypeChecking(t *testing.T) {
	tests := []struct {
		name          string
		caseExpr      *ast.CaseExpression
		expectedType  string
		expectedError string
	}{
		{
			name: "Valid case expression",
			caseExpr: &ast.CaseExpression{
				Expression: &ast.IntegerLiteral{Value: 1},
				Cases: []*ast.Case{
					{
						ObjectIdentifier: &ast.ObjectIdentifier{Value: "x"},
						TypeIdentifier:   &ast.TypeIdentifier{Value: "Int"},
						Body:             &ast.IntegerLiteral{Value: 42},
					},
					{
						ObjectIdentifier: &ast.ObjectIdentifier{Value: "y"},
						TypeIdentifier:   &ast.TypeIdentifier{Value: "Object"},
						Body:             &ast.IntegerLiteral{Value: 0},
					},
				},
			},
			expectedType: "Int",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			st := NewSymbolTable(nil)

			actualType := sa.GetCaseExpressionType(tt.caseExpr, st)

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
			}
		})
	}
}

func TestInheritanceGraphBuilding(t *testing.T) {
	tests := []struct {
		name          string
		program       *ast.Program
		expectedError string
	}{
		{
			name: "Valid inheritance",
			program: &ast.Program{
				Classes: []*ast.Class{
					{
						Name:   &ast.TypeIdentifier{Value: "Main"},
						Parent: &ast.TypeIdentifier{Value: "IO"},
					},
					{
						Name:   &ast.TypeIdentifier{Value: "A"},
						Parent: &ast.TypeIdentifier{Value: "Object"},
					},
				},
			},
		},
		{
			name: "Invalid inheritance from Int",
			program: &ast.Program{
				Classes: []*ast.Class{
					{
						Name:   &ast.TypeIdentifier{Value: "Main"},
						Parent: &ast.TypeIdentifier{Value: "Int"},
					},
				},
			},
			expectedError: "cannot inherit from Int",
		},
		{
			name: "Inheritance cycle",
			program: &ast.Program{
				Classes: []*ast.Class{
					{
						Name:   &ast.TypeIdentifier{Value: "A"},
						Parent: &ast.TypeIdentifier{Value: "B"},
					},
					{
						Name:   &ast.TypeIdentifier{Value: "B"},
						Parent: &ast.TypeIdentifier{Value: "A"},
					},
				},
			},
			expectedError: "inheritance cycle detected",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			sa.buildInheritanceGraph(tt.program)

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

func TestStaticDispatchTypeChecking(t *testing.T) {
	tests := []struct {
		name          string
		dispatch      *ast.StaticDispatchExpression
		expectedType  string
		expectedError string
	}{
		{
			name: "Valid static dispatch",
			dispatch: &ast.StaticDispatchExpression{
				Object:     &ast.ObjectIdentifier{Value: "self"},
				StaticType: &ast.TypeIdentifier{Value: "IO"},
				Method:     &ast.ObjectIdentifier{Value: "out_string"},
				Arguments: []ast.Expression{
					&ast.StringLiteral{Value: "Hello"},
				},
			},
			expectedType: "Object",
		},
		{
			name: "Invalid static type",
			dispatch: &ast.StaticDispatchExpression{
				Object:     &ast.ObjectIdentifier{Value: "self"},
				StaticType: &ast.TypeIdentifier{Value: "NonExistentType"},
				Method:     &ast.ObjectIdentifier{Value: "someMethod"},
				Arguments: []ast.Expression{
					&ast.IntegerLiteral{Value: 42},
				},
			},
			expectedType:  "Object",
			expectedError: "undefined type",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			st := NewSymbolTable(nil)

			actualType := sa.GetStaticDispatchExpressionType(tt.dispatch, st)

			if actualType != tt.expectedType {
				t.Errorf("expected type %s, got %s", tt.expectedType, actualType)
			}

			if tt.expectedError != "" && !containsError(sa.errors, tt.expectedError) {
				t.Errorf("expected error containing %q, got %v", tt.expectedError, sa.errors)
			}
		})
	}
}

func TestAssignmentTypeChecking(t *testing.T) {
	tests := []struct {
		name          string
		assignment    *ast.Assignment
		expectedType  string
		expectedError string
	}{
		{
			name: "Valid assignment",
			assignment: &ast.Assignment{
				Name: "x",
				Expression: &ast.IntegerLiteral{
					Token: lexer.Token{Type: lexer.INT_CONST, Literal: "42"},
					Value: 42,
				},
			},
			expectedType: "Int",
		},
		{
			name: "Invalid identifier",
			assignment: &ast.Assignment{
				Name: "undefined_var",
				Expression: &ast.StringLiteral{
					Token: lexer.Token{Type: lexer.STR_CONST, Literal: "hello"},
					Value: "hello",
				},
			},
			expectedType:  "String",
			expectedError: "undefined identifier",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			st := NewSymbolTable(nil)

			actualType := sa.GetAssignmentExpressionType(tt.assignment, st)

			if actualType != tt.expectedType {
				t.Errorf("expected type %s, got %s", tt.expectedType, actualType)
			}

			if tt.expectedError != "" && !containsError(sa.errors, tt.expectedError) {
				t.Errorf("expected error containing %q, got %v", tt.expectedError, sa.errors)
			}
		})
	}
}

func TestBinaryExpressionTypeChecking(t *testing.T) {
	tests := []struct {
		name          string
		expr          *ast.BinaryExpression
		expectedType  string
		expectedError string
	}{
		{
			name: "Valid arithmetic",
			expr: &ast.BinaryExpression{
				Token:    lexer.Token{Type: lexer.PLUS, Literal: "+"},
				Operator: "+",
				Left:     &ast.IntegerLiteral{Value: 1},
				Right:    &ast.IntegerLiteral{Value: 2},
			},
			expectedType: "Int",
		},
		{
			name: "Valid comparison",
			expr: &ast.BinaryExpression{
				Token:    lexer.Token{Type: lexer.LT, Literal: "<"},
				Operator: "<",
				Left:     &ast.IntegerLiteral{Value: 1},
				Right:    &ast.IntegerLiteral{Value: 2},
			},
			expectedType: "Bool",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sa := setupSemanticAnalyser()
			st := NewSymbolTable(nil)

			actualType := sa.GetBinaryExpressionType(tt.expr, st)

			if actualType != tt.expectedType {
				t.Errorf("expected type %s, got %s", tt.expectedType, actualType)
			}

			if tt.expectedError != "" && !containsError(sa.errors, tt.expectedError) {
				t.Errorf("expected error containing %q, got %v", tt.expectedError, sa.errors)
			}
		})
	}
}

// Helper function to check if errors contain a specific string
func containsError(errors []string, target string) bool {
	for _, err := range errors {
		if strings.Contains(err, target) {
			return true
		}
	}
	return false
}
