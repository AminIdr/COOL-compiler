package parser

import (
	"cool-compiler/ast"
	"strconv"
	"strings"
)

// SerializeExpression converts an AST expression into a string representation
func SerializeExpression(expr ast.Expression) string {
	if expr == nil {
		return ""
	}

	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		return strconv.FormatInt(e.Value, 10)
	case *ast.StringLiteral:
		return `"` + e.Value + `"`
	case *ast.BooleanLiteral:
		if e.Value {
			return "true"
		}
		return "false"
	case *ast.ObjectIdentifier:
		return e.Value
	case *ast.UnaryExpression:
		return "(" + e.Operator + " " + SerializeExpression(e.Right) + ")"
	case *ast.BinaryExpression:
		return "(" + SerializeExpression(e.Left) + " " + e.Operator + " " + SerializeExpression(e.Right) + ")"
	case *ast.IfExpression:
		return "if " + SerializeExpression(e.Condition) + " then " +
			SerializeExpression(e.Consequence) + " else " +
			SerializeExpression(e.Alternative) + " fi"
	case *ast.WhileExpression:
		return "while " + SerializeExpression(e.Condition) + " loop " +
			SerializeExpression(e.Body) + " pool"
	case *ast.BlockExpression:
		var result strings.Builder
		result.WriteString("{")
		for i, expr := range e.Expressions {
			if i > 0 {
				result.WriteString(" ")
			}
			result.WriteString(SerializeExpression(expr))
			result.WriteString(";")
		}
		result.WriteString("}")
		return result.String()
	case *ast.LetExpression:
		var result strings.Builder
		result.WriteString("let ")
		for i, binding := range e.Bindings {
			if i > 0 {
				result.WriteString(", ")
			}
			result.WriteString(binding.Identifier.Value)
			result.WriteString(":")
			result.WriteString(binding.Type.Value)
			if binding.Init != nil {
				result.WriteString(" <- ")
				result.WriteString(SerializeExpression(binding.Init))
			}
		}
		result.WriteString(" in ")
		result.WriteString(SerializeExpression(e.In))
		return result.String()
	case *ast.NewExpression:
		return "new " + e.Type.Value
	case *ast.IsVoidExpression:
		return "isvoid " + SerializeExpression(e.Expression)
	case *ast.Assignment:
		return "(" + e.Name + " <- " + SerializeExpression(e.Expression) + ")"
	case *ast.CaseExpression:
		var result strings.Builder
		result.WriteString("case ")
		result.WriteString(SerializeExpression(e.Expression))
		result.WriteString(" of")
		for _, c := range e.Cases {
			result.WriteString(" ")
			result.WriteString(c.ObjectIdentifier.Value)
			result.WriteString(":")
			result.WriteString(c.TypeIdentifier.Value)
			result.WriteString(" => ")
			result.WriteString(SerializeExpression(c.Body))
			result.WriteString(";")
		}
		result.WriteString(" esac")
		return result.String()
	case *ast.DispatchExpression:
		var result strings.Builder
		result.WriteString(SerializeExpression(e.Object))
		result.WriteString(".")
		result.WriteString(e.Method.Value)
		result.WriteString("(")
		for i, arg := range e.Arguments {
			if i > 0 {
				result.WriteString(", ")
			}
			result.WriteString(SerializeExpression(arg))
		}
		result.WriteString(")")
		return result.String()
	case *ast.StaticDispatchExpression:
		var result strings.Builder
		result.WriteString(SerializeExpression(e.Object))
		result.WriteString("@")
		result.WriteString(e.StaticType.Value)
		result.WriteString(".")
		result.WriteString(e.Method.Value)
		result.WriteString("(")
		for i, arg := range e.Arguments {
			if i > 0 {
				result.WriteString(", ")
			}
			result.WriteString(SerializeExpression(arg))
		}
		result.WriteString(")")
		return result.String()
	case *ast.CallExpression:
		var result strings.Builder
		result.WriteString(SerializeExpression(e.Function))
		result.WriteString("(")
		for i, arg := range e.Arguments {
			if i > 0 {
				result.WriteString(", ")
			}
			result.WriteString(SerializeExpression(arg))
		}
		result.WriteString(")")
		return result.String()
	default:
		return "unknown expression"
	}
}
