package semant

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
)

type SymbolTable struct {
	symbols map[string]*SymbolEntry
	parent  *SymbolTable
}

type SymbolEntry struct {
	Type     string
	Token    lexer.Token
	AttrType *ast.TypeIdentifier
	Method   *ast.Method
	Scope    *SymbolTable
}

func NewSymbolTable(parent *SymbolTable) *SymbolTable {
	return &SymbolTable{
		symbols: make(map[string]*SymbolEntry),
		parent:  parent,
	}
}

func (st *SymbolTable) AddEntry(name string, entry *SymbolEntry) {
	st.symbols[name] = entry
}

func (st *SymbolTable) Lookup(name string) (*SymbolEntry, bool) {
	entry, ok := st.symbols[name]
	if !ok && st.parent != nil {
		return st.parent.Lookup(name)
	}
	return entry, ok
}

type SemanticAnalyser struct {
	globalSymbolTable *SymbolTable
	errors            []string
	// IDR
	inheritanceGraph map[string]string // maps class name to parent class name
}

func NewSemanticAnalyser() *SemanticAnalyser {
	return &SemanticAnalyser{
		globalSymbolTable: NewSymbolTable(nil),
		errors:            []string{},
		// IDR
		inheritanceGraph: make(map[string]string),
	}
}

func (sa *SemanticAnalyser) Errors() []string {
	return sa.errors
}

func (sa *SemanticAnalyser) Analyze(program *ast.Program) {
	// Add Main class check before other analyses
	foundMain := false
	for _, class := range program.Classes {
		if class.Name.Value == "Main" {
			foundMain = true
			break
		}
	}

	if !foundMain {
		sa.errors = append(sa.errors, "Program does not contain a Main class")
		return // Early return since this is a critical error
	}

	sa.buildClassesSymboltables(program)
	// IDR
	sa.buildInheritanceGraph(program)
	// NOT IDR
	sa.buildSymboltables(program)
	sa.typeCheck(program)
}

func (sa *SemanticAnalyser) typeCheck(program *ast.Program) {
	for _, class := range program.Classes {
		st := sa.globalSymbolTable.symbols[class.Name.Value].Scope
		sa.typeCheckClass(class, st)
	}
}

func (sa *SemanticAnalyser) typeCheckClass(cls *ast.Class, st *SymbolTable) {
	for _, feature := range cls.Features {
		switch f := feature.(type) {
		case *ast.Attribute:
			sa.typeCheckAttribute(f, st)
		case *ast.Method:
			sa.typeCheckMethod(f, st)
		}
	}
}

func (sa *SemanticAnalyser) typeCheckAttribute(attribute *ast.Attribute, st *SymbolTable) {
	if attribute.Expression != nil {
		expressionType := sa.getExpressionType(attribute.Expression, st)
		if expressionType != attribute.TypeDecl.Value {
			sa.errors = append(sa.errors, fmt.Sprintf("attribute %s cannot be of type %s, expected %s", attribute.Name.Value, expressionType, attribute.TypeDecl.Value))
		}
	}

}

func (sa *SemanticAnalyser) typeCheckMethod(method *ast.Method, st *SymbolTable) {
	methodSt := st.symbols[method.Name.Value].Scope
	for _, formal := range method.Formals {
		if _, ok := methodSt.Lookup(formal.Name.Value); ok {
			sa.errors = append(sa.errors, fmt.Sprintf("argument %s in method %s is already defined",
				formal.Name.Value, method.Name.Value))
			continue
		}

		methodSt.parent.AddEntry(formal.Name.Value, &SymbolEntry{
			Token: formal.Token,
			Type:  formal.TypeDecl.Value,
		})
	}

	// Only check return type if method has an expression and declared type
	if method.Expression != nil && method.TypeDecl.Value != "" {
		methodExpressionType := sa.getExpressionType(method.Expression, methodSt)
		if !sa.isTypeConformant(methodExpressionType, method.TypeDecl.Value) {
			sa.errors = append(sa.errors, fmt.Sprintf("method %s is expected to return %s, found %s",
				method.Name.Value, method.TypeDecl.Value, methodExpressionType))
		}
	}
}
func (sa *SemanticAnalyser) isTypeConformant(type1, type2 string) bool {
	// TODO
	// A type always conforms to itself
	if type1 == type2 {
		return true
	}

	// Walk up the inheritance tree from type1
	current := type1
	for current != "Object" {
		parent, exists := sa.inheritanceGraph[current]
		if !exists {
			return false
		}
		if parent == type2 {
			return true
		}
		current = parent
	}

	// Special case: everything conforms to Object
	return type2 == "Object"
}

func (sa *SemanticAnalyser) getExpressionType(expression ast.Expression, st *SymbolTable) string {
	switch e := expression.(type) {
	case *ast.IntegerLiteral:
		return "Int"
	case *ast.StringLiteral:
		return "String"
	case *ast.BooleanLiteral:
		return "Bool"
	case *ast.BlockExpression:
		return sa.getBlockExpressionType(e, st)
	case *ast.IfExpression:
		return sa.getIfExpressionType(e, st)
	case *ast.WhileExpression:
		return sa.getWhileExpressionType(e, st)
	case *ast.NewExpression:
		return sa.GetNewExpressionType(e, st)
	case *ast.LetExpression:
		return sa.GetLetExpressionType(e, st)
	case *ast.Assignment:
		return sa.GetAssignmentExpressionType(e, st)
	case *ast.UnaryExpression:
		return sa.GetUnaryExpressionType(e, st)
	case *ast.BinaryExpression:
		return sa.GetBinaryExpressionType(e, st)
	case *ast.CaseExpression:
		return sa.GetCaseExpressionType(e, st)
	case *ast.IsVoidExpression:
		return "Bool"
	default:
		return "Object"
	}
}

func (sa *SemanticAnalyser) getWhileExpressionType(wexpr *ast.WhileExpression, st *SymbolTable) string {
	conditionType := sa.getExpressionType(wexpr.Condition, st)
	if conditionType != "Bool" {
		sa.errors = append(sa.errors, fmt.Sprintf("condition of if statement is of type %s, expected Bool", conditionType))
		return "Object"
	}

	return sa.getExpressionType(wexpr.Body, st)
}

func (sa *SemanticAnalyser) getBlockExpressionType(bexpr *ast.BlockExpression, st *SymbolTable) string {
	lastType := ""
	for _, expression := range bexpr.Expressions {
		lastType = sa.getExpressionType(expression, st)
	}

	return lastType
}

func (sa *SemanticAnalyser) getIfExpressionType(ifexpr *ast.IfExpression, st *SymbolTable) string {
	conditionType := sa.getExpressionType(ifexpr.Condition, st)
	if conditionType != "Bool" {
		sa.errors = append(sa.errors, fmt.Sprintf("condition of if statement is of type %s, expected Bool", conditionType))
		return "Object"
	}

	constype := sa.getExpressionType(ifexpr.Consequence, st)
	alttype := sa.getExpressionType(ifexpr.Alternative, st)

	if constype != alttype {
		sa.errors = append(sa.errors, fmt.Sprintf("ambiguous if statement return type %s vs %s", constype, alttype))
		return "Object"
	}

	return constype
}

func (sa *SemanticAnalyser) buildClassesSymboltables(program *ast.Program) {
	sa.globalSymbolTable.AddEntry("Object", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Object"}})
	sa.globalSymbolTable.AddEntry("Int", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Int"}})
	sa.globalSymbolTable.AddEntry("String", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "String"}})
	sa.globalSymbolTable.AddEntry("Bool", &SymbolEntry{Type: "Class", Token: lexer.Token{Literal: "Bool"}})

	for _, class := range program.Classes {
		if _, ok := sa.globalSymbolTable.Lookup(class.Name.Value); ok {
			sa.errors = append(sa.errors, fmt.Sprintf("class %s is already defined", class.Name.Value))
			continue
		}

		sa.globalSymbolTable.AddEntry(class.Name.Value, &SymbolEntry{Type: "Class", Token: class.Name.Token})
	}
}

func (sa *SemanticAnalyser) buildSymboltables(program *ast.Program) {
	for _, class := range program.Classes {
		classEntry, _ := sa.globalSymbolTable.Lookup(class.Name.Value)
		classEntry.Scope = NewSymbolTable(sa.globalSymbolTable)

		for _, feature := range class.Features {
			switch f := feature.(type) {
			case *ast.Attribute:
				if _, ok := classEntry.Scope.Lookup(f.Name.Value); ok {
					sa.errors = append(sa.errors, fmt.Sprintf("attribute %s is already defined in class %s", f.Name.Value, class.Name.Value))
					continue
				}
				// Fix: Use f.Type directly instead of taking its address
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, AttrType: f.Type})
			case *ast.Method:
				methodST := NewSymbolTable(classEntry.Scope)
				classEntry.Scope.AddEntry(f.Name.Value, &SymbolEntry{Token: f.Name.Token, Scope: methodST, Method: f})
			}
		}
	}
}

func (sa *SemanticAnalyser) GetNewExpressionType(ne *ast.NewExpression, st *SymbolTable) string {
	// TODO: handle SELF_TYPE when implemented
	if _, ok := sa.globalSymbolTable.Lookup(ne.Type.Value); !ok {
		sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in new expression", ne.Type.Value))
		return "Object"
	}
	return ne.Type.Value
}

func (sa *SemanticAnalyser) GetLetExpressionType(le *ast.LetExpression, st *SymbolTable) string {
	for _, binding := range le.Bindings {
		exprType := sa.getExpressionType(binding.Init, st)
		if exprType != binding.Type.Value {
			sa.errors = append(sa.errors, fmt.Sprintf("Let binding with wrong type %s, expected %s", exprType, binding.Type.Value))
		}
	}
	return sa.getExpressionType(le.In, st)
}

// func (sa *SemanticAnalyser) GetLetExpressionType(le *ast.LetExpression, st *SymbolTable) string {
//     // Create a new scope for let bindings
//     letScope := NewSymbolTable(st)

//     // Process each binding
//     for _, binding := range le.Bindings {
//         // Check if type exists
//         if _, ok := sa.globalSymbolTable.Lookup(binding.Type.Value); !ok {
//             sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in let binding",
//                 binding.Type.Value))
//             continue
//         }

//         // If there's an initialization expression
//         if binding.Init != nil {
//             initType := sa.getExpressionType(binding.Init, letScope)
//             if !sa.isTypeConformant(initType, binding.Type.Value) {
//                 sa.errors = append(sa.errors, fmt.Sprintf(
//                     "type mismatch in let binding: expected %s, got %s",
//                     binding.Type.Value, initType))
//             }
//         }

//         // Add binding to scope
//         letScope.AddEntry(binding.Identifier.Value, &SymbolEntry{
//             Type:  binding.Type.Value,
//             Token: binding.Identifier.Token,
//         })
//     }

//     // Type check the body expression in the new scope
//     return sa.getExpressionType(le.In, letScope)
// }

func (sa *SemanticAnalyser) GetAssignmentExpressionType(a *ast.Assignment, st *SymbolTable) string {
	// TODO: look for object in symbol table walking the scope and then check type
	return ""
}

func (sa *SemanticAnalyser) GetUnaryExpressionType(uexpr *ast.UnaryExpression, st *SymbolTable) string {
	rightType := sa.getExpressionType(uexpr.Right, st)
	switch uexpr.Operator {
	case "~":
		if rightType != "Int" {
			sa.errors = append(sa.errors, fmt.Sprintf("bitwise negation on non-Int type: %s", rightType))
		}
		return "Int"
	case "not":
		if rightType != "Bool" {
			sa.errors = append(sa.errors, fmt.Sprintf("logical negation on non-Bool type: %s", rightType))
		}
		return "Bool"
	default:
		sa.errors = append(sa.errors, fmt.Sprintf("unknown unary operator %s", uexpr.Operator))
		return "Object"
	}
}

func isComparable(t string) bool {
	return t == "Int" || t == "Bool" || t == "String"
}

func (sa *SemanticAnalyser) GetBinaryExpressionType(be *ast.BinaryExpression, st *SymbolTable) string {
	leftType := sa.getExpressionType(be.Left, st)
	rightType := sa.getExpressionType(be.Right, st) // Fix: Use be.Right instead of be.Left

	switch be.Operator {
	case "+", "*", "/", "-":
		if leftType != "Int" || rightType != "Int" {
			sa.errors = append(sa.errors, fmt.Sprintf("arithmetic operation on non-Int types: %s %s %s", leftType, be.Operator, rightType))
		}
		return "Int"
	case "<", "<=", "=":
		if leftType != rightType || !isComparable(leftType) {
			sa.errors = append(sa.errors, fmt.Sprintf("comparison between incompatible types: %s %s %s", leftType, be.Operator, rightType))
		}
		return "Bool"
	case "<-": // Add support for assignment operator
		if !sa.isTypeConformant(rightType, leftType) {
			sa.errors = append(sa.errors, fmt.Sprintf("cannot assign %s to %s", rightType, leftType))
		}
		return rightType
	default:
		sa.errors = append(sa.errors, fmt.Sprintf("unknown binary operator %s", be.Operator))
		return "Object"
	}
}

func (sa *SemanticAnalyser) GetCaseExpressionType(ce *ast.CaseExpression, st *SymbolTable) string {
	exprType := sa.getExpressionType(ce.Expression, st)
	if exprType == "" {
		return "Object"
	}

	if len(ce.Cases) == 0 {
		return "Object"
	}

	// Get type of first case's body as initial LCA candidate
	branchType := sa.getExpressionType(ce.Cases[0].Body, st)

	// For each subsequent case, find LCA of current type and case's body type
	for _, cse := range ce.Cases[1:] {
		// Create new scope for this branch
		branchSt := NewSymbolTable(st)

		// Since Case has Pattern and Body fields directly
		if cse.Pattern == nil {
			sa.errors = append(sa.errors, "missing pattern in case branch")
			continue
		}

		// Type check the pattern and body
		patternType := sa.getExpressionType(cse.Pattern, st)
		if patternType == "" {
			sa.errors = append(sa.errors, "invalid case branch pattern")
			continue
		}

		// Check if the pattern type exists in the global symbol table
		if _, ok := sa.globalSymbolTable.Lookup(patternType); !ok {
			sa.errors = append(sa.errors, fmt.Sprintf("undefined type %s in case pattern", patternType))
			continue
		}

		// Get this branch's body type
		currentBranchType := sa.getExpressionType(cse.Body, branchSt)

		// Find LCA of current running type and this branch's type
		branchType = sa.findLCA(branchType, currentBranchType)
	}

	return branchType
}

// IDR

func (sa *SemanticAnalyser) buildInheritanceGraph(program *ast.Program) {
	// All classes inherit from Object by default
	for _, class := range program.Classes {
		if class.Parent == nil {
			sa.inheritanceGraph[class.Name.Value] = "Object"
		} else {
			// Check if parent class exists
			if _, ok := sa.globalSymbolTable.Lookup(class.Parent.Value); !ok {
				sa.errors = append(sa.errors, fmt.Sprintf("class %s inherits from undefined class %s",
					class.Name.Value, class.Parent.Value))
				continue
			}

			// Check for inheritance cycles
			parent := class.Parent.Value
			current := parent
			for current != "Object" {
				if current == class.Name.Value {
					sa.errors = append(sa.errors, fmt.Sprintf("inheritance cycle detected for class %s",
						class.Name.Value))
					break
				}
				current = sa.inheritanceGraph[current]
			}

			sa.inheritanceGraph[class.Name.Value] = class.Parent.Value
		}
	}
}

// findLCA finds the least common ancestor type of two types in the inheritance hierarchy
func (sa *SemanticAnalyser) findLCA(type1, type2 string) string {
	// If types are the same, that's the LCA
	if type1 == type2 {
		return type1
	}

	// Get all ancestors of type1 (including type1)
	ancestors1 := make(map[string]bool)
	current := type1
	ancestors1[current] = true
	for current != "Object" {
		parent, exists := sa.inheritanceGraph[current]
		if !exists {
			return "Object"
		}
		ancestors1[parent] = true
		current = parent
	}

	// Walk up type2's ancestry until we find a common ancestor
	current = type2
	for {
		if ancestors1[current] {
			return current
		}
		if current == "Object" {
			return "Object"
		}
		parent, exists := sa.inheritanceGraph[current]
		if !exists {
			return "Object"
		}
		current = parent
	}
}
